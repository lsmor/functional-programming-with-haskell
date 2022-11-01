{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

import           Data.Monoid                   (mappend)
import           Data.List                     (sortBy)
import           Data.Ord                      (comparing)
import qualified Hakyll
import           Hakyll ((.&&.), (.||.))
import           Control.Monad                 (liftM, forM_)
import           System.FilePath               (takeBaseName)
import           Data.Function (on)
import qualified Data.Map.Strict               as Map

{-
 I don't know how Hakyll works and I had a long time tweaking the original Heuna's code to fit my workflow
 It is very unstable code. For example, uncommenting line 122 causes an error compiling 404.md, which is 
 completely unrelated (line 32).
 
 Also matching "page/*" makes an infinite loop unless we exclude Home.markdown. 

 Also some rutes has to be manually relativize, which can cause broken links in the future if folder structure
 changes

 For sure I am doing something wrong but I don't know what. If you are touching this code, be sure you know what you are doing.
-}
main :: IO ()
main = Hakyll.hakyllWith  Hakyll.defaultConfiguration{ Hakyll.destinationDirectory = "docs"} $ do
    -- Build images and javascript under route _site/images and _site/js
    Hakyll.match ("images/*" .||. "js/*") $ do
        Hakyll.route   Hakyll.idRoute
        Hakyll.compile Hakyll.copyFileCompiler

    -- Build compressed css under _site/css
    Hakyll.match "css/*" $ do
        Hakyll.route   Hakyll.idRoute
        Hakyll.compile Hakyll.compressCssCompiler

    -- Build error page "error/404.md" as 404.html
    -- using the default template
    Hakyll.match "error/*" $ do
        Hakyll.route (Hakyll.gsubRoute "error/" (const "") `Hakyll.composeRoutes` Hakyll.setExtension "html")
        Hakyll.compile $ Hakyll.pandocCompiler
            >>= Hakyll.applyAsTemplate siteCtx
            >>= embedOnDefaultTemplate

    -- build all markdowns under pages as html files.
    -- It creates the body using the page.html template, then
    -- apply the body to default.html
    Hakyll.match ("pages/*" .&&. Hakyll.complement "pages/Home.markdown") $ do
        Hakyll.route $ Hakyll.setExtension "html"
        Hakyll.compile $ do
            pageName <- takeBaseName . Hakyll.toFilePath <$> Hakyll.getUnderlying
            let pageCtx = Hakyll.constField pageName "" <> baseNodeCtx
            let evalCtx = Hakyll.functionField "get-meta" getMetadataKey
                       <> Hakyll.functionField "eval" (evalCtxKey pageCtx)
            let activeSidebarCtx = sidebarCtx (evalCtx <> pageCtx)

            Hakyll.pandocCompiler
                >>= Hakyll.saveSnapshot "page-content"
                >>= Hakyll.loadAndApplyTemplate "templates/page.html"    siteCtx
                >>= Hakyll.loadAndApplyTemplate "templates/default.html" (activeSidebarCtx <> siteCtx)
                >>= Hakyll.relativizeUrls

    -- Don't really know why or how this works... Just matching the types
    -- We use Hakyll.getMatches to get the list of chapters (I don't know if it reads from the folder, or from in-memory store)
    -- We build the two maps {current_file -> next_file}  and {current_file -> previous_file}
    -- We use those maps to create contexts with the next/prev file in scope to we can have a "Next/Previous Chapter" button
    -- The maps are just identifiers, we need to query the compiler with Hakyll.getRoute in order to get the 
    -- destination url of the identifier. 
    -- 
    -- For some reason the urls must be relativize manually despite of relativizeUrls
    -- being called afterwards
    Hakyll.match "chapters/*" $ do
        Hakyll.route $ Hakyll.setExtension "html"
        chapter_identifiers <- Hakyll.getMatches "chapters/*"
        let next_chapter_map =
             case chapter_identifiers of
               [] -> Map.empty
               iden : idens -> Map.fromList (zip chapter_identifiers idens)
        let prev_chapter_map =
             case chapter_identifiers of
               [] -> Map.empty
               iden : idens -> Map.fromList (zip idens chapter_identifiers)

        Hakyll.compile $ do
            current_iden <- Hakyll.getUnderlying
            let next_chapter_iden = Map.lookup current_iden next_chapter_map
            let prev_chapter_iden = Map.lookup current_iden prev_chapter_map
            -- Build a context with next-chapter pointing to the url where the next chapter will be
            ctx_next <-
                case next_chapter_iden of
                    Nothing   -> pure mempty
                    Just iden -> do
                      iden_route <- Hakyll.getRoute iden
                      case iden_route of
                        Nothing -> pure mempty
                        Just s -> do
                            pure $ Hakyll.constField "next-chapter" (Hakyll.toSiteRoot s <> Hakyll.toUrl s) -- manually use relative url
            -- Build a context with prev-chapter pointing to the url where the prev chapter is
            ctx_prev <-
                case prev_chapter_iden of
                    Nothing   -> pure mempty
                    Just iden -> do
                      iden_route <- Hakyll.getRoute iden
                      case iden_route of
                        Nothing -> pure mempty
                        Just s -> do
                            pure $ Hakyll.constField "prev-chapter" (Hakyll.toSiteRoot s <> Hakyll.toUrl s)

            Hakyll.pandocCompiler
                >>= Hakyll.saveSnapshot "content"
                >>= Hakyll.loadAndApplyTemplate "templates/chapter.html" (chapterCtx <> ctx_next <> ctx_prev)
                >>= embedOnDefaultTemplate
                >>= Hakyll.relativizeUrls

    -- Build the book index
    Hakyll.create ["book-index.html"] $ do
        Hakyll.route Hakyll.idRoute
        Hakyll.compile $ do
            chapters <- Hakyll.loadAllSnapshots ("chapters/*" .&&. Hakyll.hasNoVersion) "content"
            let bookIndexCtx =
                    Hakyll.listField "chapters" chapterCtx (return chapters) <> siteCtx

            Hakyll.makeItem ""
                >>= Hakyll.loadAndApplyTemplate "templates/book-index.html" bookIndexCtx
                >>= embedOnDefaultTemplate
                >>= Hakyll.relativizeUrls

    -- Adding this line cause error to fail???? 
    -- Hakyll.match "pages/Home.markdown" do
    --     Hakyll.compile Hakyll.pandocCompiler


    -- Create Home Page
    Hakyll.match "pages/Home*" $ do -- using match pages/Home* goes into infinite loop.
        Hakyll.route $ Hakyll.constRoute "index.html"
        Hakyll.compile $
            Hakyll.pandocCompiler
                >>= Hakyll.loadAndApplyTemplate "templates/index.html" (siteCtx <> Hakyll.constField "home" "")
                >>= embedOnDefaultTemplate
                >>= Hakyll.relativizeUrls

    Hakyll.match "templates/*" $ Hakyll.compile Hakyll.templateBodyCompiler

--------------------------------------------------------------------------------

-- | This is the context of the whole site.
siteCtx :: Hakyll.Context String
siteCtx =
    baseCtx `mappend`
    Hakyll.constField "site_description" "Lanyon Theme on Hakyll" `mappend`
    Hakyll.constField "site-url" "https://github.com/hahey/lanyon-hakyll" `mappend`
    Hakyll.constField "tagline" "A Fork of Lanyon based on Poole" `mappend`
    Hakyll.constField "site-title" "lanyon-hakyll" `mappend`
    Hakyll.constField "copy-year" "2020" `mappend`
    Hakyll.constField "github-repo" "https://github.com/hahey/lanyon-hakyll" `mappend`
    Hakyll.defaultContext

-- | This context contains the base url.
baseCtx :: Hakyll.Context a
baseCtx =
    Hakyll.constField "baseurl" "https://lsmor.github.io/functional-programming-with-haskell/"

--------------------------------------------------------------------------------

-- | default context.
chapterCtx :: Hakyll.Context String
chapterCtx = Hakyll.defaultContext

--------------------------------------------------------------------------------

-- | The context of the sidebar. Currently, the list of chapters, the list of pages and the default context
sidebarCtx :: Hakyll.Context String -> Hakyll.Context String
sidebarCtx nodeCtx =
    Hakyll.listField "list_chapters" nodeCtx (Hakyll.loadAllSnapshots ("chapters/*" .&&. Hakyll.hasNoVersion) "content")
    --                                                                                                         |- dropping this complement pattern, causes Hakyll to loop (maybe) infinitely
    --                                                                                                         |  Notice that this happens only if line 125 does not match on pages/Home...
    <> Hakyll.listField "list_pages" nodeCtx (Hakyll.loadAllSnapshots ("pages/*" .&&. Hakyll.hasNoVersion .&&. Hakyll.complement "pages/Home.markdown") "page-content")
    <> Hakyll.defaultContext

-- | 
baseNodeCtx :: Hakyll.Context String
baseNodeCtx =
    Hakyll.urlField "node-url" `mappend`
    Hakyll.titleField "title" `mappend`
    baseCtx

baseSidebarCtx :: Hakyll.Context String
baseSidebarCtx = sidebarCtx baseNodeCtx

evalCtxKey :: Hakyll.Context String -> [String] -> Hakyll.Item String -> Hakyll.Compiler String
evalCtxKey context [key] item = Hakyll.unContext context key [] item >>=
        \case
            Hakyll.StringField s -> return s
            _             -> error "Internal error: StringField expected"

getMetadataKey :: [String] -> Hakyll.Item String -> Hakyll.Compiler String
getMetadataKey [key] item = Hakyll.getMetadataField' (Hakyll.itemIdentifier item) key


embedOnDefaultTemplate :: Hakyll.Item String -> Hakyll.Compiler (Hakyll.Item String)
embedOnDefaultTemplate = Hakyll.loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)