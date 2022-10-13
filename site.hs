--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid                   (mappend)
import           Data.List                     (sortBy)
import           Data.Ord                      (comparing)
import qualified Hakyll
import           Control.Monad                 (liftM, forM_)
import           System.FilePath               (takeBaseName)
import           Data.Function (on)
--------------------------------------------------------------------------------
main :: IO ()
main = Hakyll.hakyll $ do
    Hakyll.match ("images/*" Hakyll..||. "js/*") $ do
        Hakyll.route   Hakyll.idRoute
        Hakyll.compile Hakyll.copyFileCompiler

    Hakyll.match "css/*" $ do
        Hakyll.route   Hakyll.idRoute
        Hakyll.compile Hakyll.compressCssCompiler

    Hakyll.match "error/*" $ do
        Hakyll.route (Hakyll.gsubRoute "error/" (const "") `Hakyll.composeRoutes` Hakyll.setExtension "html")
        Hakyll.compile $ Hakyll.pandocCompiler
            >>= Hakyll.applyAsTemplate siteCtx
            >>= Hakyll.loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)

    Hakyll.match "pages/*" $ do
        Hakyll.route $ Hakyll.setExtension "html"
        Hakyll.compile $ do
            pageName <- takeBaseName . Hakyll.toFilePath <$> Hakyll.getUnderlying
            let pageCtx = Hakyll.constField pageName "" `mappend`
                          baseNodeCtx
            let evalCtx = Hakyll.functionField "get-meta" getMetadataKey `mappend`
                          Hakyll.functionField "eval" (evalCtxKey pageCtx)
            let activeSidebarCtx = sidebarCtx (evalCtx <> pageCtx)

            Hakyll.pandocCompiler
                >>= Hakyll.saveSnapshot "page-content"
                >>= Hakyll.loadAndApplyTemplate "templates/page.html"    siteCtx
                >>= Hakyll.loadAndApplyTemplate "templates/default.html" (activeSidebarCtx <> siteCtx)
                >>= Hakyll.relativizeUrls

    tags <- Hakyll.buildTags "posts/*" (Hakyll.fromCapture "tags/*.html")

    Hakyll.match "posts/*" $ Hakyll.version "meta" $ do
        Hakyll.route   $ Hakyll.setExtension "html"
        Hakyll.compile Hakyll.getResourceBody

    Hakyll.match "posts/*" $ do
        Hakyll.route $ Hakyll.setExtension "html"
        Hakyll.compile $ do
            posts <- Hakyll.loadAll ("posts/*" Hakyll..&&. Hakyll.hasVersion "meta")
            let taggedPostCtx = Hakyll.tagsField "tags" tags `mappend`
                                postCtx `mappend`
                                relatedPostsCtx posts 3

            Hakyll.pandocCompiler
                >>= Hakyll.saveSnapshot "content"
                >>= Hakyll.loadAndApplyTemplate "templates/post.html" taggedPostCtx
                >>= Hakyll.loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
                >>= Hakyll.relativizeUrls

    Hakyll.create ["archive.html"] $ do
        Hakyll.route Hakyll.idRoute
        Hakyll.compile $ do
            posts <- Hakyll.recentFirst =<< Hakyll.loadAllSnapshots ("posts/*" Hakyll..&&. Hakyll.hasNoVersion) "content"
            let archiveCtx =
                    Hakyll.listField "posts" postCtx (return posts) `mappend`
                    Hakyll.constField "title" "Archive"             `mappend`
                    Hakyll.constField "archive" ""                  `mappend`
                    siteCtx

            Hakyll.makeItem ""
                >>= Hakyll.loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= Hakyll.loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> archiveCtx)
                >>= Hakyll.relativizeUrls

    paginate <- Hakyll.buildPaginateWith postsGrouper "posts/*" postsPageId

    Hakyll.paginateRules paginate $ \page pattern -> do
        Hakyll.route Hakyll.idRoute
        Hakyll.compile $ do
            posts <- Hakyll.recentFirst =<< Hakyll.loadAllSnapshots (pattern Hakyll..&&. Hakyll.hasNoVersion) "content"
            let indexCtx =
                    Hakyll.constField "title" (if page == 1 then "Home"
                                                     else "Blog posts, page " ++ show page) `mappend`
                    Hakyll.listField "posts" postCtx (return posts) `mappend`
                    Hakyll.constField "home" "" `mappend`
                    Hakyll.paginateContext paginate page `mappend`
                    siteCtx

            Hakyll.makeItem ""
                >>= Hakyll.applyAsTemplate indexCtx
                >>= Hakyll.loadAndApplyTemplate "templates/index.html" indexCtx
                >>= Hakyll.loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> indexCtx)
                >>= Hakyll.relativizeUrls

    Hakyll.match "templates/*" $ Hakyll.compile Hakyll.templateBodyCompiler

    Hakyll.create ["atom.xml"] $ do
        Hakyll.route Hakyll.idRoute
        Hakyll.compile $ do
            let feedCtx = postCtx `mappend`
                    Hakyll.bodyField "description"
            posts <- fmap (take 10) . Hakyll.recentFirst =<< Hakyll.loadAllSnapshots ("posts/*" Hakyll..&&. Hakyll.hasNoVersion) "content"
            Hakyll.renderAtom feedConfig feedCtx posts

--------------------------------------------------------------------------------

postsGrouper :: (Hakyll.MonadMetadata m, MonadFail m) => [Hakyll.Identifier] -> m [[Hakyll.Identifier]]
postsGrouper = fmap (Hakyll.paginateEvery 3) . Hakyll.sortRecentFirst

postsPageId :: Hakyll.PageNumber -> Hakyll.Identifier
postsPageId n = Hakyll.fromFilePath $ if n == 1 then "index.html" else show n ++ "/index.html"

--------------------------------------------------------------------------------

feedConfig :: Hakyll.FeedConfiguration
feedConfig = Hakyll.FeedConfiguration
    { Hakyll.feedTitle       = "lanyon-hakyll: Lanyon Theme on Hakyll"
    , Hakyll.feedDescription = "A Fork of Lanyon based on Poole"
    , Hakyll.feedAuthorName  = "Heuna Kim"
    , Hakyll.feedAuthorEmail = "ai@heuna-kim.net"
    , Hakyll.feedRoot        = "https://github.com/hahey/lanyon-hakyll"
    }

--------------------------------------------------------------------------------

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

baseCtx :: Hakyll.Context a
baseCtx =
    Hakyll.constField "baseurl" "http://localhost:8000"

--------------------------------------------------------------------------------

postCtx :: Hakyll.Context String
postCtx =
    Hakyll.dateField "date" "%B %e, %Y" `mappend`
    Hakyll.defaultContext

tagsRulesVersioned :: Hakyll.Tags -> (String -> [Hakyll.Identifier] -> Hakyll.Rules ()) -> Hakyll.Rules ()
tagsRulesVersioned tags rules =
    forM_ (Hakyll.tagsMap tags) $ \(tag, identifiers) ->
        Hakyll.rulesExtraDependencies [Hakyll.tagsDependency tags] $
            Hakyll.create [Hakyll.tagsMakeId tags tag] $
                rules tag identifiers

relatedPostsCtx
  :: [Hakyll.Item String]  -> Int  -> Hakyll.Context String
relatedPostsCtx posts n = Hakyll.listFieldWith "related_posts" postCtx selectPosts
  where
    rateItem ts i = length . filter (`elem` ts) <$> Hakyll.getTags (Hakyll.itemIdentifier i)
    selectPosts s = do
      postTags <- Hakyll.getTags $ Hakyll.itemIdentifier s
      let trimmedItems = filter (not . matchPath s) posts
      take n . reverse <$> sortOnM (rateItem postTags) trimmedItems

matchPath :: Hakyll.Item String -> Hakyll.Item String -> Bool
matchPath = (==) `on` (Hakyll.toFilePath . Hakyll.itemIdentifier)

sortOnM :: (Monad m, Ord b) => (a -> m b) -> [a] -> m [a]
sortOnM f xs = map fst . sortBy (comparing snd) . zip xs <$> mapM f xs

--------------------------------------------------------------------------------

sidebarCtx :: Hakyll.Context String -> Hakyll.Context String
sidebarCtx nodeCtx =
    Hakyll.listField "list_posts" nodeCtx (Hakyll.recentFirst =<< Hakyll.loadAllSnapshots ("posts/*" Hakyll..&&. Hakyll.hasNoVersion) "content")
    <> Hakyll.listField "list_pages" nodeCtx (Hakyll.loadAllSnapshots ("pages/*" Hakyll..&&. Hakyll.hasNoVersion) "page-content") 
    <> Hakyll.defaultContext

baseNodeCtx :: Hakyll.Context String
baseNodeCtx =
    Hakyll.urlField "node-url" `mappend`
    Hakyll.titleField "title" `mappend`
    baseCtx

baseSidebarCtx :: Hakyll.Context String
baseSidebarCtx = sidebarCtx baseNodeCtx

evalCtxKey :: Hakyll.Context String -> [String] -> Hakyll.Item String -> Hakyll.Compiler String
evalCtxKey context [key] item = Hakyll.unContext context key [] item >>= \cf ->
        case cf of
            Hakyll.StringField s -> return s
            _             -> error $ "Internal error: StringField expected"

getMetadataKey :: [String] -> Hakyll.Item String -> Hakyll.Compiler String
getMetadataKey [key] item = Hakyll.getMetadataField' (Hakyll.itemIdentifier item) key
