# Functional Programming with Haskell

## Introduction

This online book is a translation of ["Programación Funcional con Haskell"](https://jaalonso.github.io/materias/PFconHaskell/temas.html) by [José A. Alonso Jiménez](https://jaalonso.github.io/) at University of Seville. It is suitable for students with or without prior knowledge in functional programming or progamming in general.

## Site and Theme

- This book is built with [Hakyll](https://jaspervdj.be/hakyll/) originally written by Jasper Van der Jeugt.
- It uses [Lanyon theme](https://github.com/poole/lanyon) originated by Mark Otto (mdo) and ported to Hakyll by [Heuna Kim]("https://github.com/hahey/lanyon-hakyll")
- Heuna's original code has been forked and adapted to fit the workflow for this book better

## Contributing

Feel free to open an issue if you find a typo or something poorly explained. Spanish speaker can also translate chapters from the [original book](https://jaalonso.github.io/materias/PFconHaskell/temas.html) and submit them.

## Building

The project is built with cabal. Use `cabal build` to build an executable called `site`. Then run `cabal exec site rebuild` to actually build the site and `cabal exec site watch` to start a http server capable of hot reloading changes in your markdowns and templates.


On `vscode` you can create a building task within `.vscode/task.json` (not tested in Windows nor MacOs). If you set it as the default build task you can run it with `Ctrl + Shift + B`

```json
{
    // See https://go.microsoft.com/fwlink/?LinkId=733558
    // for the documentation about the tasks.json format
    "version": "2.0.0",
    "tasks": [
        {
            "label": "build and watch site",
            "type": "shell",
            "command": "cabal build && cabal exec site rebuild && cabal exec site watch",
            "problemMatcher": [],
            "group": {
                "kind": "build",
                "isDefault": true
            }
        }
    ]
}
```
