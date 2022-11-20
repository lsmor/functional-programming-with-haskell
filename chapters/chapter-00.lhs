---
title: Programs, Functions and Plots
chapter-num: 0
tags: introduction
description: In this chapter you will learn the basics of Haskell syntax
jupyter:
  nbformat: 4
  nbformat_minor: 4
  kernelspec:
     display_name: Haskell
     language: haskell
     name: haskell
  language_info:
     codemirror_mode: ihaskell
     file_extension: .hs
     name: haskell
     pygments_lexer: Haskell
     version: 8.10.7
---



== Introduction

- This chapter introduces some functional programing concepts defining functions which generate plots
- We will use [CodeWorld/Haskell](https://code.world/haskell#) for this chapter
- CodeWorld is an interactive web for generating plots and animations using Haskell code

== Basic Plots

First, let's see the basic constructors CodeWorld provides

=== Plot a Circunference

- **Example**: a program which draws a circunference of radious 3

![]($baseurl$/images/circunference.png)


- **Program**

\begin{code}
import CodeWorld

main :: IO ()
main = drawingOf circunference

circunference :: Picture
circunference = circle 3
\end{code}

- **Execute** the code in [CodeWorld](https://code.world/haskell#P5KKezecmTgz9bQQ6aGzYLg)

- **Comments**:

   - In Haskell, type annotations are written with double colon: `<function name> :: <types>`
   - Type annotations are mostly optional, but are highly recomended, as they provide usefull information about the program
   - In the code example `circunference` has type `Picture`, which is the type provided by `CodeWorld` library for representing pictures
   - the type of main is `IO ()` meaning that `main` is a function which does input/output (hence `IO`) and doesn't returns a value (hence `()`)
   - As you can see, both `main` and `circunference` are built from smaller parts called `circle` and `drawingOf`
     ```haskell
     circle :: Double -> Picture   -- circle is a function which takes a Double and produces a Picture
     drawingOf :: Picture -> IO () -- drawingOf is a function which takes a Picture and produces an input/output action.
     ```

=== Plot a Solid Circle

- **Example**: A program which plots a circle of radious 3

![]($baseurl$/images/solidcircle.png)


- **Program**:

\begin{code}
import CodeWorld

main :: IO ()
main = drawingOf dot

dot :: Picture
dot = solidCircle 3
\end{code}

- **Execute** the code in [CodeWorld](https://code.world/haskell#P3jImu_nMYR4NLwfiMEMpbA)

- **Comments**:

  - function `solidCircle :: Double -> Picture` takes a double (the radious) and produces a Picture

=== Plot of the Coordinate Plane

- **Example**: A program which plots the coordinate plane

![]($baseurl$/images/coordinatePlane.png)

- **Program**:

\begin{code}
import CodeWorld

main :: IO ()
main = drawingOf plane

plane :: Picture
plane = coordinatePlane
\end{code}

- **Execute** the code in [CodeWorld](https://code.world/haskell#P0i9McWGHcgIYlVH8NafIig)

- **Comments**:

  - function `coordinatePlane :: Picture` is a picture provided by `CodeWorld` library.

=== Plot Some Rectangles

- **Example**: plot a basic rectangle

![]($baseurl$/images/rectangle.png)

- **Program**:

\begin{code}
import CodeWorld

main :: IO ()
main = drawingOf rect

rect :: Picture
rect = rectangle 6 3
\end{code}

- **Execute** the code in [CodeWorld](https://code.world/haskell#PwlyYoJ_zFPuXLxIJlobSig)

- **Comments**:
  - function `rectangle :: Double -> Double -> Picture` takes two `Double`s (base and height) and produces a `Picture`

- **Example**: modify previous example to produce a thicker rectangle

![]($baseurl$/images/thickrectangle.png)

- **Program**:

\begin{code}
import CodeWorld

main :: IO ()
main = drawingOf rect

rect :: Picture
rect = thickRectangle 0.5 6 3
\end{code}

- **Execute** the code in [CodeWorld](https://code.world/haskell#PeXWSobEbKkcq6DYN0SxEsg)

- **Comments**:
   - function `thickRectangle :: Double -> Double -> Double -> Picture` takes three `Double`s (thickness, base and height) and produces a `Picture`

- **Example**: modify previous example to produce a solid rectangle

![]($baseurl$/images/solidRectangle.png)

- **Program**:

\begin{code}
import CodeWorld

main :: IO ()
main = drawingOf rect

rect :: Picture
rect = solidRectangle 6 3
\end{code}

- **Execute** the code in [CodeWorld](https://code.world/haskell#PqM_KvRjg_AJJgTjxo0nJ6A)

- **Comments**:
   - function `solidRectangle :: Double -> Double -> Picture` takes two `Double`s (base and height) and produces a `Picture`

=== Plot Paths

- **Example**: A program which draws a path passing through points (-3,3), (3,3), (-3,-3) and (3,-3)

![]($baseurl$/images/openPath.png)

- **Program**:

\begin{code}
import CodeWorld

main :: IO ()
main = drawingOf openPath

openPath :: Picture
openPath = polyline [(-3,3),(3,3),(-3,-3),(3,-3)]
\end{code}

- **Execute** the code in [CodeWorld](https://code.world/haskell#P1OkzsDM_YkB3PG6NQ879oQ)

- **Comments**:
   - function `polyline :: [Point] -> Picture` takes a list of points and returns a picture
   - A point is just a pair of `Double`s. In Haskell you can define a type synonym with the `type` keyword: `type Point = (Double, Double)`

- **Example**: A program which draws a closed path passing through points (-9,-9), (0,9) and (9,-9) forming a rectangle.

![]($baseurl$/images/closedPath.png)

- **Program**:

\begin{code}
import CodeWorld

main :: IO ()
main = drawingOf closedPath

closedPath :: Picture
closedPath = polygon [(-9,-9),(0,9),(9,-9)]
\end{code}

- **Execute** the code in [CodeWorld](https://code.world/haskell#PKC0AxSHjLz5TadYtoJ_GPg)

- **Comments**:
   - function `polygon :: [Point] -> Picture` takes a list of points and returns a picture. 
   - The difference between `polygon` and `polyline` is that the former will produce a closed path, and the later an open one.


=== Plot Paths