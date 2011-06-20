> module Cologne.Lens (
>     runLens
>   , standardLens
>   ) where
> 
> import Graphics.Formats.Assimp (Camera, position, lookAt, horizontalFOV, aspect, up)
> import Data.Vect ((&+), (&*), (&^), normalize)
>
> import Cologne.Primitives

Introduction
============

The purpose of this module is to describe "lenses". These are *not* functional
programming lenses, but simulations of physical lenses on the front of a
camera. There are many different types we might like to simulate. One idea
would be to have all rays pointing in the same direction, straight out from the
screen. We could simulate a pinhole camera or a wide-angle camera. Usually,
though, we will probably want to simulate the human eye.

Describing the Camera
=====================

Coordinate Systems
------------------

Let's briefly discuss coordinate systems. Unfortunately there are two different
types of Cartesian coordinate systems, left-handed and right-handed. OpenGL by
convention uses a right-handed system, and DirectX uses a left-handed system.
On the left is a left-handed coordinate system, and on the right is a
right-handed coordinate system. In case you couldn't tell from the picture, on
the left the z-axis is pointing out the other side of the screen and on the
right it is pointing out from the screen towards you.

![3 dimensional cartesian coordinate system handedness](handedness.png)

The authors of Assimp have decided to use a right-handed system so we will too.
From now on you may assume we're working in a coordinate system that looks like
the one on the right.

Computing Shot Ray Direction
----------------------------

We're concerned with 5 parts of the description of the camera: the vectors
`position`, `up`, and `lookAt`, and the `width` and `height` of the screen. One
thing to keep in mind is that `up` is not necessarily at a right angle to
`lookAt`, but it represents the up direction in the scene.

Since it would look weird doing math with such long names, let's give them
symbols:

\newcommand{\vect}[1]{\mathbf{#1}}

\begin{align*}
\tt{position} &: \vect{p} \\
\tt{up} &: \vect{u} \\
\tt{lookAt} &: \vect{l} \\
\tt{width} &: w \\
\tt{height} &: h
\end{align*}

We can compute the vectors `u'` and `r` as shown in the figure as follows.

$u' = \frac{u \times l}{}$

$\vect{P}_{ij} = \vect{A} + c_u \vect{u} + c_v \vect{v}$

$c_u = \left(\frac{2i + 1}{2x_{res}}-\frac{1}{2}\right) L_u$

$c_v = \left(\frac{2j + 1}{2y_{res}}-\frac{1}{2}\right) L_v$

Lens Definition
---------------

> newtype Lens = Lens {
>   runLens :: Camera -- ^ we're shooting rays out of this camera
>           -> Int    -- ^ total rows
>           -> Int    -- ^ total columns
>           -> Int    -- ^ row
>           -> Int    -- ^ column
>           -> Float  -- ^ horizontal wiggle in range (-1,1)
>           -> Float  -- ^ vertical wiggle in range (-1,1)
>           -> Ray    
> }

Standard Lens
-------------

> standardLens :: Lens
> standardLens = Lens stdcamdir
>
> stdcamdir :: Camera -> Int -> Int -> Int -> Int -> Float -> Float -> Ray
> stdcamdir cam w h x y _ _ = ray
>   where
>     ray = Ray ((position cam) &+ (dir &* 140.0)) (normalize dir)
>     dir = (lookAt cam) 
>            &+ (deltax &* ((iToF x) - (iToF w)/2)) 
>            &+ (deltay &* ((iToF y) - (iToF h)/2))
>     totalx  = tan . horizontalFOV $ cam
>     totaly  = tan . (/(aspect cam)) . horizontalFOV $ cam
>     deltax  = (normalize right) &* (totalx / (iToF w))
>     deltay  = (normalize up')   &* (totaly / (iToF h))
>     right   = normalize $ (lookAt cam) &^ (up cam)
>     up'     = normalize $ right &^ (lookAt cam)
>     iToF = fromInteger . toInteger

References
==========

Vector Algebra for Ray Tracing, Dr. Thomas W. Sederberg
