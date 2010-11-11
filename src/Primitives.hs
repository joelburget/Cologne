{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
module Primitives (
    Refl(..)
  , Object(..)
  , VecD(..)
  , VecI(..)
  , (|*|)
  , (|+|)
  , (|-|)
  , vecD2I
  , clamp
  , toInt
  , cross
  , dot
  , line
  , norm
  , vmult
  , Render.fmap
  ) where

import Control.Applicative
import Control.Monad.State
import Data.Typeable
import Data.Data
import Data.List
import Data.Ord
import Random

{-
 - Here we define the basic vector types, VecD and VecI, representing a
 - 3-dimensional vector of Double and Int respectively. The reason for using
 - VecD and VecI, rather than Vec a, is for performance. This is one area
 - where increased complexity is worth it.
 -}

-- Strict vector types reduced execution time from 15 to 13 seconds in early testing
data VecD = VecD !Double !Double !Double deriving (Data, Typeable)
data VecI = VecI !Int !Int !Int deriving (Data, Typeable)

type ColorD = VecD
type ColorI = VecI

(|+|) :: VecD -> VecD -> VecD
(VecD x1 y1 z1) |+| (VecD x2 y2 z2) = VecD (x1 + x2) (y1 + y2) (z1 + z2)

(|-|) :: VecD -> VecD -> VecD 
(VecD x1 y1 z1) |-| (VecD x2 y2 z2) = VecD (x1 - x2) (y1 - y2) (z1 - z2)

(|*|) :: VecD -> Double -> VecD 
(VecD x y z) |*| n = VecD (n*x) (n*y) (n*z)

vmult :: VecD -> VecD -> VecD 
(VecD x1 y1 z1) `vmult` (VecD x2 y2 z2) = VecD (x1 * x2) (y1 * y2) (z1 * z2)

norm :: VecD -> VecD 
norm v = let VecD x y z = v in v |*| (1 / sqrt ((x * x) + (y * y) + (z * z)))

dot :: VecD -> VecD -> Double
(VecD x1 y1 z1) `dot` (VecD x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

cross :: VecD -> VecD -> VecD 
(VecD x1 y1 z1) `cross` (VecD x2 y2 z2) = VecD (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

fmap :: (Double -> Double) -> VecD -> VecD
fmap f (VecD x y z) = VecD (f x) (f y) (f z)

vecD2I :: VecD -> VecI
vecD2I (VecD x y z) = VecI (toInt x) (toInt y) (toInt z)

infixl 6 |+|
infixl 6 |-|
infixl 7 |*|
  
data Ray = Ray {
    origin    :: VecD
  , direction :: VecD
  }

{-
 - Here we consider three different types of interaction between ray and
 - surface:
 -
 - Diffusion: This is when the rays coming into an object are sent off in
 - random directions. Think matte paint. A potential issue here is that the
 - outbound rays are, to us, random. Thus we may wish to pass around a
 - pseudo-random number generator, perhaps in the state.
 -
 - Specular Reflection: This is what we typically think of as reflection, where
 - a ray comes into a surface and exits at the same angle, think of a mirror.
 -
 - Refraction: Refraction is the change in direction of a wave due to a change
 - in its speed. Think of a straw appearing to change direction in water.
 -
 - Refraction is governed by Snell's law: sin(theta_1) / sin(theta_2) = n_1 /
 - n_2 where theta is the angle from the normal of the boundary of the surface
 - and n is the index of refraction of the material.
 -}

data Refl = DIFF -- ^ Diffuse
          | SPEC -- ^ Specular
          | REFR -- ^ Refraction
          deriving (Data, Typeable)

data Object = Sphere { 
    radius :: Double
  , position :: VecD
  --, colorf :: Maybe (Ray a -> VecD a)
  , emission :: VecD
  , color :: Color
  , refl :: Refl 
  } | Triangle {
    x :: Double
  , y :: Double
  , z :: Double
  } deriving (Data, Typeable)

intersectSphere :: Ray -> Object -> Maybe Double
intersectSphere (Ray o d) s | det < 0 = Nothing
                            | t > eps = Just t
                            | t' > eps = Just t'
                            | otherwise = Nothing
                            where op = position s |-| o -- Solve t^2*d.d + 2*t*(o-p).d + (o-p).(o-p)-R^2 = 0
                                  eps = 1e-4
                                  b = op `dot` d
                                  det = (b * b) - (op `dot` op) + (radius s * radius s)
                                  det' = sqrt det
                                  t = b - det'
                                  t' = b + det'
