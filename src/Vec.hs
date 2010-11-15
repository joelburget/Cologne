{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Vec (
    VecD(..)
  , VecI(..)
  , ColorD
  , ColorI
  , (|*|)
  , (|+|)
  , (|-|)
  , vecD2I
  , cross
  , dot
  , norm
  , vmult
  , Vec.fmap
  , clamp
  , toInt
  ) where

import Data.Data
import Data.Typeable

{- We up toInt and clamp here even though they don't really fit in this module
 - because of dependency issues -}

toInt :: (Floating a, Ord a, RealFrac a, Integral b) => a -> b
toInt x = truncate (((clamp x ** (1 / 2.2)) * 255) + 0.5)

clamp :: (Num a, Ord a) => a -> a
clamp x | x < 0 = 0
        | x > 1 = 1
        | otherwise = x

{-
 - Here we define the basic vector types, VecD and VecI, representing a
 - 3-dimensional vector of Double and Int respectively. The reason for using
 - VecD and VecI, rather than Vec a, is for performance. This is one area
 - where increased complexity is worth it.
 -}

-- Strict vector types reduced execution time from 15 to 13 seconds in early testing
-- In the future I may decide to allow Float as well
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
