{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

module Cologne.Primitives (
    Primitive(Primitive)
  , intersect
  , bound
  , normal
  , colorInfo
  , Bbox(..)
  , bbox
  , bboxIntersect 
  , Dimension(..)
  , nextDim
  , Intersection(..)
  , Ray(..)
  , AccelStruct(..)
  , ReflectionType(..)
  , Context(..)
  , avgColor
  , toInt
  ) where

import Data.List (foldl1')
import Data.Vect.Float (Vec3(Vec3), (&+), (&*))
import Graphics.Formats.Assimp (Camera)
  
data Ray = Ray {
    origin    :: !Vec3
  , direction :: !Vec3
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
data ReflectionType = Diffuse | Specular | Refractive deriving (Show)

data Intersection a = Intersection {
    dist    :: !Float
  , info    :: !a
  , iNormal :: !Vec3
  } | Miss

data Bbox = Bbox {
    start :: !Vec3
  , stop  :: !Vec3
  }

data Dimension = X | Y | Z
nextDim :: Dimension -> Dimension
nextDim X = Y
nextDim Y = Z
nextDim Z = X

bbox :: [Primitive a] -> Bbox
bbox xs = Bbox smallest largest
  where
  -- TODO: loop fusion
  smallest = foldl1' minVec $ map (start . bound) xs
  largest  = foldl1' maxVec $ map (stop . bound) xs
  minVec (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    Vec3 (min x1 x2) (min y1 y2) (min z1 z2)
  maxVec (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    Vec3 (max x1 x2) (max y1 y2) (max z1 z2)

bboxIntersect :: Bbox -> Ray -> Bool
bboxIntersect (Bbox (Vec3 x1 y1 z1) (Vec3 x2 y2 z2)) 
              (Ray (Vec3 ox oy oz) (Vec3 dx dy dz)) =
  if lastin > firstout || firstout < 0 then False else True
    where
    (inx, outx) = (if dx > 0 then id else rev) ((x1-ox)/dx, (x2-ox)/dx)
    (iny, outy) = (if dy > 0 then id else rev) ((y1-oy)/dy, (y2-oy)/dy)
    (inz, outz) = (if dz > 0 then id else rev) ((z1-oz)/dz, (z2-oz)/dz)
    rev (a, b)  = (b, a)
    lastin      = max inx  (max iny  inz)
    firstout    = min outx (min outy outz)

data Primitive a = Primitive {

  -- | Test for intersection between the ray and the primitive
    intersect :: Ray -> Maybe Float

  -- bound returns an axis-aligned bounding box around the object, it should
  -- be as small as possible
  , bound     :: Bbox

  -- Return the normal vector at any point on the surface
  , normal    :: Vec3 -> Vec3
    
  -- Do your color stuff however you want, for instance if you're feeling
  -- really haskell-ish, you may like each object to have a function that you
  -- can call, thus b will be a function ((AccelStruct s) => s -> Ray ->
  -- Float -> Int -> Int -> ColorD) taking the scene, incoming ray,
  -- recursive depth, and a random number. This function would then be called
  -- by the radiance function. This approach gives a lot of flexibilitiy. On
  -- the other hand it is probably more efficient for b to be a tuple (Vec3,
  -- Vec3, ReflectionType) containing the color, emission, and whether the
  -- surface is diffuse, refractive, or specular. Then the logic would be
  -- shifted from the object to the radiance function.
  , colorInfo :: a
  }

instance Show (Primitive a) where
  show = const "prim"

{- This is the AccelStruct typeclass, which is used to store and interact with
 - the primitives of a scene. This can result in huge speedups because rather
 - than checking for intersection with every primitive in a scene (O(n)) we can
 - reduce the complexity to O(lg(n)).
 -}
-- a: The accel struct type, e.g. [Primitive b]
-- b: The info type
class AccelStruct a b | a -> b where
  insert            :: Primitive b -> a -> a
  aIntersect        :: a -> Ray -> Intersection b
  listToAccelStruct :: [Primitive b] -> a

data (AccelStruct a b) => Context a b = Context { 
    width   :: !Int        -- ^ Width in Pixels
  , height  :: !Int        -- ^ Height in Pixels
  , samples :: !Int        -- ^ Samples to take
  , cameras :: [Camera]    -- ^ List of cameras. Only the first is currently used.
  , scene   :: a           -- ^ Scene Primitive
  }

avgColor :: [Vec3] -> Vec3
avgColor xs = (foldl1' (&+) xs) 
  &* (1 / ((fromInteger . toInteger) (length xs)))

toInt :: (Floating a, Ord a, RealFrac a, Integral b) => a -> b
toInt x = truncate (((clamp x ** (1 / 2.2)) * 255) + 0.5)

clamp :: (Num a, Ord a) => a -> a
clamp x | x < 0     = 0
        | x > 1     = 1
        | otherwise = x
