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
  ) where

import Cologne.Vec
import Data.List (foldl1')
  
data Ray = Ray {
    origin    :: !VecD
  , direction :: !VecD
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
    dist    :: !Double
  , info    :: !a
  , iNormal :: !VecD
  } | Miss

data Bbox = Bbox {
    start :: !VecD
  , stop  :: !VecD
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
  minVec (VecD x1 y1 z1) (VecD x2 y2 z2) =
    VecD (min x1 x2) (min y1 y2) (min z1 z2)
  maxVec (VecD x1 y1 z1) (VecD x2 y2 z2) =
    VecD (max x1 x2) (max y1 y2) (max z1 z2)

bboxIntersect :: Bbox -> Ray -> Bool
bboxIntersect (Bbox (VecD x1 y1 z1) (VecD x2 y2 z2)) 
              (Ray (VecD ox oy oz) (VecD dx dy dz)) =
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
    intersect :: Ray -> Maybe Double

  -- bound returns an axis-aligned bounding box around the object, it should
  -- be as small as possible
  , bound     :: Bbox

  -- Return the normal vector at any point on the surface
  , normal    :: VecD -> VecD
    
  -- Do your color stuff however you want, for instance if you're feeling
  -- really haskell-ish, you may like each object to have a function that you
  -- can call, thus b will be a function ((AccelStruct s) => s -> Ray ->
  -- Double -> Int -> Int -> ColorD) taking the scene, incoming ray,
  -- recursive depth, and a random number. This function would then be called
  -- by the radiance function. This approach gives a lot of flexibilitiy. On
  -- the other hand it is probably more efficient for b to be a tuple (VecD,
  -- VecD, ReflectionType) containing the color, emission, and whether the
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
    ctxw      :: !Int        -- ^ Width in Pixels
  , ctxh      :: !Int        -- ^ Height in Pixels
  , ctxsamp   :: !Int        -- ^ Samples to take?
  , ctxcx     :: !VecD       -- ^ Change in x Direction per pixel
  , ctxcy     :: !VecD       -- ^ Change in y Direction per pixel
  , ctxcamdir :: !VecD       -- ^ Camera Direction
  , ctxcampos :: !VecD       -- ^ Camera Position
  , ctxscene  :: a           -- ^ Scene Primitive
  } -- deriving (Data, Typeable)
