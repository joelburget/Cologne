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
  , Intersection(..)
  , Ray(..)
  , AccelStruct(..)
  , ReflectionType(..)
  ) where

import Control.Applicative
import Control.Monad.State
import Data.List hiding (intersect, insert)
import Data.Ord
import Random

import Cologne.Vec
  
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
    start      :: !VecD
  , dimensions :: !VecD
  }

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

-- a: The accel struct type
-- b: The info type
class AccelStruct a b | a -> b where
  insert            :: Primitive b -> a -> a
  aIntersect        :: a -> Ray -> Intersection b
  listToAccelStruct :: [Primitive b] -> a
