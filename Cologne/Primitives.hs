{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
--{-# LANGUAGE TypeSynonymInstances #-}

module Cologne.Primitives (
    Primitive
  , color
  , intersect
  , bound
  , Prim(..)
  , Bbox(..)
  , Intersection(..)
  , Ray(..)
  , AccelStruct(..)
  , Accel(..)
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

data Intersection = Intersection {
    dist   :: !Double
  , object :: !Prim
  } | Miss

data Bbox = Bbox {
    start      :: !VecD
  , dimensions :: !VecD
  }

{- There is a relevant post here
 - http://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/
 - that discusses whether it would be better to just use something like 
 - > data Primitive
 -
 - I would like to test this out in the future. (Be sure to read the comments
 - too, it looks like there's good discussion there.
 -}

class Primitive a where
  -- | Test for intersection between the ray and the primitive
  intersect :: a -> Ray -> Intersection
  -- bound returns an axis-aligned bounding box around the object, it should 
  -- be as small as possible
  bound     :: a -> Bbox
  -- color takes the object, the incoming ray, the recursive depth, and a random number
  color     :: (AccelStruct b) => a -> b -> Ray -> Double -> Int -> Int -> ColorD

-- TODO: check if newtype performs better here
data Prim = forall a. Primitive a => Prim a

instance Primitive Prim where
  intersect (Prim a) !ray                      = intersect a ray
  bound (Prim a)                               = bound a
  color (Prim a) !accel !ray !len !depth !rand = color a accel ray len depth rand

class AccelStruct a where
  insert            :: Prim -> a -> a
  aIntersect        :: a -> Ray -> Intersection
  listToAccelStruct :: [Prim] -> a

data Accel = forall a. AccelStruct a => Accel a

instance AccelStruct Accel where
  insert prim (Accel a)    = insert prim (Accel a)
  aIntersect (Accel a) ray = aIntersect a ray
  listToAccelStruct lst    = listToAccelStruct lst
