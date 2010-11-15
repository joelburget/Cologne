{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ExistentialQuantification #-}

module Primitives (
    Refl(..)
  , Object(..)
  ) where

import Control.Applicative
import Control.Monad.State
import Data.Typeable
import Data.Data
import Data.List
import Data.Ord
import Random
  
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

-- This should no longer be necessary
-- data Refl = DIFF -- ^ Diffuse
--           | SPEC -- ^ Specular
--           | REFR -- ^ Refraction
--           deriving (Data, Typeable)

data Intersection = Intersection {
  dist   :: !Double,
  object :: !Prim
  } | Miss

class Primitive a where
  -- | Test for intersection between the ray and the primitive
  intersect :: a -> Ray -> Intersection
  bound :: a -> Bbox
  color :: a -> Ray -> ColorD

data Prim = forall a. Primitive a => Prim a
