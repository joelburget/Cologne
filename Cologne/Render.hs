{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}

module Cologne.Render (
    Context(..)
  , radiance
  ) where

import Data.List hiding (intersect)
import Data.Ord
import Random

import Cologne.Vec
import Cologne.Primitives.Primitives hiding (intersect)
import Cologne.Accel.AccelStruct 
  
{- This is  where the magic happens. radiance returns the color that a ray sees
 - when it intersects the scene. If the ray misses everything, just return
 - black.
 -}

radiance :: (AccelStruct a) => a -> Ray -> Int -> Int -> ColorD
radiance scene r depth rand | Intersection t obj <- intersect scene r = color obj r depth rand
                            | otherwise = VecD 0 0 0

data (AccelStruct a) => Context a = Context { 
    ctxw      :: !Int        -- ^ Width in Pixels
  , ctxh      :: !Int        -- ^ Height in Pixels
  , ctxsamp   :: !Int        -- ^ Samples to take?
  , ctxcx     :: !VecD       -- ^ Change in x Direction per pixel
  , ctxcy     :: !VecD       -- ^ Change in y Direction per pixel
  , ctxcamdir :: !VecD       -- ^ Camera Direction
  , ctxcampos :: !VecD       -- ^ Camera Position
  , ctxscene  :: a           -- ^ Scene Primitive
  } -- deriving (Data, Typeable)
