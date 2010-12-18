{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}

module Cologne.Render (
    Context(..)
  , radiance
  ) where

import Data.List hiding (intersect)
import Data.Ord
import Random

import Cologne.Vec
import Cologne.Shaders
import Cologne.Primitives hiding (intersect)
import Cologne.Accel
  
{- This is  where the magic happens. radiance returns the color that a ray sees
 - when it intersects the scene. If the ray misses everything, just return
 - black.
 -}

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
