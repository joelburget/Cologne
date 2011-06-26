{-# LANGUAGE FlexibleContexts #-}

module Cologne.Shaders.Debug (
    debug
  ) where

import Control.Monad.ST
import Data.Word
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Control.Monad.State (State, runState)
import Data.Vect (Vec3(Vec3), (&+), (&*), (&.), (&^), len, normalize)
import Data.Vector.Unboxed (Vector, unsafeFreeze, enumFromN, forM_)
import Data.Vector.Unboxed.Mutable (MVector, new, read, write)
import Data.Array.Repa
import Graphics.Formats.Assimp (lookAt, position, horizontalFOV, aspect, up, 
  Camera(Camera))

import Cologne.Primitives hiding (intersect, Z)
import Cologne.AssimpImport (ColorInfo)

-- Just return the color we intersect multiplied by the cosine of the angle of
-- intersection.
radiance :: (AccelStruct a (Vec3 , Vec3, ReflectionType))
         => a
         -> Ray
         -> Int
         -> Vec3
radiance scene ray _ = do
  case aIntersect scene ray of
    Miss -> Vec3 0 0 0
    Intersection _ (color, _, _) nrm ->
      color &* (abs (((direction ray) &. nrm) / ((len (direction ray)) * (len nrm))))

debug :: Context [Primitive ColorInfo] ColorInfo
      -> Array DIM3 Word8
debug (Context options cams scene) = fromVector (Z :. w :. h :. 4) $ runST generatePicture
  where
    generatePicture :: ST s (Vector Word8)
    generatePicture = do
      pic <- new (w * h * 4) -- The 4 comes from the 4 channels of rgba
      forM_ (enumFromN 0 h) $ \row -> do
        -- vec <- new w
        forM_ (enumFromN 0 w) $ \column -> do
          let (Vec3 r g b) = radiance scene (ray column row) 0
              ind = (row*w + column)*4
          -- write vec column val
          write pic (ind + 0) $ round (r*255)
          write pic (ind + 1) $ round (g*255)
          write pic (ind + 2) $ round (b*255)
          write pic (ind + 3) $ 255
        -- unsafeFreeze vec >>= write pic (h-row-1)
        -- write pic (h-row-1)
      unsafeFreeze pic

    w = width options
    h = height options
    samp = samples options
    cam | length cams > 0 = head cams
        | otherwise       = Camera "" (Vec3 0 0 500) (Vec3 0 1 0) 
                                   (Vec3 0 0.1 (-1)) 0.5 1e-2 1e5 1
    dir x y = (cx &* (((fromIntegral x) / fromIntegral w) - 0.5))
           &+ (cy &* (((fromIntegral y) / fromIntegral h) - 0.5))
           &+ (lookAt cam)
    ray x y = Ray ((position cam) &+ ((dir x y) &* 140.0)) (normalize (dir x y))
    cx = Vec3 (0.5135 * fromIntegral w / fromIntegral h) 0 0
    cy = normalize (cx &^ (lookAt cam)) &* 0.5135
