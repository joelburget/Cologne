{-# LANGUAGE FlexibleContexts #-}

module Cologne.Shaders.Debug (
    debug
  ) where

import Control.Monad.ST
import System.Random.MWC (Seed)
import Data.Word
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Vect (Vec3(Vec3), (&+), (&*), (&.), (&^), len, normalize)
import Data.Vector.Unboxed (Vector, unsafeFreeze, enumFromN, forM_)
import Data.Vector.Unboxed.Mutable (MVector, new, read, write)
import Graphics.Formats.Assimp (lookAt, position, horizontalFOV, aspect, up, 
  Camera(Camera))

import Cologne.Primitives hiding (intersect, Z)
import Cologne.AssimpImport (ColorInfo)

-- Just return the color we intersect multiplied by the cosine of the angle of
-- intersection.
{-# INLINE radiance #-}
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
       -> Int
       -> Int
       -> Seed
       -> Vec3
debug (Context options cams scene) column row _ = 
  radiance scene (ray options cams column row) 0

--debug :: Context [Primitive ColorInfo] ColorInfo
--      -> Array DIM3 Word8
--debug (Context options cams scene) = fromVector (Z :. w :. h :. 4) $ runST generatePicture
--  where
--    generatePicture :: ST s (Vector Word8)
--    generatePicture = do
--      pic <- new (w * h * 4) -- The 4 comes from the 4 channels of rgba
--      forM_ (enumFromN 0 h) $ \row -> do
--        forM_ (enumFromN 0 w) $ \column -> do
--          writePixel pic row column (radiance scene (ray options column row) 0)
--      unsafeFreeze pic
--
--    writePixel pic row column (Vec3 r g b) =
--      let index = (row*w + column)*4
--      in do
--        write pic (index + 0) $ round (r*255)
--        write pic (index + 1) $ round (g*255)
--        write pic (index + 2) $ round (b*255)
--        write pic (index + 3) $ 255

ray :: Options -> [Camera] -> Int -> Int -> Ray
ray options cams x y = Ray ((position cam) &+ ((dir x y) &* 140.0)) (normalize (dir x y))
  where
    w = width options
    h = height options
    cam | length cams > 0 = head cams
        | otherwise       = Camera "" (Vec3 0 0 500) (Vec3 0 1 0) 
                                   (Vec3 0 0.1 (-1)) 0.5 1e-2 1e5 1
    dir x y = (cx &* (((fromIntegral x) / fromIntegral w) - 0.5))
           &+ (cy &* (((fromIntegral y) / fromIntegral h) - 0.5))
           &+ (lookAt cam)
    cx = Vec3 (0.5135 * fromIntegral w / fromIntegral h) 0 0
    cy = normalize (cx &^ (lookAt cam)) &* 0.5135
