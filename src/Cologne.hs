{-# LANGUAGE DeriveDataTypeable #-}

{-
 - Primitives:
 -   Sphere
 -   *Triangle
 -   *Plane
 -
 - Rendering Methods:
 -   Path Tracing
 -   *Bidirectional Path Tracing
 -   *#Metropolis
 -   *Photon Mapping
 -   *Adaptive Sampling
 -   *Progressive Photon Mapping
 -
 - Acceleration Structures
 -   *kd-tree
 -   *bih
 -   *bvh
 -
 - * - Not yest implemented
 -}

module Main where

import Control.Parallel.Strategies(parMap, rwhnf)
import Control.Monad.State
import System.Console.CmdArgs
import Graphics.GD
import System.IO

import Render

spheres :: [Object]
spheres = [Sphere { radius = 1e5,  position = VecD (1+1e5) 40.8 81.6,    emission = VecD 0 0 0,    color = VecD 0.75 0.25 0.25,  refl = DIFF},--Left
           Sphere { radius = 1e5,  position = VecD (99-1e5) 40.8 81.6,   emission = VecD 0 0 0,    color = VecD 0.25 0.25 0.75,  refl = DIFF},--Rght
           Sphere { radius = 1e5,  position = VecD 50 40.8 1e5,          emission = VecD 0 0 0,    color = VecD 0.75 0.75 0.75,  refl = DIFF},--Back
           Sphere { radius = 1e5,  position = VecD 50 40.8 (170-1e5),    emission = VecD 0 0 0,    color = VecD 0 0 0,           refl = DIFF},--Frnt
           Sphere { radius = 1e5,  position = VecD 50 1e5 81.6,          emission = VecD 0 0 0,    color = VecD 0.75 0.75 0.75,  refl = DIFF},--Botm
           Sphere { radius = 1e5,  position = VecD 50 (81.6-1e5) 81.6,   emission = VecD 0 0 0,    color = VecD 0.75 0.75 0.75,  refl = DIFF},--Top
           Sphere { radius = 16.5, position = VecD 27 16.5 47,           emission = VecD 0 0 0,    color = VecD 1 1 1 |*| 0.999, refl = SPEC},--Mirr
           Sphere { radius = 16.5, position = VecD 73 16.5 78,           emission = VecD 0 0 0,    color = VecD 1 1 1 |*| 0.999, refl = REFR},--Glas
           Sphere { radius = 600,  position = VecD 50 (681.6-0.27) 81.6, emission = VecD 12 12 12, color = VecD 0 0 0,           refl = DIFF}] --Lite

context :: Int -> Int -> Int -> [Object] -> Context
context w h samp scene = 
  Context {ctxw = w, ctxh = h, ctxsamp = samp, ctxcx = cx, ctxcy = cy, 
  ctxcampos = VecD 50 52 295.6, ctxcamdir = camdir, ctxscene = scene}
    where camdir = norm (VecD 0 (-0.042612) (-1))
          cx = VecD (0.5135 * fromIntegral w / fromIntegral h) 0 0
          cy = norm (cx `cross` camdir) |*| 0.5135

data Options = Options {
    width   :: Int
  , height  :: Int
  , samples :: Int
  } deriving (Data, Typeable)

options = cmdArgsMode $ Options {
    width   = 100 &= name "x" &= help "Width of image"
  , height  = 100 &= name "y" &= help "Height of image"
  , samples = 100 &= help "Samples"
  } &= summary "Cologne Ray Tracer"

main :: IO ()
main = do
  opts <- cmdArgsRun options
  let x = width opts
      y = height opts
      s = samples opts
  image <- newImage (x,y)
  let output = "image.png"
      ctx = (context x y s spheres)
      colors = parMap rwhnf (line ctx) [1..y]
      setOnePixel y x v = let VecI r g b = vecD2I v in setPixel (x, y) (rgb r g b) image
      setLinePixels (l, y) = zipWithM_ (setOnePixel y) [0..] l
  -- here we have to use stderr (as far as I know) so we can output the progress
  mapM_ (\a -> setLinePixels a >> hPutStr stderr ("\r" ++ show ((fromInteger . toInteger) ((snd a + 1) * 100) / (fromInteger . toInteger) y) ++ "%"))
    (zip colors [0..])
  savePngFile output image
