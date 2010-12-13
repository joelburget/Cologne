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
 - * - Not yet implemented
 -}

module Main where

import Control.Parallel.Strategies(parMap, rwhnf)
import Control.Monad.State
import System.Console.CmdArgs
import Graphics.GD
import System.IO

import Cologne.Vec
import Cologne.Primitives.Primitives
import Cologne.Primitives.Sphere
import Cologne.Accel.AccelStruct
import Cologne.Accel.List
import Cologne.Render

scene :: [Prim]
scene = listToAccelStruct
          [  Prim $ Sphere 1e5  (VecD (1+1e5) 40.8 81.6)    (\r i j -> VecD 0.75 0.25 0.25)
          ,  Prim $ Sphere 1e5  (VecD (99-1e5) 40.8 81.6)   (\r i j -> VecD 0.25 0.25 0.75) --Rght
          ,  Prim $ Sphere 1e5  (VecD 50 40.8 1e5)          (\r i j -> VecD 0.75 0.75 0.75) --Back
          ,  Prim $ Sphere 1e5  (VecD 50 40.8 (170-1e5))    (\r i j -> VecD 0 0 0)          --Frnt
          ,  Prim $ Sphere 1e5  (VecD 50 1e5 81.6)          (\r i j -> VecD 0.75 0.75 0.75) --Botm
          ,  Prim $ Sphere 1e5  (VecD 50 (81.6-1e5) 81.6)   (\r i j -> VecD 0.75 0.75 0.75) --Top
          ,  Prim $ Sphere 16.5 (VecD 27 16.5 47)           (\r i j -> VecD 1 1 1 |*| 0.999)--Mirr
          ,  Prim $ Sphere 16.5 (VecD 73 16.5 78)           (\r i j -> VecD 1 1 1 |*| 0.999)--Glas
          ,  Prim $ Sphere 600  (VecD 50 (681.6-0.27) 81.6) (\r i j -> VecD 0 0 0)           --Lite
          ]

context :: (AccelStruct a) => Int -> Int -> Int -> a -> Context a
context w h samp scene = 
  Context {ctxw = w, ctxh = h, ctxsamp = samp, ctxcx = cx, ctxcy = cy, 
  ctxcampos = VecD 50 52 295.6, ctxcamdir = camdir, ctxscene = scene}
    where camdir = norm (VecD 0 (-0.042612) (-1))
          cx = VecD (0.5135 * fromIntegral w / fromIntegral h) 0 0
          cy = norm (cx `cross` camdir) |*| 0.5135

saveImage :: [[ColorD]] -> Image -> FilePath -> IO ()
saveImage picture image name = do
  mapM_ (\a -> setLinePixels a) (zip picture [0..])
  savePngFile name image
  where
    setOnePixel y x v = let VecI r g b = vecD2I v 
                        in setPixel (x, y) (rgb r g b) image
    setLinePixels (l, y) = zipWithM_ (setOnePixel y) [0..] l

tell :: Int -> Int -> IO ()
tell x len = hPutStr stderr ("\r" ++ show ((fromInteger . toInteger) x * 100 / 
           (fromInteger . toInteger) len) ++ "%       ")

generatePicture :: (AccelStruct a) =>
                   (a -> Ray -> Int -> Int -> ColorD) 
                -> Context a
                -> Int
                -> [[ColorD]]
generatePicture color context rand =
  [line y | y <- [1..h]]
    where line y = [color' x y | x <- [1..w]]
          Context { 
            ctxw = w 
          , ctxh = h
          , ctxsamp = samp
          , ctxcx = cx
          , ctxcy = cy
          , ctxcamdir = camdir
          , ctxcampos = campos
          , ctxscene = scene
          } = context
          color' x y = color (ctxscene context) ray 0 rand
            where
              d =   (cx |*| (((fromIntegral x) / fromIntegral w) - 0.5))
                |+| (cy |*| (((fromIntegral y) / fromIntegral h) - 0.5))
                |+| camdir
              ray = Ray (campos |+| (d |*| 140.0)) (norm d)

data Options = Options {
    width   :: Int
  , height  :: Int
  , samples :: Int
  } deriving (Data, Typeable)

options = cmdArgsMode $ Options {
    width   = 100 &= name "x" &= help "Width of image"
  , height  = 100 &= name "y" &= help "Height of image"
  , samples = 100 &= help "Samples per pixel"
  } &= summary "Cologne Ray Tracer"

main :: IO ()
main = do
  opts <- cmdArgsRun options
  let output = "image.png"
      w = width opts
      h = height opts
      camdir = norm (VecD 0 (-0.042612) (-1))
      cx = VecD (0.5135 * fromIntegral w / fromIntegral h) 0 0
      cy = norm (cx `cross` camdir) |*| 0.5135
      context = Context { 
        ctxw = w
      , ctxh = h
      , ctxsamp = samples opts
      , ctxcx = cx
      , ctxcy = cy
      , ctxcampos = VecD 50 52 295.6
      , ctxcamdir = camdir
      , ctxscene = scene 
      }
      -- replace 0 with a random number later
      picture = generatePicture radiance context 0
  image <- newImage (w,h)
  saveImage picture image output 
