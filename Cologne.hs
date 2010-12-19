{-# LANGUAGE DeriveDataTypeable #-}

{-
 - Primitives:
 -   Sphere
 -   *Triangle
 -   *Plane
 -
 - Rendering Methods:
 -   Path Tracing
 -   *#Bidirectional Path Tracing
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
 - # - Not planning to implement
 -}

module Main where

import Control.Parallel.Strategies(parMap, rwhnf)
--import Control.Monad.State
import Control.Monad
import System.Console.CmdArgs
import Graphics.GD
import System.IO

import Cologne.Vec hiding (fmap)
import Cologne.Primitives
import Cologne.Primitives.Sphere
import Cologne.Accel
import Cologne.Accel.List
import Cologne.Render
import Cologne.Shaders

sphr :: Double -> VecD -> ColorD -> ColorD -> ReflectionType -> Prim
sphr radius center emission color eType =
  Prim $ Sphere radius center
           (objRadiance color emission eType (\v -> norm (v |-| center)))

--{- vista
cen = VecD 50 (-20) (-860)

scene :: [Prim]
scene = listToAccelStruct
  [ sphr 8000 (cen |+| (VecD 0 (-8000) (-900))) ((VecD 1 0.4 0.1) |*| 5e-1) (VecD 0 0 0)  Diffuse
  , sphr 1e4  (cen |+| (VecD 0 0 0))  ((VecD 0.631 0.753 1.00) |*| 3e-1) ((VecD 1 1 1) |*| 0.5) Diffuse

  , sphr 150  (cen |+| (VecD (-350) 0  (-100))) (VecD 0 0 0)  ((VecD 1 1 1) |*| 0.3)  Diffuse
  , sphr 200  (cen |+| (VecD (-210) 0 (-100)))  (VecD 0 0 0)  ((VecD 1 1 1) |*| 0.3)  Diffuse
  , sphr 145  (cen |+| (VecD (-210) 85 (-100))) (VecD 0 0 0)  ((VecD 1 1 1) |*| 0.8)  Diffuse
  , sphr 150  (cen |+| (VecD (-50) 0 (-100)))   (VecD 0 0 0)  ((VecD 1 1 1) |*| 0.3)  Diffuse
  , sphr 150  (cen |+| (VecD 100 0 (-100)))   (VecD 0 0 0)  ((VecD 1 1 1) |*| 0.3)  Diffuse
  , sphr 125  (cen |+| (VecD 250 0 (-100)))   (VecD 0 0 0)  ((VecD 1 1 1) |*| 0.3)  Diffuse
  , sphr 150  (cen |+| (VecD 375 0 (-100)))   (VecD 0 0 0)  ((VecD 1 1 1) |*| 0.3)  Diffuse

  , sphr 2500 (cen |+| (VecD 0 (-2400) (-500))) (VecD 0 0 0)  ((VecD 1 1 1) |*| 0.1)   Diffuse

  , sphr 8000 (cen |+| (VecD 0 (-8000) 200) ) (VecD 0 0 0)  (VecD 0.2 0.2 1)     Refractive
  , sphr 8000 (cen |+| (VecD 0 (-8000) 1100)) (VecD 0 0 0)  (VecD 0 0.3 0)      Diffuse
  , sphr 8    (cen |+| (VecD (-75)  (-5)  850)) (VecD 0 0 0)  (VecD 0 0.3 0)      Diffuse
  , sphr 30   (cen |+| (VecD 0    23  825)) (VecD 0 0 0)  ((VecD 1 1 1) |*| 0.996)  Refractive

  , sphr 30   (cen |+| (VecD 200 280 (-400)))   (VecD 0 0 0)  ((VecD 1 1 1) |*| 0.8)   Diffuse
  , sphr 37   (cen |+| (VecD 237 280 (-400)))   (VecD 0 0 0)  ((VecD 1 1 1) |*| 0.8)   Diffuse
  , sphr 28   (cen |+| (VecD 267 280 (-400)))   (VecD 0 0 0)  ((VecD 1 1 1) |*| 0.8)   Diffuse

  , sphr 40   (cen |+| (VecD 150 280 (-1000)))   (VecD 0 0 0)  ((VecD 1 1 1) |*| 0.8)  Diffuse
  , sphr 37   (cen |+| (VecD 187 280 (-1000)))   (VecD 0 0 0)  ((VecD 1 1 1) |*| 0.8)  Diffuse

  , sphr 40   (cen |+| (VecD 600 280 (-1100)))   (VecD 0 0 0)  ((VecD 1 1 1) |*| 0.8)  Diffuse
  , sphr 37   (cen |+| (VecD 637 280 (-1100)))   (VecD 0 0 0)  ((VecD 1 1 1) |*| 0.8)  Diffuse

  , sphr 37   (cen |+| (VecD (-800) 280 (-1400)))   (VecD 0 0 0)  ((VecD 1 1 1) |*| 0.8)   Diffuse
  , sphr 37   (cen |+| (VecD 0 280 (-1600)))   (VecD 0 0 0)  ((VecD 1 1 1) |*| 0.8)   Diffuse
  , sphr 37   (cen |+| (VecD 537 280 (-1800)))   (VecD 0 0 0)  ((VecD 1 1 1) |*| 0.8)   Diffuse
  ]
--}

{- forest
tc = VecD 0.0588 0.361 0.0941
sc = VecD 0.7 0.7 0.7

scene :: [Prim]
scene = listToAccelStruct
 [ sphr 1e5  (VecD 50 (1e5+130) 0) (VecD 0 0 0) (VecD 1.3 1.3 1.3) Diffuse
 , sphr 1e2  (VecD 50 (-1e2+2) 47) (VecD 0.7 0.7 0.7) (VecD 0 0 0) Diffuse
 , sphr 1e4  ((VecD 50 (-30) 300) |+| (VecD ((negate . sin) (50*pi/180)) 0 (cos (50*pi/180))) |*| 1e4) ((VecD 1 1 1) |*| 0.99) (VecD 0 0 0) Specular
 , sphr 1e4  ((VecD 50 (-30) 300) |+| (VecD (sin (50*pi/180)) 0 (cos (50*pi/180))) |*| 1e4)  ((VecD 1 1 1) |*| 0.99) (VecD 0 0 0) Specular
 , sphr 1e4  ((VecD 50 (-30) (-50)) |+| (VecD ((negate . sin) (30*pi/180)) 0 ((negate . cos) (30*pi/180))) |*| 1e4) ((VecD 1 1 1) |*| 0.99) (VecD 0 0 0) Specular
 , sphr 1e4  ((VecD 50 (-30) (-50)) |+| (VecD (sin (30*pi/180)) 0 ((negate . cos) (30*pi/180))) |*| 1e4) ((VecD 1 1 1) |*| 0.99) (VecD 0 0 0) Specular
 , sphr 4    (VecD 50 (6*0.6) 47)   (VecD 0.13 0.066 0.033) (VecD 0 0 0) Diffuse
 , sphr 16   (VecD 50 (6*2+16*0.6) 47)   tc (VecD 0 0 0)  Diffuse
 , sphr 11   (VecD 50 (6*2+16*0.6*2+11*0.6) 47)   tc (VecD 0 0 0)  Diffuse
 , sphr 7    (VecD 50 (6*2+16*0.6*2+11*0.6*2+7*0.6) 47)   tc (VecD 0 0 0)  Diffuse
 , sphr 15.5 (VecD 50 (1.8+6*2+16*0.6) 47)   sc (VecD 0 0 0)  Diffuse
 , sphr 10.5 (VecD 50 (1.8+6*2+16*0.6*2+11*0.6) 47)   sc (VecD 0 0 0)  Diffuse
 , sphr 6.5  (VecD 50 (1.8+6*2+16*0.6*2+11*0.6*2+7*0.6) 47)   sc (VecD 0 0 0)  Diffuse
 ]
--}

{- cornell box
scene :: [Prim]
scene = listToAccelStruct
  [  sphr 1e5  (VecD (1+1e5) 40.8 81.6) (VecD 0.75 0.25 0.25) (VecD 0 0 0) Diffuse
  ,  sphr 1e5  (VecD (99-1e5) 40.8 81.6) (VecD 0.25 0.25 0.75) (VecD 0 0 0) Diffuse
  ,  sphr 1e5  (VecD 50 40.8 1e5) (VecD 0.75 0.75 0.75) (VecD 0 0 0) Diffuse
  ,  sphr 1e5  (VecD 50 40.8 (170-1e5)) (VecD 0 0 0) (VecD 0 0 0) Diffuse
  ,  sphr 1e5  (VecD 50 1e5 81.6) (VecD 0.75 0.75 0.75) (VecD 0 0 0) Diffuse
  ,  sphr 1e5  (VecD 50 (81.6-1e5) 81.6) (VecD 0.75 0.75 0.75) (VecD 0 0 0) Diffuse
  ,  sphr 16.5 (VecD 27 16.5 47) (VecD 0.999 0.999 0.999) (VecD 0 0 0) Specular
  ,  sphr 16.5 (VecD 73 16.5 78) (VecD 0.999 0.999 0.999) (VecD 0 0 0) Refractive
  ,  sphr 600  (VecD 50 (681.6-0.27) 81.6) (VecD 0 0 0) (VecD 12 12 12) Diffuse
  ]
--}

saveImage :: [[ColorD]] -> Image -> FilePath -> IO ()
saveImage picture image name = do
  mapM_ (\a -> setLinePixels a >> tell (snd a) (length picture)) (zip picture [0..])
  putStrLn ""
  savePngFile name image
  where
    setOnePixel y x v = let VecI r g b = vecD2I v 
                        in setPixel (x, y) (rgb r g b) image
    setLinePixels (l, y) = zipWithM_ (setOnePixel y) [0..] l

tell :: Int -> Int -> IO ()
tell x len = hPutStr stderr ("\r" ++ show ((fromInteger . toInteger) (x+1) * 100 / 
           (fromInteger . toInteger) len) ++ "%       ")

generatePicture :: (AccelStruct a) =>
                   (a -> Ray -> Int -> Int -> ColorD) 
                -> Context a
                -> Int
                -> [[ColorD]]
generatePicture color context rand =
  [line y | y <- [h,h-1..1]]
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
          color' x y = 
            avgColor [color (ctxscene context) 
                            ray 
                            0  -- The depth we start from
                            (rand * 2713 * x * y * s) -- Make this
                            -- number as random as possible, without using the 
                            -- State monad, which is where I'm going next (TODO)
                              | s <- [1..samp]]
            where
              d =   (cx |*| (((fromIntegral x) / fromIntegral w) - 0.5))
                |+| (cy |*| (((fromIntegral y) / fromIntegral h) - 0.5))
                |+| camdir
              ray = Ray (campos |+| (d |*| 140.0)) (norm d)
              -- TODO: make a specialized avgColor function
              avgColor xs = VecD (avg (map (dot (VecD 1 0 0)) xs))
                                 (avg (map (dot (VecD 0 1 0)) xs))
                                 (avg (map (dot (VecD 0 0 1)) xs))

avg :: [Double] -> Double
avg xs = (sum xs) / ((fromInteger . toInteger) (length xs))

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
      picture = generatePicture radiance context 19
  image <- newImage (w,h)
  saveImage picture image output 
