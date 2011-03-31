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

import Control.Monad
import Control.Monad.State
import System.Console.CmdArgs
import Graphics.GD
import System.IO
import Graphics.Formats.Assimp (Vec(Vec3I, Vec3D), Color3D, dot, (|*|), 
                                avgColor, len, toInt)

import Cologne.Primitives
import Cologne.Accel.KdTree
--import Cologne.Accel.List
import Cologne.Shaders.Smallpt
import Cologne.AssimpImport

saveImage :: [[Color3D]] -> Image -> FilePath -> IO ()
saveImage picture image imageName = do
  mapM_ (\a -> setLinePixels a >> tell (snd a) (length picture)) (zip picture [0..])
  putStrLn ""
  savePngFile imageName  image
  where
    setOnePixel y x (Vec3D r g b) = setPixel (x, y) 
      (rgb (toInt r) (toInt g) (toInt b)) image
    setLinePixels (l, y) = zipWithM_ (setOnePixel y) [0..] l

tell :: Int -> Int -> IO ()
tell x len = hPutStr stderr ("\r" ++ show (toFloat (x+1) * 100 /
           toFloat len) ++ "%       ")
  where toFloat :: Int -> Float
        toFloat = fromInteger . toInteger

{-
generatePicture :: (AccelStruct a b) =>
                   (a -> Ray -> Int -> State (Halton, Halton) Color3D)
                -> Context a b
                -> [[Color3D]]
generatePicture color context =
  evalState (sequence $ replicate h line) (1, h, hal1, hal2)
     where
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
       hal1 = halton 0 2
       hal2 = halton 0 2
       line :: State (Int, Int, Halton, Halton) [Color3D]
       line = do
         (x, y, hal1, hal2) <- get
         let (vals, (x', y', h1', h2')) = runState (sequence $ replicate w color') (1, y, hal1, hal2)
         put (x', y'-1, h1', h2')
         return vals
       color' :: State (Int, Int, Halton, Halton) Color3D
       color' = do
         (x, y, hal1, hal2) <- get
         let (val, (hal1', hal2')) = runState (sequence $ replicate samp (color (ctxscene context) (ray x y) 0)) (hal1, hal2)
         put (x+1, y, hal1', hal2')
         return $ avgColor val
           where
             d x y = (cx |*| (((fromIntegral x) / fromIntegral w) - 0.5))
                 |+| (cy |*| (((fromIntegral y) / fromIntegral h) - 0.5))
                 |+| camdir
             ray x y = Ray (campos |+| ((d x y) |*| 140.0)) (norm (d x y))
-}

generatePicture :: (AccelStruct a b) =>
                   (a -> Ray -> Int -> State Int Color3D)
                -> Context a b
                -> [[Color3D]]
generatePicture color (Context w h samp cams scene) =
  evalState (sequence $ replicate h line) (1, h, firstRand)
     where
       firstRand = 17
       line :: State (Int, Int, Int) [Color3D]
       line = do
         (_, y, rand) <- get
         let (vals, (x', y', nextRand)) = runState (sequence $ replicate w color') (1, y, rand)
         put (x', y'-1, nextRand)
         return vals
       color' :: State (Int, Int, Int) Color3D
       color' = do
         (x, y, rand) <- get
         let (val, nextRand) = runState (sequence $ replicate samp (color scene (ray x y) 0)) rand
         put (x+1, y, nextRand)
         return $ avgColor val
           where
             -- d x y = (cx |*| (((fromIntegral x) / fromIntegral w) - 0.5))
             --     |+| (cy |*| (((fromIntegral y) / fromIntegral h) - 0.5))
             --     |+| camdir
             -- ray x y = Ray (campos |+| ((d x y) |*| 140.0)) (norm (d x y))
             d = undefined
             ray = undefined

data Options = Options {
    oWidth   :: Int
  , oHeight  :: Int
  , oInput   :: String
  , oSamples :: Int
  } deriving (Data, Typeable)

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options {
    oWidth   = 100 &= name "x" &= help "Width of image"
  , oHeight  = 100 &= name "y" &= help "Height of image"
  , oInput   = "cornell.col"   &= args &= typ "Input File"
  , oSamples = 100 &= help "Samples per pixel"
  } &= summary "Cologne Ray Tracer"

radiance' scene ray depth = do
  case aIntersect scene ray of
    Miss -> return $ Vec3D 0 0 0
    Intersection t (color, emission, rtype) nrm -> 
      return $ color |*| (abs (((direction ray) `dot` nrm) / ((len (direction ray)) * (len nrm))))

main :: IO ()
main = do
  opts <- cmdArgsRun options
  let inputName = oInput opts
  putStrLn "Parsing input"
  context <- assimpImport inputName
  image <- newImage (200,200)
  let picture = generatePicture radiance' context
      output = "image.png"
  saveImage picture image output
