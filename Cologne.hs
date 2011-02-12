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
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos

import Cologne.Vec hiding (fmap)
import Cologne.Primitives
import Cologne.Primitives.Sphere
import Cologne.Accel
import Cologne.Accel.List
import Cologne.Render
import Cologne.Shaders.Smallpt
import Cologne.ParseNFF

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

{-
generatePicture :: (AccelStruct a b) =>
                   (a -> Ray -> Int -> State (Halton, Halton) ColorD)
                -> Context a b
                -> [[ColorD]]
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
       line :: State (Int, Int, Halton, Halton) [ColorD]
       line = do
         (x, y, hal1, hal2) <- get
         let (vals, (x', y', h1', h2')) = runState (sequence $ replicate w color') (1, y, hal1, hal2)
         put (x', y'-1, h1', h2')
         return vals
       color' :: State (Int, Int, Halton, Halton) ColorD
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
                   (a -> Ray -> Int -> State Int ColorD)
                -> Context a b
                -> [[ColorD]]
generatePicture color context =
  evalState (sequence $ replicate h line) (1, h, firstRand)
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
       firstRand = 17
       line :: State (Int, Int, Int) [ColorD]
       line = do
         (x, y, rand) <- get
         let (vals, (x', y', nextRand)) = runState (sequence $ replicate w color') (1, y, rand)
         put (x', y'-1, nextRand)
         return vals
       color' :: State (Int, Int, Int) ColorD
       color' = do
         (x, y, rand) <- get
         let (val, nextRand) = runState (sequence $ replicate samp (color (ctxscene context) (ray x y) 0)) rand
         put (x+1, y, nextRand)
         return $ avgColor val
           where
             d x y = (cx |*| (((fromIntegral x) / fromIntegral w) - 0.5))
                 |+| (cy |*| (((fromIntegral y) / fromIntegral h) - 0.5))
                 |+| camdir
             ray x y = Ray (campos |+| ((d x y) |*| 140.0)) (norm (d x y))

data Options = Options {
    width   :: Int
  , height  :: Int
  , input   :: String
  , samples :: Int
  } deriving (Data, Typeable)

options = cmdArgsMode $ Options {
    width   = 100 &= name "x" &= help "Width of image"
  , height  = 100 &= name "y" &= help "Height of image"
  , input  = "cornell.col" &= args &= typ "Input File"
  , samples = 100 &= help "Samples per pixel"
  } &= summary "Cologne Ray Tracer"

main :: IO ()
main = do
  opts <- cmdArgsRun options
  let inputName = input opts --"cornell.col"
  inputContents <- readFile inputName
  putStrLn "Parsing input"
  case parseNFF inputContents inputName of
    Left error -> putStrLn $ show error
    Right ((campos, camdir), scene) -> do
      putStrLn "Input parsed"
      putStrLn "Rendering:"
      let output = "image.png"
          w = width opts
          h = height opts
          cx = VecD (0.5135 * fromIntegral w / fromIntegral h) 0 0
          cy = norm (cx `cross` camdir) |*| 0.5135
          context = Context { 
            ctxw = w
          , ctxh = h
          , ctxsamp = samples opts
          , ctxcx = cx
          , ctxcy = cy
          , ctxcampos = campos
          , ctxcamdir = camdir
          , ctxscene = scene 
          }
          picture = generatePicture radiance context
      image <- newImage (w,h)
      saveImage picture image output 
