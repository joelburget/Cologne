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

import Prelude hiding (mapM_, zip, length)
import Control.Monad hiding (mapM_)
import Control.Monad.State hiding (mapM_)
import System.Console.CmdArgs
import Graphics.GD
import System.IO
import Data.Vector (Vector, mapM_, zip, (!), length)
import Data.Vect (Vec3(Vec3))

import Cologne.Primitives
--import Cologne.Accel.KdTree
import Cologne.Accel.List
import Cologne.Shaders.Smallpt
import Cologne.AssimpImport

saveImage :: Vector (Vector Vec3) -> Image -> FilePath -> IO ()
saveImage picture image imageName = do
  forM_ [0..((length picture)-1)] $ \y -> do
    forM_ [0..((width)-1)] $ \x -> do
      setOnePixel y x ((picture!y)!x)
    tell y (length picture)

  putStrLn ""
  savePngFile imageName  image
  where
    setOnePixel y x (Vec3 r g b) = setPixel (x, y) 
      (rgb (toInt r) (toInt g) (toInt b)) image
    setLinePixels (l, y) = zipWithM_ (setOnePixel y) [0..] l
    width = length (picture ! 0)

tell :: Int -> Int -> IO ()
tell x len = hPutStr stderr ("\r" ++ show (toFloat (x+1) * 100 /
           toFloat len) ++ "%       ")
  where toFloat :: Int -> Float
        toFloat = fromInteger . toInteger

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

main :: IO ()
main = do
  opts <- cmdArgsRun options
  let inputName = oInput opts
  putStrLn "Parsing input"
  context <- assimpImport inputName
  image <- newImage (200,200)
  let picture = generatePicture context
      output = "image.png"
  saveImage picture image output
