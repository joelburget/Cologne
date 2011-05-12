{-
 - Primitives:
 -   Sphere
 -   Triangle
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

import Prelude hiding (mapM_, zip, length, read)
import Control.Monad hiding (mapM_)
import Control.Monad.State hiding (mapM_)
import System.Console.CmdArgs
import Graphics.GD
import System.IO
import Data.Vector (Vector, mapM_, zip, (!), length)
import Data.Vect (Vec3(Vec3))
import Data.List (isSuffixOf)
import Data.Char (toLower)

import Cologne.Primitives
import Cologne.Accel.List

import Cologne.Shaders

import Cologne.AssimpImport
import Cologne.ParseNFF

saveImage :: Image -> FilePath -> Vector (Vector Vec3) -> IO ()
saveImage image imageName picture = do
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
tell x len = whenNormal $ hPutStr stderr ("\r" ++ show (toFloat (x+1) * 100 /
           toFloat len) ++ "%       ")
  where toFloat :: Int -> Float
        toFloat = fromInteger . toInteger

userOptions :: Mode (CmdArgs Options)
userOptions = cmdArgsMode defaultOptions

putStrLnNormal :: String -> IO ()
putStrLnNormal = whenNormal . putStrLn

main :: IO ()
main = do
  opts@(Options w h _ _ _) <- cmdArgsRun userOptions
  putStrLnNormal $ "Parsing input"
  readInput <- read opts
  case readInput of
    Left err -> putStrLn err
    Right context -> do
      putStrLnNormal "Rendering"
      image <- newImage (w, h)
      saveImage image "image.png" $ 
        (case ((map toLower) . shader . options) context of
          "smallpt" -> smallpt
          --"debug" -> debug    -- Notice we're selecting a function and 
          _ -> smallpt) context -- applying it to context.
  where
    read opts@(Options _ _ _ input _) = 
      if (".col" `isSuffixOf` input)
      then parseNFF' input opts
      else assimpImport input
    parseNFF' input (Options w h s _ sr) = do
      inputContents <- readFile input
      return $ case parseNFF inputContents input of
        Left err -> Left err
        Right (Context _ cams objs) -> return $ 
          Context (Options w h s input sr) cams objs
