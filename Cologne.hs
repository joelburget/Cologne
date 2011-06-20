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
import System.Console.CmdArgs
import System.IO
import Data.Vect (Vec3(Vec3))
import Data.List (isSuffixOf)
import Data.Char (toLower)
import Data.Array.Repa.IO.DevIL (runIL, writeImage)

import Cologne.Primitives
import Cologne.Accel.List
--import Cologne.Accel.KdTree

import Cologne.Shaders

import Cologne.AssimpImport
import Cologne.ParseNFF

userOptions :: Mode (CmdArgs Options)
userOptions = cmdArgsMode defaultOptions

-- Only outputs if `verbosity` is set to Normal or Loud, not Quiet
putStrLnNormal :: String -> IO ()
putStrLnNormal = whenNormal . putStrLn

main :: IO ()
main = do
  opts <- cmdArgsRun userOptions
  putStrLnNormal $ "Parsing input"
  readInput <- read opts
  case readInput of
    Left err -> putStrLn err
    Right context -> do
      putStrLnNormal "Rendering"
      runIL $ writeImage "image.png" $
        (case ((map toLower) . shader . options) context of
          "debug"   -> debug            -- Notice we're selecting a function and
          _         -> smallpt) context -- applying it to context.
  where
    read :: Options -> IO (Either String ContextType)
    read opts =
      (liftM . liftM) convert $
        if (".col" `isSuffixOf` indata)
        then liftM (flip parseNFF indata) (readFile indata)
        else assimpImport indata
      where convert context = context {options = opts}
            indata = input opts
