{-# LANGUAGE BangPatterns #-}
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

import Prelude hiding (read)
import Control.Monad (liftM, forM)
import Control.Concurrent (forkIO)
import Control.Concurrent.BoundedChan (newBoundedChan)
import System.Console.CmdArgs
import System.IO
import Data.Vect (Vec3(Vec3))
import Data.List (isSuffixOf)
import Data.Char (toLower)
import System.Exit (exitWith, ExitCode(..))

import Cologne.Primitives
import Cologne.Shaders
import Cologne.Workers
import Cologne.AssimpImport
import Cologne.ParseNFF
import Cologne.OpenGL.Display (display)

userOptions :: Mode (CmdArgs Options)
userOptions = cmdArgsMode defaultOptions

-- Only outputs if `verbosity` is set to Normal or Loud, not Quiet
putStrLnNormal :: String -> IO ()
putStrLnNormal = whenNormal . putStrLn

main :: IO ()
main = do
  opts <- cmdArgsRun userOptions
  putStrLnNormal $ "Parsing input"
  optParse <- read opts
  context <- either 
    (\x -> hPutStrLn stderr x >> exitWith (ExitFailure 65)) 
    return 
    optParse
  tasks   <- newBoundedChan 1000
  results <- newBoundedChan 1000
  let shade = case map toLower . shader . options $ context of
                "smallpt" -> smallpt
                _ -> debug
  workers <- forM [1..(threads . options $ context)] $ const $ forkIO $ 
    worker shade tasks results context
  let w = width  . options $ context
      h = height . options $ context
  fillers <- forkIO $ filler w h tasks
  display w h results
  where
    read :: Options -> IO (Either String ContextType)
    read opts = (liftM . liftM) convert $
        if ".col" `isSuffixOf` indata
        then liftM (flip parseNFF indata) (readFile indata)
        else assimpImport indata
      where convert context = context {options = opts}
            indata = input opts
