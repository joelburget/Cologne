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
import System.Console.CmdArgs
import System.IO
import Data.Lens ((^$!), (^=))
import Data.List (isSuffixOf)
import Data.Char (toLower)
import System.Exit (exitWith, ExitCode(..))

import Cologne.Primitives
import Cologne.Shaders
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
  --let shade = case map toLower . shader . options $ context of
  let shade = case map toLower $ shader ^$! options ^$! context of
                "smallpt" -> smallpt
                _ -> debug
  display context shade
  where
    read :: Options -> IO (Either String ContextType)
    read opts = (fmap . fmap) (options ^= opts) $
        if ".col" `isSuffixOf` indata
        then fmap (flip parseNFF indata) (readFile indata)
        else assimpImport indata
      where indata = input ^$! opts
