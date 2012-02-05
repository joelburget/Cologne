{-# LANGUAGE BangPatterns #-}
module Cologne.Workers (
    Task
  , Result
  , Shader
  , worker
  , filler
  ) where

import System.Random.MWC
import Control.Concurrent.BoundedChan (readChan, writeChan, BoundedChan)
import Control.Monad (forever, forM_)
import Data.Vect (Vec3(Vec3))

import Cologne.Primitives

type Task = (Int, Int, Seed)
type Result = (Int, Int, Vec3)
type Shader a b = Context a b -> Int -> Int -> Seed -> Vec3

worker :: AccelStruct a b
       => Shader a b
       {--> Lens
       -> Camera-}
       -> BoundedChan (Int, Int, Seed)
       -> BoundedChan (Int, Int, Vec3)
       -> Context a b
       -> IO ()
worker shader {-lens cam-} tasks results context = forever $ do
  (x, y, seed) <- readChan tasks
  let !result = shader context x y seed
  writeChan results (x, y, result)

sequentialFill :: Int -> Int
               -> BoundedChan (Int, Int, Seed)
               -> IO ()
sequentialFill width height tasks = do
  gen <- create
  !seed <- save gen
  forM_ [0..width-1] $ \x ->
    forM_ [0..height-1] $ \y -> do
      writeChan tasks (x, y, seed) -- everyone gets the same seed (gasp!)

randomFill :: Int -> Int
           -> BoundedChan (Int, Int, Seed)
           -> IO ()
randomFill width height tasks = do
  gen <- create
  forever $ do
    (x, y) <- uniformR ((0, 0), (width-1, height-1)) gen
    !seed <- save gen -- might need to deepseq this
    writeChan tasks (x, y, seed)
