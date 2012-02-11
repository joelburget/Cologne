{-# LANGUAGE BangPatterns #-}
module Cologne.Workers (
    TaskMessage(..)
  , ResultMessage(..)
  , Shader
  , worker
  , sequentialFill
  , randomFill
  ) where

import System.Random.MWC
import Control.Concurrent.BoundedChan (readChan, writeChan, BoundedChan)
import Control.Monad (forever, forM_)
import Data.Vect (Vec3(Vec3))

import Cologne.Primitives

data TaskMessage = TaskPixel !Int !Int !Seed
data ResultMessage = ResultPixel !Int !Int !Vec3
type Shader a b = Context a b -> Int -> Int -> Seed -> Vec3

worker :: AccelStruct a b
       => Context a b
       -> Shader a b
       -> BoundedChan TaskMessage
       -> BoundedChan ResultMessage
       -> IO ()
worker context shader tasks results = forever $ do
  TaskPixel x y seed <- readChan tasks
  let result = shader context x y seed
  writeChan results $ ResultPixel x y result

sequentialFill :: Int -> Int
               -> BoundedChan TaskMessage
               -> IO ()
sequentialFill width height tasks = do
  gen <- create
  seed <- save gen
  forever $
    forM_ [0..width-1] $ \x ->
      forM_ [0..height-1] $ \y -> do
        writeChan tasks $ TaskPixel x y seed -- everyone gets the same seed (gasp!)

randomFill :: Int -> Int
           -> BoundedChan TaskMessage
           -> IO ()
randomFill width height tasks = do
  gen <- create
  forever $ do
    (x, y) <- uniformR ((0, 0), (width-1, height-1)) gen
    seed <- save gen -- might need to deepseq this
    writeChan tasks $ TaskPixel x y seed
