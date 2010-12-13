{-# LANGUAGE FlexibleInstances #-}
{- A list is the simplest possible acceleration structure. It offers horrible
 - performance. The advantage is that it's very easy to write and understand.
 -} 

module Cologne.Accel.List where

import Cologne.Accel.AccelStruct hiding (intersect)
import Cologne.Accel.AccelStruct as A
import Cologne.Primitives.Primitives hiding (intersect)
import Cologne.Primitives.Primitives as P

instance AccelStruct [Prim] where
  -- insert            :: Primitive -> a -> a
  insert prim xs = prim:xs
  -- intersect         :: a -> Ray -> Intersection
  intersect [] ray = Miss
  intersect (obj:objs) ray = case P.intersect obj ray of
    Miss -> A.intersect objs ray
    i1@(Intersection d1 o1) -> case A.intersect objs ray of
      Miss -> i1
      i2@(Intersection d2 o2) -> case (d1 < d2) of
        True -> i1
        otherwise -> i2
  -- listToAccelStruct :: [Primitive] -> a
  listToAccelStruct = id
