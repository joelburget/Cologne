{-# LANGUAGE FlexibleInstances #-}
{- A list is the simplest possible acceleration structure. It offers horrible
 - performance. The advantage is that it's very easy to write and understand.
 -} 

module Cologne.Accel.List where

--import Cologne.Accel.AccelStruct hiding (intersect)
--import Cologne.Accel.AccelStruct as A
import Cologne.Primitives --hiding (intersect)
import Cologne.Primitives as P

instance AccelStruct [Prim] where
  insert prim xs = prim:xs

  aIntersect [] ray = Miss
  aIntersect (obj:objs) ray = case P.intersect obj ray of
    Miss -> aIntersect objs ray
    i1@(Intersection d1 o1) -> case aIntersect objs ray of
      Miss -> i1
      i2@(Intersection d2 o2) -> case (d1 < d2) of
        True -> i1
        otherwise -> i2

  -- In this case a list of primitives is already the acceleration structure
  -- we're converting to, so we do nothing
  listToAccelStruct = id
