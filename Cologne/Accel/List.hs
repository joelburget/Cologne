{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- A list is the simplest possible acceleration structure. It offers horrible
 - performance. The advantage is that it's very easy to write and understand.
 -} 

module Cologne.Accel.List where

import Cologne.Vec
import Cologne.Primitives

instance AccelStruct [Primitive a] a where
  --insert :: Primitive a -> [Primitive a] -> [Primitive a]
  insert prim xs = prim:xs

  --aIntersect :: a -> Ray -> Intersection b
  aIntersect [] ray = Miss
  aIntersect (obj:objs) ray@(Ray o d) = case intersect obj ray of
    Nothing -> aIntersect objs ray
    Just d1 -> case aIntersect objs ray of
      Miss -> i1
      i2@(Intersection d2 _ _) -> case (d1 < d2) of
        True -> i1
        _ -> i2
      where i1 = Intersection d1 (colorInfo obj) (normal obj (o |+| (d |*| d1)))

  -- In this case a list of primitives is already the acceleration structure
  -- we're converting to, so we do nothing
  listToAccelStruct = id
