{- A list is the simplest possible acceleration structure. It offers horrible
 - performance. The advantage is that it's very simple. 
 -} 

module List where

instance AccelStruct [] where
  -- insert            :: Primitive -> a -> a
  insert prim xs = prim:xs
  -- intersect         :: Ray -> a -> Intersection
  intersect
  -- listToAccelStruct :: [Primitive] -> a
