{- This module only defines the AccelStruct typeclass, which is used to store
 - and interact with the primitives of a scene. This can result in huge
 - speedups because rather than checking for intersection with every primitive
 - in a scene (O(n)) we can reduce the complexity to O(lg(n)).
 -}

module AccelStruct where

class AccelStruct a where
  insert            :: Primitive -> a -> a
  intersect         :: Ray -> a -> Intersection
  listToAccelStruct :: [Primitive] -> a

data Accel = forall a. AccelStruct a => Accel a
