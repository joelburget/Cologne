{-# LANGUAGE ExistentialQuantification #-}
{- This module only defines the AccelStruct typeclass, which is used to store
 - and interact with the primitives of a scene. This can result in huge
 - speedups because rather than checking for intersection with every primitive
 - in a scene (O(n)) we can reduce the complexity to O(lg(n)).
 -}

module Cologne.Accel.AccelStruct where

import Cologne.Primitives.Primitives hiding (intersect)
--import Cologne.Primitives.Primitives as P

class AccelStruct a where
  insert            :: Prim -> a -> a
  intersect         :: a -> Ray -> Intersection
  listToAccelStruct :: [Prim] -> a

-- data Accel = forall a. AccelStruct a => Accel a
-- 
-- instance AccelStruct Accel where
--   insert prim (Accel a)           = insert prim (Accel a)
--   intersect (Accel a) ray         = intersect a ray
--   listToAccelStruct lst   = listToAccelStruct lst
