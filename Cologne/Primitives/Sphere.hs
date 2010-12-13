{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE ExistentialQuantification #-}

module Cologne.Primitives.Sphere where

import Cologne.Vec
import Cologne.Vec as Vec
import Cologne.Primitives.Primitives

data Sphere = Sphere { 
    sphereRadius   :: !Double
  , spherePosition :: !VecD
  , sphereColor    :: Ray -> Int -> Int -> VecD
  } 

sphereIntersect :: Sphere -> Ray -> Intersection
sphereIntersect s r = case sphereIntersect' s r of
  Just d -> Intersection d (Prim s)
  _      -> Miss

sphereIntersect' :: Sphere -> Ray -> Maybe Double
sphereIntersect' s (Ray o d) | det < 0 = Nothing
                             | t > eps = Just t
                             | t' > eps = Just t'
                             | otherwise = Nothing
                             where op = spherePosition s |-| o -- Solve t^2*d.d + 2*t*(o-p).d + (o-p).(o-p)-R^2 = 0
                                   eps = 1e-4
                                   b = op `dot` d
                                   det = (b * b) - (op `dot` op) + (sphereRadius s * sphereRadius s)
                                   det' = sqrt det
                                   t = b - det'
                                   t' = b + det'

sphereBound :: Sphere -> Bbox
sphereBound s = Bbox 
                  (Vec.fmap ((-)(sphereRadius s)) (spherePosition s)) 
                  (VecD (2*(sphereRadius s)) (2*(sphereRadius s)) (2*(sphereRadius s)))

instance Primitive Sphere where
  intersect = sphereIntersect
  bound = sphereBound
  color sph ray 0 _ = VecD 0 0 0
  color sph ray depth rand = (sphereColor sph) ray depth rand
