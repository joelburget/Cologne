{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Cologne.Primitives.Sphere where

import Prelude hiding (fmap)

import Cologne.Vec
import Cologne.Primitives

sphere :: VecD -> Double -> a -> Primitive a
sphere pos rad info =
  Primitive (sphereIntersect pos rad)
            (sphereBound pos rad)
            (sphereNormal pos)
            info

sphereIntersect :: VecD -> Double -> Ray -> Maybe Double
sphereIntersect pos rad (Ray o d)
  | det < 0 = Nothing
  | t > eps = Just t
  | t' > eps = Just t'
  | otherwise = Nothing
  where op = pos |-| o 
        eps = 1e-4
        b = op `dot` d
        det = (b * b) - (op `dot` op) + 
          (rad * rad)
        det' = sqrt det
        t = b - det'
        t' = b + det'

sphereBound :: VecD -> Double -> Bbox
sphereBound pos rad = Bbox 
                  (fmap ((-)rad) pos) 
                  (VecD (2*rad) (2*rad) (2*rad))

sphereNormal :: VecD -> VecD -> VecD
sphereNormal pos vec = norm $ vec |-| pos
