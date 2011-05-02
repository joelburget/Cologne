{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Cologne.Primitives.Sphere where

import Data.Vect (Vec3(Vec3), normalize, mapVec, (&.), (&^), (&-))

import Cologne.Primitives

sphere :: Vec3 -> Float -> a -> Primitive a
sphere pos rad info =
  Primitive (sphereIntersect pos rad)
            (sphereBound pos rad)
            (sphereNormal pos)
            info

sphereIntersect :: Vec3 -> Float -> Ray -> Maybe Float
sphereIntersect pos rad (Ray o d)
  | det < 0 = Nothing
  | t > eps = Just t
  | t' > eps = Just t'
  | otherwise = Nothing
  where op = pos &- o 
        eps = 1e-4
        b = op &. d
        det = (b * b) - (op &. op) + 
          (rad * rad)
        det' = sqrt det
        t = b - det'
        t' = b + det'

sphereBound :: Vec3 -> Float -> Bbox
sphereBound pos rad = Bbox 
                  (mapVec ((-)rad) pos) 
                  (Vec3 (2*rad) (2*rad) (2*rad))

sphereNormal :: Vec3 -> Vec3 -> Vec3
sphereNormal pos vec = normalize $ vec &- pos
