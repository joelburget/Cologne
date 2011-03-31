{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Cologne.Primitives.Sphere where

import Graphics.Formats.Assimp (Vec3D, Vec(Vec3D), vmap, (|-|), dot, normalize)
import Cologne.Primitives

sphere :: Vec3D -> Double -> a -> Primitive a
sphere pos rad info =
  Primitive (sphereIntersect pos rad)
            (sphereBound pos rad)
            (sphereNormal pos)
            info

sphereIntersect :: Vec3D -> Double -> Ray -> Maybe Double
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

sphereBound :: Vec3D -> Double -> Bbox
sphereBound pos rad = Bbox 
                  (vmap ((-)rad) pos) 
                  (Vec3D (2*rad) (2*rad) (2*rad))

sphereNormal :: Vec3D -> Vec3D -> Vec3D
sphereNormal pos vec = normalize $ vec |-| pos
