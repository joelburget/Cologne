{-# OPTIONS_GHC -funbox-strict-fields #-}

module Cologne.Primitives.Triangle where

import Graphics.Formats.Assimp (Vec3D, Vec(Vec3D), (|-|), dot, cross, normalize)
import Cologne.Primitives

triangle :: Vec3D -> Vec3D -> Vec3D -> a -> Primitive a
triangle v1 v2 v3 i =
  Primitive (triangleIntersect v1 v2 v3)
            (triangleBound     v1 v2 v3)
            (triangleNormal    v1 v2 v3)
            i

-- Moller-Trumbore triangle intersection
triangleIntersect :: Vec3D -> Vec3D -> Vec3D -> Ray -> Maybe Double
triangleIntersect x y z (Ray orig dir) =
  let edge1 = y |-| x
      edge2 = z |-| x
      p = dir `cross` edge2
      det = edge1 `dot` p
      epsilon = 0.000001
      invdet = 1.0 / det
      t = orig |-| x
      u = (t `dot` p) * invdet
  in
    if ((abs det) < epsilon)
    then Nothing
    else
      if (u < 0.0) || (u > 1.0)
      then Nothing
      else
        let q = t `cross` edge1
            v = (dir `dot` q) * invdet
        in
          if (v < 0.0) || (u + v > 1.0)
          then Nothing
          else
            Just $ (edge2 `dot` q) * invdet


triangleBound :: Vec3D -> Vec3D -> Vec3D -> Bbox
triangleBound (Vec3D x1 y1 z1) (Vec3D x2 y2 z2) (Vec3D x3 y3 z3) =
  Bbox (Vec3D (min x1 $ min x2 x3) (min y1 $ min y2 y3) (min z1 $ min z2 z3))
       (Vec3D (max x1 $ max x2 x3) (max y1 $ max y2 y3) (max z1 $ max z2 z3))

triangleNormal :: Vec3D -> Vec3D -> Vec3D -> Vec3D -> Vec3D
triangleNormal v1 v2 v3 _ = normalize $ (v2 |-| v1) `cross` (v3 |-| v1)
