{-# OPTIONS_GHC -funbox-strict-fields #-}

module Cologne.Primitives.Triangle where

import Data.Vect (Vec3(Vec3), normalize, (&.), (&^), (&-))

import Cologne.Primitives

triangle :: Vec3 -> Vec3 -> Vec3 -> a -> Primitive a
triangle v1 v2 v3 i =
  Primitive (triangleIntersect v1 v2 v3)
            (triangleBound     v1 v2 v3)
            (triangleNormal    v1 v2 v3)
            i

-- Moller-Trumbore triangle intersection
triangleIntersect :: Vec3 -> Vec3 -> Vec3 -> Ray -> Maybe Float
triangleIntersect x y z (Ray orig dir) =
  let edge1 = y &- x
      edge2 = z &- x
      p = dir &^ edge2
      det = edge1 &. p
      epsilon = 0.000001
      invdet = 1.0 / det
      t = orig &- x
      u = (t &. p) * invdet
  in
    if ((abs det) < epsilon)
    then Nothing
    else
      if (u < 0.0) || (u > 1.0)
      then Nothing
      else
        let q = t &^ edge1
            v = (dir &. q) * invdet
        in
          if (v < 0.0) || (u + v > 1.0)
          then Nothing
          else
            Just $ (edge2 &. q) * invdet


triangleBound :: Vec3 -> Vec3 -> Vec3 -> Bbox
triangleBound (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) (Vec3 x3 y3 z3) =
  Bbox (Vec3 (min x1 $ min x2 x3) (min y1 $ min y2 y3) (min z1 $ min z2 z3))
       (Vec3 (max x1 $ max x2 x3) (max y1 $ max y2 y3) (max z1 $ max z2 z3))

triangleNormal :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3
triangleNormal v1 v2 v3 _ = normalize $ (v2 &- v1) &^ (v3 &- v1)
