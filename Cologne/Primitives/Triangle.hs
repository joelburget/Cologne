{-# OPTIONS_GHC -funbox-strict-fields #-}

module Cologne.Primitives.Triangle where

import Cologne.Vec
import Cologne.Primitives

triangle :: VecD -> VecD -> VecD -> a -> Primitive a
triangle v1 v2 v3 i =
  Primitive (triangleIntersect v1 v2 v3)
            (triangleBound     v1 v2 v3)
            (triangleNormal    v1 v2 v3)
            i

-- Moller-Trumbore triangle intersection
triangleIntersect :: VecD -> VecD -> VecD -> Ray -> Maybe Double
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


triangleBound :: VecD -> VecD -> VecD -> Bbox
triangleBound (VecD x1 y1 z1) (VecD x2 y2 z2) (VecD x3 y3 z3) =
  Bbox (VecD (min x1 $ min x2 x3) (min y1 $ min y2 y3) (min z1 $ min z2 z3))
       (VecD (max x1 $ max x2 x3) (max y1 $ max y2 y3) (max z1 $ max z2 z3))

triangleNormal :: VecD -> VecD -> VecD -> VecD -> VecD
triangleNormal v1 v2 v3 _ = norm $ (v2 |-| v1) `cross` (v3 |-| v1)
