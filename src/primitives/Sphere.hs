{-# OPTIONS_GHC -funbox-strict-fields #-}

module Sphere where

data Sphere = Sphere { 
    radius   :: !Double
  , position :: !VecD
  , color    :: Ray -> VecD
  } 

sphereIntersect :: Ray -> Sphere -> Maybe Double
sphereIntersect(Ray o d) s | det < 0 = Nothing
                            | t > eps = Just t
                            | t' > eps = Just t'
                            | otherwise = Nothing
                            where op = position s |-| o -- Solve t^2*d.d + 2*t*(o-p).d + (o-p).(o-p)-R^2 = 0
                                  eps = 1e-4
                                  b = op `dot` d
                                  det = (b * b) - (op `dot` op) + (radius s * radius s)
                                  det' = sqrt det
                                  t = b - det'
                                  t' = b + det'

sphereBound :: Sphere -> Bbox
sphereBound s = Bbox (Vec.fmap (-(radius s)) (position s)) (VecD 2*(radius s) 2*(radius) 2*(radius))

instance Primitive Sphere where
  intersect = sphereIntersect
  bound = sphereBound
  color = undefined
