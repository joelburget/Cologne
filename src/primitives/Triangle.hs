import Vec
import Primitives

data Triangle = Triangle !Int !Int !Int (Ray -> ColorD) deriving (Data, Typeable)

-- Moller-Trumbore triangle intersection
triIntersect :: Triangle -> Ray -> Intersection
triIntersect tri@(Triangle x y z) (Ray orig dir) =
  let edge1 = y |-| x
      edge2 = z |-| x
      p = dir `cross` edge2
      det = edge1 `dot` p
      epsilon = 0.000001
  in
    if ((abs det) < epsilon)
    then Miss
    else
      let invdet = 1.0 / det
      t = orig |-| x
      u = (t `dot` p) * invdet
      if (u < 0.0) || (u > 1.0)
      then Miss
      else
        let q = t `cross` edge1
            v = (dir `dot` q) * invdet
        in
          if (v < 0.0) || (u + v > 1.0)
          then Miss
          else
            Intersection ((edge2 `dot` q) * invdet) tri


triBound :: Triangle -> Bbox
triBound = undefined

instance Primitive Triange where
  intersect = triIntersect
  bound = triBound
  color = undefined
