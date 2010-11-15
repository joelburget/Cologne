import Vec
import Primitives

data Triangle = Triangle !Int !Int !Int (Ray -> ColorD) deriving (Data, Typeable)

-- Moller-Trumbore triangle intersection
triIntersect :: Triangle -> Ray -> Intersection
triIntersect (Triangle x y z) (Ray orig dir) =
  let edge1 = 

triBound :: Triangle -> Bbox
triBound = undefined

instance Primitive Triange where
  intersect = triIntersect
  bound = triBound
  color = triColor
