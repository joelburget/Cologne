{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Cologne.Accel.KdTree where
{- This code could use a lot of optimizations. The main problem is the way it
 - builds the tree. It just splits the objects in the middle. Ideally we would
 - use a heuristic, eg binned SAH.
 -}

import Data.List (sortBy)

import Cologne.Vec
import Cologne.Primitives
import Data.List (foldl1')
import Cologne.Accel.List

data KdTree a =
    Leaf Bbox Dimension [a]
  | Node Bbox Dimension (KdTree a) (KdTree a)

instance Show (KdTree a) where
  show (Leaf _ _ _)   = "leaf"
  show (Node _ _ a b) = "(node " ++ (show a) ++ " " ++ (show b) ++ ")"

maxItems = 50 -- The most items we'll store in a leaf before splitting it

xyz :: Dimension -> VecD -> Double
xyz X (VecD x _ _) = x
xyz Y (VecD _ y _) = y
xyz Z (VecD _ _ z) = z

-- We're using a poor binning heuristic
-- TODO: There was a stack overflow when I set maxItems to 2, indicating a bug
makeKd :: [Primitive a] -> Dimension -> KdTree (Primitive a)
makeKd xs dim 
  | length xs < maxItems = Leaf (bbox xs) dim xs
  | otherwise = Node (bbox xs) dim 
                  (makeKd (fst split) (nextDim dim)) 
                  (makeKd (snd split) (nextDim dim))
  where
  --split :: ([Primitive a], [Primitive a])
  split = splitAt (mid xs) $ sortBy less xs
  mid xs = (length xs) `div` 2
  less a b
    | (xyz dim $ (middle . bound) a) < (xyz dim $ (middle . bound) b) = LT
    | otherwise = GT
  middle :: Bbox -> VecD
  middle box = avgVec (start box) (stop box)
  avgVec v1 v2 = (v1 |+| v2) |*| 0.5

instance AccelStruct (KdTree (Primitive a)) a where
  --insert :: Primitive a -> KdTree (Primitive a) -> KdTree (Primitive a)
  insert prim (Leaf box dim xs)
    | length xs > maxItems = makeKd xs (nextDim dim)
    | otherwise = Leaf box dim (prim:xs)
  insert prim (Node box dim left right) = 
    if (xyz dim $ start . bound $ prim) < (xyz dim $ start box)
    then Node box dim (insert prim left) right
    else Node box dim left (insert prim right)

  --aIntersect :: KdTree (Primitive a) -> Ray -> Intersection a
  aIntersect (Leaf box _ xs) ray
    | bboxIntersect box ray == False = Miss
    | otherwise = aIntersect xs ray
  aIntersect (Node box _ left right) ray
    | bboxIntersect box ray == False = Miss
    -- I'm pattern matching here to avoid making (Intersection a) an instance
    -- of Eq.
    | otherwise = case (leftIsect, rightIsect) of
      (Miss, r) -> r
      (l, Miss) -> l
      (l,    r) -> if (dist l) < (dist r) then l else r
      where leftIsect  = aIntersect left ray
            rightIsect = aIntersect right ray

  --listToAccelStruct :: [Primitive a] -> KdTree (Primitive a)
  listToAccelStruct [] = Leaf (Bbox (VecD 0 0 0) (VecD 0 0 0)) X []
  listToAccelStruct ls = makeKd ls X
