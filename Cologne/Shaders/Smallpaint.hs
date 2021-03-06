{-# LANGUAGE FlexibleContexts #-}

-- This is a rewrite of smallpaint: http://keer0y.luminarium.hu/

module Cologne.Shaders.Smallpaint (
    smallpaint
  , Halton(Halton)
  , halton
  , next
  , hGet
  ) where

import System.Random hiding (next)
import Control.Monad.State
import Control.Applicative ((<$>))

import Cologne.Vec
import Cologne.Primitives hiding (intersect)

-- We use this data structure to generate a halton sequence, the same way as in
-- the original smallpaint.
data Halton = Halton Double Double deriving (Show)

-- for the time being assume i == 0
halton :: Int -> Int -> Halton
halton i base = Halton 0.0 (1.0 / ((fromInteger . toInteger) base))

next :: Halton -> Halton
next (Halton value inv_base) =
  if inv_base < r
    then (Halton (value + inv_base) inv_base)
    else (Halton (value + f) inv_base)
      where
        r = 1.0 - value - 0.0000001
        f = (fhh inv_base (inv_base*inv_base)) - 1.0
        fhh :: Double -> Double -> Double
        fhh hh h = if h < r
                     then hh + h
                     else fhh h (h * inv_base)

hGet :: Halton -> Double
hGet (Halton value inv_base) = value

-- To compute the radiance of a ray shot into the scene we check to see if the
-- ray intersects an object. If it misses all objects we return black, if it
-- hits, we return the color returned by the object for that particular ray.
smallpaint :: (AccelStruct a (VecD, VecD, ReflectionType)) => 
             a -> 
             Ray -> 
             Int -> 
             State (Halton, Halton) ColorD
smallpaint scene ray depth = do
  (hal1, hal2) <- get
  case aIntersect scene ray of
    Miss -> return $ VecD 0 0 0
    Intersection t (color, emission, rtype) nrm -> objRadiance
      where
        objRadiance
          | depth >= 20 = return $ VecD 0 0 0
          | otherwise = do
              c <- clr rtype
              return $ (emission |*| 2) |+| c
        Ray raypos raydir = ray
        hp = raypos |+| (raydir |*| t)    -- hit point
        n = nrm                           -- normal at intersection
        clr Diffuse =
          let
            hal1' = next hal1
            hal2' = next hal2
            rayd = n |+| hemisphere (hGet hal1') (hGet hal2')
            -- the cost decreases the further the reflectant ray gets from
            -- straight out
            cost = rayd `dot` n
            -- hemisphere returns a direction inside the hemisphere based on its
            -- two parameters
            hemisphere u1 u2 =
              let r = sqrt $ 1.0 - u1*u1
                  phi = 2 * pi * u2
              in
                VecD ((cos phi) * r) ((sin phi) * r) u1
          in do
            put (hal1', hal2')
            (\x -> (x `vmult` color) |*| (cost * 0.1)) <$> smallpaint scene (Ray hp rayd) (depth + 1)
        clr Specular = 
          let
            reflRay = Ray hp (raydir |-| (n |*| (2 * (n `dot` raydir))))
          in
            smallpaint scene reflRay (depth + 1)
        clr Refractive = 
          let
            refrIndex = 1.9
            -- (n `dot` raydir) < 0  if the ray is going into the object. If the
            -- ray is going out of the object (coming from inside) we flip the
            -- normal.
            (nl, ri) | (n `dot` raydir) < 0 = (n, 1/refrIndex)
                     | otherwise = (n |*| (-1), refrIndex)
            cosin = (nl `dot` raydir) * (-1)
            cost2 = 1.0 - ri*ri*(1.0-cosin*cosin)
            rayd = norm $ -- the direction of the refractive ray
              (raydir |*| ri) |+| (nl |*| (ri*cosin - (sqrt cost2)))
          in
            if (cost2 > 0)
              then 
                smallpaint scene (Ray hp rayd) (depth + 1)
              else return $ VecD 0 0 0

generatePicture :: (AccelStruct a b) =>
                   (a -> Ray -> Int -> State (Halton, Halton) Vec3)
                -> Context a b
                -> [[Vec3]]
generatePicture color context =
  evalState (sequence $ replicate h line) (1, h, hal1, hal2)
     where
       Context {
         ctxw = w
       , ctxh = h
       , ctxsamp = samp
       , ctxcx = cx
       , ctxcy = cy
       , ctxcamdir = camdir
       , ctxcampos = campos
       , ctxscene = scene
       } = context
       hal1 = halton 0 2
       hal2 = halton 0 2
       line :: State (Int, Int, Halton, Halton) [Vec3]
       line = do
         (x, y, hal1, hal2) <- get
         let (vals, (x', y', h1', h2')) = runState (sequence $ replicate w color') (1, y, hal1, hal2)
         put (x', y'-1, h1', h2')
         return vals
       color' :: State (Int, Int, Halton, Halton) Vec3
       color' = do
         (x, y, hal1, hal2) <- get
         let (val, (hal1', hal2')) = runState (sequence $ replicate samp (color (ctxscene context) (ray x y) 0)) (hal1, hal2)
         put (x+1, y, hal1', hal2')
         return $ avgColor val
           where
             d x y = (cx &! (((fromIntegral x) / fromIntegral w) - 0.5))
                 &+ (cy &! (((fromIntegral y) / fromIntegral h) - 0.5))
                 &+ camdir
             ray x y = Ray (campos &+ ((d x y) &! 140.0)) (normalize (d x y))
