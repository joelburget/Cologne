{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
module Render (
    Context(..)
  , line
  ) where

import Control.Applicative
import Control.Monad.State
import Data.Typeable
import Data.Data
import Data.List
import Data.Ord
import Random

import Vec
import Primitives
  
data Ray = Ray {
    origin    :: !VecD
  , direction :: !VecD
  }

maybeMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
maybeMinimumBy _ [] = Nothing
maybeMinimumBy f l = Just (minimumBy f l)

intersectScene :: [Primitive] -> Ray -> Maybe (Primitive, Double)
intersectScene scene r = maybeMinimumBy (comparing snd) [(s, t) | (s, Just t) <- map ((,) <*> intersectSphere r) scene]

radiance' :: (RandomGen g) => [Primitive] -> Ray -> Int -> Primitive -> Double -> State g VecD
radiance' scene r depth obj t | depth >= 5 = return (emission obj) --R.R.
                              | otherwise = do p' <- State (randomR (0, 1))
                                               if p' >= p
                                                 then return (emission obj) --R.R.
                                                 else let f = color obj |*| (1.0 / p) in (emission obj |+|) . (f `vmult`) <$> reflect (refl obj)
                              where Ray raypos raydir = r
                                    x = raypos |+| (raydir |*| t)
                                    n = norm (x |-| position obj)
                                    nl | (n `dot` raydir) < 0 = n
                                       | otherwise = n |*| (-1)
                                    p = let VecD fx fy fz = color obj in maximum [fx, fy, fz]
                                    reflRay = Ray x (raydir |-| (n |*| (2 * (n `dot` raydir))))
                                    reflect DIFF = let w = nl                                -- Ideal DIFFUSE reflection
                                                       VecD wx _ _ = w
                                                       u | abs wx > 0.1 = norm (VecD 0 1 0 `cross` w)
                                                         | otherwise = norm (VecD 1 0 0 `cross` w)
                                                       v = w `cross` u
                                                   in do r1 <- State (randomR (0, 2 * pi))
                                                         r2 <- State (randomR (0, 1))
                                                         let r2s = sqrt r2
                                                             d = norm ((u |*| (cos r1 * r2s)) |+| 
                                                                       (v |*| (sin r1 * r2s)) |+| 
                                                                       (w |*| sqrt (1 - r2)))
                                                         radiance scene (Ray x d) (depth + 1)
                                    reflect SPEC = radiance scene reflRay (depth + 1)             -- Ideal SPECULAR reflection
                                    reflect REFR | cos2t < 0 = radiance scene reflRay (depth + 1) -- Total internal reflection
                                                 | depth >= 2 = do pp' <- State (randomR (0, 1))
                                                                   if pp' < pp
                                                                     then (|*| rp) <$> radiance scene reflRay (depth + 1)
                                                                     else (|*| tp) <$> radiance scene (Ray x tdir) (depth + 1)
                                                 | otherwise = do re' <- (|*| re) <$> radiance scene reflRay (depth + 1)
                                                                  tr' <- (|*| tr) <$> radiance scene (Ray x tdir) (depth + 1)
                                                                  return (re' |+| tr')    -- Ideal dielectric REFRACTION
                                                 where into = (n `dot` nl) > 0             -- Ray from outside going in?
                                                       nc = 1
                                                       nt = 1.5
                                                       nnt | into = nc / nt
                                                           | otherwise = nt / nc
                                                       ddn = raydir `dot` nl
                                                       cos2t = 1 - (nnt * nnt * (1 - (ddn * ddn)))
                                                       tdir = norm ((raydir |*| nnt) |-| (n |*| ((if into then 1 else (-1)) * (ddn * nnt + sqrt cos2t))))
                                                       a = nt - nc
                                                       b = nt + nc
                                                       r0 = a * a / (b * b)
                                                       c | into = 1 + ddn
                                                         | otherwise = 1 - tdir `dot` n
                                                       re = r0 + ((1 - r0) * c * c * c * c * c)
                                                       tr = 1 - re
                                                       pp = 0.25 + (0.5 * re)
                                                       rp = re / p
                                                       tp = tr / (1 - pp)

radiance :: (RandomGen g) => [Primitive] -> Ray -> Int -> State g VecD
radiance scene r depth | Just (obj, t) <- intersectScene scene r = radiance' scene r depth obj t
                       | otherwise = return (VecD 0 0 0)

data Context = Context { 
    ctxw :: Int             -- ^ Width in Pixels
  , ctxh :: Int             -- ^ Height in Pixels
  , ctxsamp :: Int          -- ^ Samples to take?
  , ctxcx :: VecD           -- ^ Change in x Direction per pixel
  , ctxcy :: VecD           -- ^ Change in y Direction per pixel
  , ctxcamdir :: VecD       -- ^ Camera Direction
  , ctxcampos :: VecD       -- ^ Camera Position
  , ctxscene :: [Primitive]    -- ^ Scene Primitive
  } deriving (Data, Typeable)

-- Given the context render line number y
line :: Context -> Int -> [VecD]
line context y = evalState (mapM (pixel . subtract 1) [1..w]) (mkStdGen (y * y * y))
                 where Context { ctxw = w, ctxh = h, ctxsamp = samp, ctxcx = cx, ctxcy = cy, ctxcamdir = camdir, ctxcampos = campos, ctxscene = scene } = context
                       pixel x = (|*| 0.25) . foldl1 (|+|) <$> sequence [subpixel x sx sy | sy <- [0 :: Int, 1], sx <- [0 :: Int, 1]]
                       subpixel x sx sy = Render.fmap clamp . (|*| (1 / fromIntegral samp)) . foldl1 (|+|) <$> replicateM samp (sample x sx sy)
                       sample x sx sy = do r1 <- State (randomR (0, 4))
                                           r2 <- State (randomR (0, 4))
                                           let dx | r1 < 2 = sqrt r1 - 2
                                                  | otherwise = 2 - sqrt (4 - r1)
                                               dy | r2 < 2 = sqrt r2 - 2
                                                  | otherwise = 2 - sqrt (4 - r2)
                                               d = (cx |*| ((((fromIntegral sx + 0.5 + dx) / 2 + fromIntegral x) / fromIntegral w) - 0.5)) |+|
                                                   (cy |*| ((((fromIntegral sy + 0.5 + dy) / 2 + fromIntegral y) / fromIntegral h) - 0.5)) |+| camdir
                                               ray = Ray (campos |+| (d |*| 140.0)) (norm d)
                                           radiance scene ray 0
