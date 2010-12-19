module Cologne.Shaders.Smallpt (
    ReflectionType(..)
  , radiance
  , objRadiance
  ) where

import System.Random
import Cologne.Vec
import Cologne.Primitives hiding (intersect)
import Cologne.Accel

-- We're just emulating the classic diffuse, specular, refractive model.  We
-- need to know the color, diffusion, and what type of surface interaction
-- there is.

data ReflectionType = Diffuse | Specular | Refractive

-- We have two radiance functions, one to give the radiance returned at the
-- intersection of an object (objRadiance) and another to give the radiance of
-- a ray shot into the scene (radiance).

-- To compute the radiance of a ray shot into the scene we check to see if the
-- ray intersects an object. If it misses all objects we return black, if it
-- hits, we return the color returned by the object for that particular ray.
-- Note that 'color' is the funtion required by the Primitive typeclass, in
-- this case it would be objRadiance partially applied.
radiance :: (AccelStruct a) => a -> Ray -> Int -> Int -> ColorD
radiance scene r depth rand 
  | Intersection t obj <- aIntersect scene r = color obj scene r t depth rand
  | otherwise = VecD 0 0 0

-- Our purpose here is to partially apply just the first 3 arguments, then
-- giving the resulting function to an object, to be its radiance function.
-- This is a rewritten copy of the radiance function from smallpt-haskell:
-- http://www.partario.com/blog/2010/03/a-render-farm-in-haskell.html
-- which is, in turn, a rewrite of the original smallpt:
-- http://kevinbeason.com/smallpt/
objRadiance :: ({-Primitive pr, -}AccelStruct a) =>
               ColorD ->                 -- The color of the object
               ColorD ->                 -- Its emission
               ReflectionType ->
               (VecD -> VecD) ->         -- Equation for the normal
               --pr ->
               (a -> Ray -> Double -> Int -> Int -> ColorD)
objRadiance color emission refl normal scene ray t depth rand
  | depth >= 5 = emission
  | otherwise = if r2 >= p
    then emission
    else emission |+| (f `vmult` reflect refl)
      where
        f = color |*| (1.0 / p)
        Ray raypos raydir = ray
        x = raypos |+| (raydir |*| t)
        n = normal x
        nl | (n `dot` raydir) < 0 = n
           | otherwise = n |*| (-1)
        p = let VecD fx fy fz = color in maximum [fx, fy, fz]
        sGen = mkStdGen rand
        r2 = fst $ randomR (0, 1.0) ((snd . next) sGen) --(fst . next . snd . next) sGen
        reflRay = Ray x (raydir |-| (n |*| (2 * (n `dot` raydir))))
        nextRand = (fst . next) sGen
        reflect Diffuse =
          let
            w = nl
            VecD wx _ _ = w
            u | abs wx > 0.1 = norm $ VecD 0 1 0 `cross` w
              | otherwise = norm $ VecD 1 0 0 `cross` w
            v = w `cross` u
            r1 = fst $ randomR (0, 2*pi) sGen --(fst . next) sGen
            -- see above for r2
            r2s = sqrt r2
            d = norm ((u |*| ((cos r1) * r2s))
                  |+| (v |*| ((sin r1) * r2s))
                  |+| (w |*| sqrt (1 - r2)))
          in
            radiance scene (Ray x d) (depth + 1) nextRand
        reflect Specular = radiance scene reflRay (depth + 1) nextRand
        reflect Refractive
          | cos2t < 0 = radiance scene reflRay (depth + 1) nextRand
          | depth >= 2 =
            let pp' = r2
            in if pp' < pp
               then (radiance scene reflRay (depth + 1) nextRand) |*| rp 
               else (radiance scene (Ray x tdir) (depth + 1) nextRand) |*| tp 
          | otherwise =
            let re' = (radiance scene reflRay (depth + 1) nextRand) |*| re 
                tr' = (radiance scene (Ray x tdir) (depth + 1) nextRand) |*| tr 
            in
              re' |+| tr'
          where
            into = (n `dot` nl) > 0
            nc = 1
            nt = 1.5
            nnt | into = nc / nt
                | otherwise = nt / nc
            ddn = raydir `dot` nl
            cos2t = 1 - (nnt * nnt * (1 - (ddn * ddn)))
            tdir = norm ((raydir |*| nnt) |-|
              (n |*| ((if into then 1 else (-1)) * (ddn * nnt + sqrt cos2t))))
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
