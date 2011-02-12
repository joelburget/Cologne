{-# LANGUAGE FlexibleContexts #-}

module Cologne.Shaders.Smallpt (
    radiance
  ) where

import System.Random
import Control.Monad.State
import Control.Applicative ((<$>))

import Cologne.Vec
import Cologne.Primitives hiding (intersect)

-- To compute the radiance of a ray shot into the scene we check to see if the
-- ray intersects an object. If it misses all objects we return black, if it
-- hits, we return the color returned by the object for that particular ray.
-- This is a rewritten copy of the radiance function from smallpt-haskell:
-- http://www.partario.com/blog/2010/03/a-render-farm-in-haskell.html which is,
-- in turn, a rewrite of the original smallpt: http://kevinbeason.com/smallpt/
radiance :: (AccelStruct a (VecD , VecD, ReflectionType)) =>
            a ->
            Ray ->
            Int ->
            State Int ColorD
radiance scene ray depth = do
  rand <- get
  put $ (fst . next . mkStdGen) rand
  case aIntersect scene ray of
    Miss -> return $ VecD 0 0 0
    Intersection t (color, emission, rtype) nrm -> objRadiance
      where
        objRadiance
          | depth >= 5 = return emission
          | otherwise = if r2 >= p
            then return emission
            else do clr <- reflect rtype
                    return $ emission |+| (f `vmult` clr)
              where
                f = color |*| (1.0 / p)
                Ray raypos raydir = ray
                x = raypos |+| (raydir |*| t)
                n = nrm
                nl | (n `dot` raydir) < 0 = n
                   | otherwise = n |*| (-1)
                p = let VecD fx fy fz = color in maximum [fx, fy, fz]
                sGen = mkStdGen rand
                r2 = fst $ randomR (0, 1.0) ((snd . next) sGen) --(fst . next . snd . next) sGen
                reflRay = Ray x (raydir |-| (n |*| (2 * (n `dot` raydir))))
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
                    radiance scene (Ray x d) (depth + 1)
                reflect Specular = radiance scene reflRay (depth + 1)
                reflect Refractive
                  | cos2t < 0 = radiance scene reflRay (depth + 1)
                  | depth >= 2 =
                    let pp' = r2
                    in if pp' < pp
                       then (|*| rp) <$> (radiance scene reflRay (depth + 1))
                       else (|*| tp) <$> (radiance scene (Ray x tdir) (depth + 1))
                  | otherwise = do
                    re' <- (|*| re) <$> (radiance scene reflRay (depth + 1))
                    tr' <- (|*| tr) <$> (radiance scene (Ray x tdir) (depth + 1))
                    return $ re' |+| tr'
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
