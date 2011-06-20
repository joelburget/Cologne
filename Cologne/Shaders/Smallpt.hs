{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Cologne.Shaders.Smallpt (
    smallpt
  ) where

import System.Random
import Control.Monad.ST
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Control.Monad.State (State, get, put, runState)
import Control.Applicative ((<$>))
import Data.Vect (Vec3(Vec3), (&+), (&-), (&*), (&.), (&!), (&^), len, normalize)
import Data.Vector.Mutable (MVector, new, read, write)
import Data.Vector (Vector, unsafeFreeze, forM_, enumFromN)
import Graphics.Formats.Assimp (lookAt, position, horizontalFOV, aspect, up)

import Cologne.Primitives hiding (intersect)
import Cologne.AssimpImport (ColorInfo)
import Cologne.Lens (runLens, standardLens)

-- To compute the radiance of a ray shot into the scene we check to see if the
-- ray intersects an object. If it misses all objects we return black, if it
-- hits, we return the color returned by the object for that particular ray.
-- This is a rewritten copy of the radiance function from smallpt-haskell:
-- http://www.partario.com/blog/2010/03/a-render-farm-in-haskell.html which is,
-- in turn, a rewrite of the original smallpt: http://kevinbeason.com/smallpt/
radiance :: (AccelStruct a (Vec3 , Vec3, ReflectionType)) =>
            a
            -> Ray
            -> Int
            -> State Int Vec3
radiance scene ray depth = do
  !rand <- get
  put $ (fst . next . mkStdGen) rand
  case aIntersect scene ray of
    Miss -> return $ Vec3 0 0 0
    Intersection t (color, emission, rtype) nrm -> objRadiance
      where
        objRadiance
          | depth >= 5 = return emission
          | otherwise = if r2 >= p
            then return emission
            else do clr <- reflect rtype
                    return $ emission &+ (f &! clr)
              where
                f = color &* (1.0 / p)
                Ray raypos raydir = ray
                x = raypos &+ (raydir &* t)
                n = nrm
                nl | (n &. raydir) < 0 = n
                   | otherwise = n &* (-1)
                p = let Vec3 fx fy fz = color in maximum [fx, fy, fz]
                sGen = mkStdGen rand
                r2 = fst $ randomR (0, 1.0) ((snd . next) sGen) --(fst . next . snd . next) sGen
                reflRay = Ray x (raydir &- (n &* (2 * (n &. raydir))))
                reflect Diffuse =
                  let
                    w = nl
                    Vec3 wx _ _ = w
                    u | abs wx > 0.1 = normalize $ Vec3 0 1 0 &^ w
                      | otherwise = normalize $ Vec3 1 0 0 &^ w
                    v = w &^ u
                    r1 = fst $ randomR (0, 2*pi) sGen --(fst . next) sGen
                    -- see above for r2
                    r2s = sqrt r2
                    d = normalize ((u &* ((cos r1) * r2s))
                          &+ (v &* ((sin r1) * r2s))
                          &+ (w &* sqrt (1 - r2)))
                  in
                    radiance scene (Ray x d) (depth + 1)
                reflect Specular = radiance scene reflRay (depth + 1)
                reflect Refractive
                  | cos2t < 0 = radiance scene reflRay (depth + 1)
                  | depth >= 2 =
                    let pp' = r2
                    in if pp' < pp
                       then (&* rp) <$> (radiance scene reflRay (depth + 1))
                       else (&* tp) <$> (radiance scene (Ray x tdir) (depth + 1))
                  | otherwise = do
                    re' <- (&* re) <$> (radiance scene reflRay (depth + 1))
                    tr' <- (&* tr) <$> (radiance scene (Ray x tdir) (depth + 1))
                    return $ re' &+ tr'
                  where
                    into = (n &. nl) > 0
                    nc = 1
                    nt = 1.5
                    nnt | into = nc / nt
                        | otherwise = nt / nc
                    ddn = raydir &. nl
                    cos2t = 1 - (nnt * nnt * (1 - (ddn * ddn)))
                    tdir = normalize ((raydir &* nnt) &-
                      (n &* ((if into then 1 else (-1)) * (ddn * nnt + sqrt cos2t))))
                    a = nt - nc
                    b = nt + nc
                    r0 = a * a / (b * b)
                    c | into = 1 + ddn
                      | otherwise = 1 - tdir &. n
                    re = r0 + ((1 - r0) * c * c * c * c * c)
                    tr = 1 - re
                    pp = 0.25 + (0.5 * re)
                    rp = re / p
                    tp = tr / (1 - pp)

smallpt :: Context [Primitive ColorInfo] ColorInfo
                -> Vector (Vector Vec3)
smallpt (Context options cams scene) =
  runST generatePicture
  where
    generatePicture :: ST s (Vector (Vector Vec3))
    generatePicture = do
      pic <- new h
      rand <- newSTRef 17
      forM_ (enumFromN 0 h) $ \row -> do
        vec <- new w
        forM_ (enumFromN 0 w) $ \column -> do
          randN <- readSTRef rand
          let (val, nextRand) = runState (sequence $ replicate samp (radiance scene (dir column row) 0)) randN
          writeSTRef rand nextRand
          write vec column (avgColor val)
        unsafeFreeze vec >>= write pic row
      unsafeFreeze pic

    w = width options
    h = height options
    samp = samples options
    cam = head cams

    dir x y = runLens standardLens cam w h x y 0 0
