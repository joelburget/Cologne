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


{- This is  where the magic happens. radiance returns the color that a ray sees
 - when it intersects the scene.
 -}

radiance :: (RandomGen g) => Accel -> Ray -> Int -> State g VecD
radiance scene r depth | Intersection t obj <- intersect scene r = color obj r depth
                       | otherwise = return (VecD 0 0 0)

data Context = Context { 
    ctxw      :: !Int        -- ^ Width in Pixels
  , ctxh      :: !Int        -- ^ Height in Pixels
  , ctxsamp   :: !Int        -- ^ Samples to take?
  , ctxcx     :: !VecD       -- ^ Change in x Direction per pixel
  , ctxcy     :: !VecD       -- ^ Change in y Direction per pixel
  , ctxcamdir :: !VecD       -- ^ Camera Direction
  , ctxcampos :: !VecD       -- ^ Camera Position
  , ctxscene  :: Accel       -- ^ Scene Primitive
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
