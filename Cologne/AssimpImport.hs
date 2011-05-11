module Cologne.AssimpImport where

import Data.Maybe (catMaybes)
import Control.Monad (liftM)
import Data.Vect (Vec3(Vec3), (&*), (&^))

import Graphics.Formats.Assimp (importFile, Scene(Scene), cameraPosition, 
  lookAt, Light, Face, Mesh, indices, cameras, meshes, faces, vertices, 
  lightPosition, PostProcessSteps(CalcTangentSpace, Triangulate), lights)
import Cologne.Primitives (Primitive, AccelStruct, Context(Context), 
  ReflectionType(Diffuse), listToAccelStruct, defaultOptions)
import Cologne.Primitives.Triangle (triangle)
import Cologne.Primitives.Sphere (sphere)
-- import Cologne.Accel.KdTree (KdTree)
import Cologne.Accel.List

import Debug.Trace

type ColorInfo = (Vec3, Vec3, ReflectionType)
-- type ContextType = Context (KdTree (Primitive ColorInfo)) ColorInfo
type ContextType = Context [Primitive ColorInfo] ColorInfo

assimpImport :: FilePath -> IO (Either String ContextType)
assimpImport path = (liftM . liftM) (convert path) $ importFile path Triangulate

convert :: FilePath -> Scene -> ContextType
convert file scene =
  let cams    = cameras scene
      scene'  = convertScene scene
  in Context defaultOptions cams scene'

convertScene :: Scene -> [Primitive ColorInfo]
convertScene scene = listToAccelStruct $ prims ++ lights'
  where
    prims   = concatMap convertMesh  $ meshes scene
    lights' = map       convertLight $ lights scene

    convertLight :: Light -> Primitive ColorInfo
    convertLight light = sphere (lightPosition light) 0.0001
                                (Vec3 0 0 0, Vec3 1 1 1, Diffuse)

    convertMesh :: Mesh -> [Primitive ColorInfo]
    convertMesh mesh = catMaybes $ map (convertFace (vertices mesh)) (faces mesh)

    convertFace :: [Vec3] -> Face -> Maybe (Primitive ColorInfo)
    convertFace vert face =
      if length ind == 3
      then Just $ triangle x y z (Vec3 1 1 1, Vec3 0 0 0, Diffuse)
      else Nothing
      where ind = indices face
            x = vert !! (fromIntegral $ ind !! 0)
            y = vert !! (fromIntegral $ ind !! 1)
            z = vert !! (fromIntegral $ ind !! 2)
