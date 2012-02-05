{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{- todo:
 - investigate stm chans:
 - http://hackage.haskell.org/packages/archive/BoundedChan/1.0.0.2/doc/html/Control-Concurrent-BoundedChan.html
 - http://hackage.haskell.org/packages/archive/bounded-tchan/0.2.3/doc/html/Control-Concurrent-STM-BTChan.html
 - http://community.haskell.org/~wren/stm-chans/dist/doc/html/stm-chans/Control-Concurrent-STM-TBChan.html
 - benchmark this:
 - http://stackoverflow.com/questions/4567750/removing-the-contents-of-a-chan-or-mvar-in-a-single-discrete-step
 -
 - the opengl code could also use some major work
 - investigate gl_arb_texture_rectangle
 -}

module Cologne.OpenGL.Display (display) where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw (gluPerspective)

import System.Exit (exitWith, ExitCode(..))
import Control.Monad (forever, forM_)
import Control.Concurrent.BoundedChan (readChan, BoundedChan, getChanContents, isEmptyChan)
import Data.IORef
import Foreign.Ptr
import Foreign.Storable (poke, peek)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Alloc (mallocBytes)
import Data.Vect (Vec3(Vec3))
import Data.ByteString.Internal (memset)

import Cologne.Primitives hiding (width, height)
import Cologne.Workers (Result)

data GLConfig = GLConfig
  { texId  :: GLuint
  , width  :: Int
  , height :: Int
  , buffer :: Ptr GLubyte
  }

texInit :: IO GLuint
texInit = do
  tId <- alloca $ \ptr -> glGenTextures 1 ptr >> peek ptr
  glBindTexture gl_TEXTURE_2D tId
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_REPEAT
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_REPEAT
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_NEAREST
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_NEAREST
  err <- glGetError
  if err == gl_NO_ERROR then return () else error "Died in texInit"
  return tId

initGL :: IORef GLConfig -> IO ()
initGL conf = do
  glShadeModel gl_FLAT -- enables smooth color shading
  glClearColor 0 0 0 0 -- Clear the background color to black
  -- glClearDepth 1 -- enables clearing of the depth buffer
  -- glDepthFunc gl_LEQUAL -- type of depth test
  -- glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST
  glDisable gl_DEPTH_TEST
  glPixelStorei gl_UNPACK_ALIGNMENT 1

  tId <- texInit
  modifyIORef conf $ \c -> c {texId = tId}

resizeScene :: GLFW.WindowSizeCallback
resizeScene w h = do
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  -- glOrtho (-1) 1 (-1) 1 (-1) 1
  glOrtho 0 (fromIntegral w) (fromIntegral h) 0 (-1) 1
  glDisable gl_DEPTH_TEST
  glMatrixMode gl_MODELVIEW
  glLoadIdentity

drawScene :: GLConfig -> BoundedChan Result -> IO ()
drawScene conf resultChan = do

  let w = width conf
      h = height conf
      buf = buffer conf

  results <- getChanContents resultChan
  forM_ (take 1000 results) $ \(x, y, Vec3 r g b) -> do
    poke (buf `plusPtr` (fromIntegral $ 3 * (y * w + x)))     $ (round (r * 255) :: GLubyte)
    poke (buf `plusPtr` (fromIntegral $ 3 * (y * w + x) + 1)) $ (round (g * 255) :: GLubyte)
    poke (buf `plusPtr` (fromIntegral $ 3 * (y * w + x) + 2)) $ (round (b * 255) :: GLubyte)

  glTexImage2D gl_TEXTURE_2D 0 (fromIntegral gl_RGB8) (fromIntegral w) (fromIntegral h) 0 gl_RGB gl_UNSIGNED_BYTE buf
  
  glClear $ fromIntegral gl_DEPTH_BUFFER_BIT
  glClear $ fromIntegral gl_COLOR_BUFFER_BIT
  glEnable gl_TEXTURE_2D
  glTexEnvf gl_TEXTURE_ENV gl_TEXTURE_ENV_MODE $ fromIntegral gl_DECAL
  glBindTexture gl_TEXTURE_2D (texId conf)

  glBegin gl_QUADS
  -- glTexCoord2f 0 0; glVertex3f (-1) 1    0
  -- glTexCoord2f 1 0; glVertex3f 1    1    0
  -- glTexCoord2f 1 1; glVertex3f 1    (-1) 0
  -- glTexCoord2f 0 1; glVertex3f (-1) (-1) 0
  let w' = fromIntegral w
      h' = fromIntegral h
  glTexCoord2f 0 0; glVertex3f 0 0 0
  glTexCoord2f 1 0; glVertex3f w' 0 0
  glTexCoord2f 1 1; glVertex3f w' h' 0
  glTexCoord2f 0 1; glVertex3f 0 h' 0
  glEnd

  glFlush
  glDisable gl_TEXTURE_2D

shutdown :: GLFW.WindowCloseCallback
shutdown = do
  GLFW.closeWindow
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return True

keyPressed :: GLFW.KeyCallback
keyPressed GLFW.KeyEsc True = shutdown >> return ()
keyPressed _           _    = return ()

display :: Int -> Int -> BoundedChan Result -> IO ()
display w h results = do
    True <- GLFW.initialize

    buf <- mallocBytes $ w * h * 3
    _ <- memset (castPtr buf) 255 (fromIntegral $ w * h * 3)
    conf <- newIORef $ GLConfig (-1) w h buf

    True <- GLFW.openWindow GLFW.defaultDisplayOptions
                    { GLFW.displayOptions_width  = w
                    , GLFW.displayOptions_height = h
                    -- Set depth buffering and RGB colors
                    , GLFW.displayOptions_numRedBits   = 8
                    , GLFW.displayOptions_numGreenBits = 8
                    , GLFW.displayOptions_numBlueBits  = 8
                    , GLFW.displayOptions_numAlphaBits = 0
                    , GLFW.displayOptions_numDepthBits = 1
                    -- , GLFW.displayOptions_displayMode  = GLFW.Fullscreen
                    } 
    -- open a window
    GLFW.setWindowTitle "CologneGL"
    -- register the function called when our window is resized
    GLFW.setWindowSizeCallback resizeScene
    -- register the function called when the keyboard is pressed.
    GLFW.setKeyCallback keyPressed
    -- register window close handler
    GLFW.setWindowCloseCallback shutdown
    initGL conf
    -- start event processing engine

    forever $ do
      conf' <- readIORef conf
      drawScene conf' results
      GLFW.swapBuffers
