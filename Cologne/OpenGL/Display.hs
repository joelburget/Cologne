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

import System.Exit (exitWith, ExitCode(..))
import System.Mem
import Control.Monad (liftM, forever, forM, forM_)
import Control.Concurrent.BoundedChan (newBoundedChan, BoundedChan,
  getChanContents)
import Control.Concurrent (ThreadId, forkIO, killThread)
import Foreign.Ptr
import Foreign.Storable (poke, peek, sizeOf)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Alloc (mallocBytes)
import Data.Lens (getL, setL, (^$!), (^%=), (^!=))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Data.Vect (Vec3(Vec3))
import Data.Word (Word32)
import Data.ByteString.Internal (memset)

import Cologne.Primitives hiding (shader, width, height, threads)
import qualified Cologne.Primitives as P (width, height, threads)
import Cologne.Workers (ResultMessage(ResultPixel), TaskMessage,
  worker, randomFill, Shader)
import Cologne.OpenGL.GLConfig

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
initGL confRef = do
  glShadeModel gl_FLAT -- enables smooth color shading
  glClearColor 0 0 0 0 -- Clear the background color to black
  -- glClearDepth 1 -- enables clearing of the depth buffer
  -- glDepthFunc gl_LEQUAL -- type of depth test
  -- glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST
  glDisable gl_DEPTH_TEST
  glPixelStorei gl_UNPACK_ALIGNMENT 1

  tId <- texInit
  modifyIORef confRef $ setL texId tId

resizeScene :: AccelStruct a b 
            => Context a b 
            -> Shader a b 
            -> IORef GLConfig 
            -> GLFW.WindowSizeCallback
resizeScene context shader confRef w h = do
  modifyIORef confRef $ setL width w . setL height h

  -- kill workers
  _ <- (killWorkers . getL threads) `liftM` readIORef confRef

  -- new queues
  taskChan   <- newBoundedChan 1000
  resultChan <- newBoundedChan 1000

  -- new buffers
  _ <- allocateBuffers confRef

  -- restart workers
  workers <- spawnWorkers w h context shader taskChan resultChan
  modifyIORef confRef $ (threads ^!= workers) . (tasks ^!= taskChan) . (results ^!= resultChan)

  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  -- glOrtho (-1) 1 (-1) 1 (-1) 1
  glOrtho 0 (fromIntegral w) (fromIntegral h) 0 (-1) 1
  glDisable gl_DEPTH_TEST
  glMatrixMode gl_MODELVIEW
  glLoadIdentity

bufferIndex :: Int -> Int -> Int -> Int
bufferIndex w x y = 3 * (y * w + x)

drawScene :: GLConfig -> IO ()
drawScene conf = do
  let buf        = buffer  ^$! conf
      resultChan = results ^$! conf

  results <- getChanContents resultChan
  forM_ (take 1000 results) $ \(ResultPixel x y (Vec3 r g b)) -> do
    let idx = bufferIndex (width ^$! conf) x y
    poke (buf `plusPtr` (idx))     (round (r * 255) :: GLubyte)
    poke (buf `plusPtr` (idx + 1)) (round (g * 255) :: GLubyte)
    poke (buf `plusPtr` (idx + 2)) (round (b * 255) :: GLubyte)

  glTexImage2D 
    gl_TEXTURE_2D 0 
    (fromIntegral gl_RGB8) 
    (fromIntegral $ width  ^$! conf)
    (fromIntegral $ height ^$! conf)
    0 
    gl_RGB 
    gl_UNSIGNED_BYTE 
    buf
  
  glClear $ fromIntegral gl_DEPTH_BUFFER_BIT
  glClear $ fromIntegral gl_COLOR_BUFFER_BIT
  glEnable gl_TEXTURE_2D
  glTexEnvf gl_TEXTURE_ENV gl_TEXTURE_ENV_MODE $ fromIntegral gl_DECAL
  glBindTexture gl_TEXTURE_2D $ texId ^$! conf

  let w   = fromIntegral $ width  ^$! conf
      h   = fromIntegral $ height ^$! conf

  glBegin gl_QUADS
  -- glTexCoord2f 0 0; glVertex3f (-1) 1    0
  -- glTexCoord2f 1 0; glVertex3f 1    1    0
  -- glTexCoord2f 1 1; glVertex3f 1    (-1) 0
  -- glTexCoord2f 0 1; glVertex3f (-1) (-1) 0
  glTexCoord2f 0 0; glVertex3f 0 0 0
  glTexCoord2f 1 0; glVertex3f w 0 0
  glTexCoord2f 1 1; glVertex3f w h 0
  glTexCoord2f 0 1; glVertex3f 0 h 0
  glEnd

  glFlush
  glDisable gl_TEXTURE_2D
  performGC

shutdown :: GLFW.WindowCloseCallback
shutdown = do
  GLFW.closeWindow
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return True

keyPressed :: GLFW.KeyCallback
keyPressed GLFW.KeyEsc True = shutdown >> return ()
-- keyPressed (GLFW.CharKey 's') = undefined -- save image
keyPressed _           _    = return ()

killWorkers :: [ThreadId] -> IO ()
killWorkers = mapM_ killThread

spawnWorkers :: AccelStruct a b
             => Int
             -> Int
             -> Context a b 
             -> Shader a b
             -> BoundedChan TaskMessage 
             -> BoundedChan ResultMessage 
             -> IO [ThreadId]
spawnWorkers w h context shader tasks results = do
  let threadcount = P.threads ^$! options ^$! context
      context'    = options ^%= (P.width ^!= w) . (P.height ^!= h) $ context
  tIds <- forM [1..threadcount] $ const $ forkIO $ 
    worker context' shader tasks results
  fId <- forkIO $ randomFill w h tasks
  return $ fId:tIds

allocateBuffers :: IORef GLConfig -> IO ()
allocateBuffers confRef = do
  -- may be better to use http://hackage.haskell.org/packages/archive/vector/0.9.1/doc/html/Data-Vector-Storable.html
  conf <- readIORef confRef
  let w = width  ^$! conf
      h = height ^$! conf
      bufSize     = w * h * 3
      weightsSize = w * h * (sizeOf (undefined :: Word32))
  buf1 <- mallocBytes bufSize
  buf2 <- mallocBytes weightsSize 
  _ <- memset (castPtr buf1) 255 (fromIntegral bufSize)
  _ <- memset (castPtr buf2) 0   (fromIntegral weightsSize)
  modifyIORef confRef $ setL buffer buf1 . setL weight buf2

display :: AccelStruct a b => Context a b -> Shader a b -> IO ()
display context shader = do
  True <- GLFW.initialize

  let w = P.width  ^$! options ^$! context
      h = P.height ^$! options ^$! context

  tasks   <- newBoundedChan 1000
  results <- newBoundedChan 1000
  workers <- spawnWorkers w h context shader tasks results

  confRef <- newIORef $ GLConfig (-1) w h nullPtr nullPtr workers tasks results
  _ <- allocateBuffers confRef

  True <- GLFW.openWindow GLFW.defaultDisplayOptions
                  { GLFW.displayOptions_width  = w
                  , GLFW.displayOptions_height = h
                  -- Set depth buffering and RGB colors
                  , GLFW.displayOptions_numRedBits   = 8
                  , GLFW.displayOptions_numGreenBits = 8
                  , GLFW.displayOptions_numBlueBits  = 8
                  , GLFW.displayOptions_numAlphaBits = 0
                  , GLFW.displayOptions_numDepthBits = 1
                  } 
  -- open a window
  GLFW.setWindowTitle "CologneGL"
  -- register the function called when our window is resized
  GLFW.setWindowSizeCallback $ resizeScene context shader confRef
  -- register the function called when the keyboard is pressed.
  GLFW.setKeyCallback keyPressed
  -- register window close handler
  GLFW.setWindowCloseCallback shutdown
  initGL confRef
  -- start event processing engine

  forever $ do
    conf <- readIORef confRef
    drawScene conf
    GLFW.swapBuffers
