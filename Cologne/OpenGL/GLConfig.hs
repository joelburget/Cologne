{-# LANGUAGE TemplateHaskell #-}

module Cologne.OpenGL.GLConfig (
    GLConfig(GLConfig)
  , texId
  , width
  , height
  , buffer
  , weight
  , threads
  , tasks
  , results
  ) where

import Data.Lens.Template (makeLens)
import Graphics.Rendering.OpenGL.Raw (GLuint, GLubyte)
import Foreign.Ptr (Ptr)
import Control.Concurrent (ThreadId)
import Cologne.Workers (ResultMessage, TaskMessage)
import Control.Concurrent.BoundedChan (BoundedChan)

data GLConfig = GLConfig
  { _texId   :: !GLuint
  , _width   :: !Int
  , _height  :: !Int
  , _buffer  :: !(Ptr GLubyte)
  , _weight  :: !(Ptr Int)
  , _threads :: ![ThreadId]
  , _tasks   :: !(BoundedChan TaskMessage)
  , _results :: !(BoundedChan ResultMessage)
  }

$(makeLens ''GLConfig)
