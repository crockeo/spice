{-|
  This module need not be used directly. Refer to @'FRP.Spice.Engine'@ instead.
-}
module FRP.Spice.Engine.RunInput where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Data.IORef
import GHC.Float

----------
-- Code --

{-|
  Given an @'IORef'@ indicating whether or not the program should close, it
  either returns the delta time since the last call, or a @'Nothing'@,
  indicating that the program should close.
-}
runInput :: IORef Bool -> IO (Maybe Float)
runInput closed = do
  pollEvents
  c <- readIORef closed

  if c
    then return Nothing
    else do
      dt <- get GLFW.time
      GLFW.time $= 0
      return $ Just $ double2Float dt
