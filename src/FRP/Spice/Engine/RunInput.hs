module FRP.Spice.Engine.RunInput where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Data.IORef
import GHC.Float

-------------------
-- Local Imports --
import FRP.Spice.Game

----------
-- Code --

-- Running the input
runInput :: IORef Bool -> IO (Maybe Float)
runInput closed = do
  c <- readIORef closed

  if c
    then return Nothing
    else do
      dt <- get GLFW.time
      GLFW.time $= 0
      return $ Just $ double2Float (dt / 1000)
