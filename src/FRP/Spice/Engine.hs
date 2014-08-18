module FRP.Spice.Engine where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import FRP.Elerea.Param
import Data.IORef

-------------------
-- Local Imports --
import FRP.Spice.Engine.RunInput
import FRP.Spice.Engine.Network
import FRP.Spice.Engine.Driver
import FRP.Spice.Config
import FRP.Spice.Input
import FRP.Spice.Game

----------
-- Code --

-- Starting the engine
startEngine :: Game a => WindowConfig -> a -> IO ()
startEngine wc game = do
  -- Opening the window
  initialize
  openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window

  -- Checking for the window being closed
  closed <- newIORef False
  windowCloseCallback $= do
    writeIORef closed True
    return True

  -- Getting an external of the game
  (gameSignal, gameSink) <- external game

  -- Getting the input container
  ic <- makeInputContainer

  -- Updating the input
  mousePosCallback    $= makeMousePositionCallback ic
  keyCallback         $= makeKeyboardCallback ic
  mouseButtonCallback $= makeMouseCallback ic

  -- Creating the network
  network <- makeNetwork (getInput ic) gameSignal gameSink

  -- Driving the network
  GLFW.time $= 0
  driveNetwork network $ runInput closed

  -- Closing the window, after all is said and done
  closeWindow
