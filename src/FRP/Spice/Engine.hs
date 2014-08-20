{-|
  This module handles starting the engine. This is done via the use of the
  @'startEngine'@ function.
-}
module FRP.Spice.Engine (startEngine) where

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

-- Containing if the engine has been made before
madeRef :: IO (IORef Bool)
madeRef = newIORef False

{-|
  Starting the spice engine with the parameters prescribed in the
  @'WindowConfig'@. It updates and renders the @'Game'@ automatically so all
  you need to to is set up the @'WindowConfig'@ and make a datatype with an
  instance of @'Game'@.
-}
startEngine :: Game a => WindowConfig -> a -> IO ()
startEngine wc game = do
  made   <- madeRef
  isMade <- readIORef made
  if isMade
    then error "You cannot run 'startEngine' more than once per program."
    else do
      writeIORef made True

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
