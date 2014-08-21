{-|
  This module handles starting the engine. This is done via the use of the
  @'startEngine'@ function.
-}
module FRP.Spice.Engine (startEngine) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Data.IORef

-------------------
-- Local Imports --
import FRP.Spice.Engine.RunInput
import FRP.Spice.Engine.Network
import FRP.Spice.Engine.Driver
import FRP.Spice.Input.Backend
import FRP.Spice.Config
import FRP.Spice.Game
import FRP.Spice.Math

----------
-- Code --

-- Making the size from a WindowConfig
makeSize :: WindowConfig -> Size
makeSize wc = Size (fromIntegral $ getWindowWidth wc) (fromIntegral $ getWindowHeight wc)

-- Making the displaybits from a WindowConfig
makeDisplayBits :: [DisplayBits]
makeDisplayBits = [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24]

-- Making the window mode from a WindowConfig
makeWindowMode :: WindowConfig -> WindowMode
makeWindowMode wc =
  if getWindowFullscreen wc
    then FullScreen
    else Window

-- Resizing the window
resizeCallback :: IORef (Vector Int) -> WindowSizeCallback
resizeCallback wSize size@(Size w h) = do
  writeIORef wSize $ Vector (fromIntegral w) (fromIntegral h)
  viewport $= (Position 0 0, size)

{-|
  Starting the spice engine with the parameters prescribed in the
  @'WindowConfig'@. It updates and renders the @'Game'@ automatically so all
  you need to to is set up the @'WindowConfig'@ and make a datatype with an
  instance of @'Game'@.
-}
startEngine :: Game a => WindowConfig -> a -> IO ()
startEngine wc game = do
  -- Opening the window
  initialize
  openWindow (makeSize wc) makeDisplayBits (makeWindowMode wc)
  windowTitle $= getWindowTitle wc

  -- Checking for the window being closed
  closed <- newIORef False
  windowCloseCallback $= do
    writeIORef closed True
    return True

  -- Function to run on window resize
  wSizeRef <- newIORef $ Vector (getWindowWidth wc) (getWindowHeight wc)
  windowSizeCallback $= resizeCallback wSizeRef

  -- Getting the input container
  ic <- makeInputContainer

  -- Updating the input
  mousePosCallback    $= makeMousePositionCallback ic wSizeRef
  keyCallback         $= makeKeyboardCallback ic
  mouseButtonCallback $= makeMouseCallback ic

  -- Creating the network
  network <- makeNetwork (getInput ic) game

  -- Driving the network
  GLFW.time $= 0
  driveNetwork network $ runInput closed

  -- Closing the window, after all is said and done
  closeWindow
