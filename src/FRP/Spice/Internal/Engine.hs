{-|
  This module contains all of the functions to create and start the engine. The
  only exposed function is @'startEngine'@ seeing as every other function is to
  be used solely internally within the module.
-}
module FRP.Spice.Internal.Engine ( startEngine
                                 , startEngineDefault
                                 ) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import FRP.Elerea.Param
import Data.Default
import Data.IORef

-------------------
-- Local Imports --
import FRP.Spice.Internal.Assets
import FRP.Spice.Internal.Input
import FRP.Spice.Internal.Types

----------
-- Code --

{-|
  The callback for the window closing.
-}
closeCallback :: IORef Bool -> WindowCloseCallback
closeCallback closedRef = do
  writeIORef closedRef True
  return True

{-|
  The callback for the window resizing.
-}
resizeCallback :: WindowSizeCallback
resizeCallback size@(Size w' h') = do
  let (Vector w h) = Vector (fromIntegral w') (fromIntegral h')

  matrixMode $= Projection
  loadIdentity

  ortho (-w) ( w)
        (-h) ( h)
        (-1) ( 1)

  matrixMode $= Modelview 0
  viewport   $= (Position 0 0, size)

{-|
  Creating the elerea network to be used in @'driveNetwork'@.
-}
makeNetwork :: Game a => a -> Signal Input -> IO (Float -> IO a)
makeNetwork game inputSignal = start $ transfer game update inputSignal

{-|
  Either sending back a 'should close window' (in the form of a Nothing) signal
  or the @'DeltaTime'@ (type synonym for @'Float'@) since the last update.
-}
runInput :: IORef Bool -> IO (Maybe DeltaTime)
runInput closedRef = do
  closed <- readIORef closedRef

  t <- get time
  time $= 0

  return $ if closed
    then Nothing
    else Just $ realToFrac t

{-|
  Driving the network created from @'makeNetwork'@ with the input created from
  @'runInput'@ to run the game loop.
-}
driveNetwork :: Game a => Assets -> (Float -> IO a) -> IO (Maybe Float) -> IO ()
driveNetwork assets network iomdriver = do
  mdriver <- iomdriver

  case mdriver of
    Nothing     -> return ()
    Just driver -> do
      game <- network driver
      renderWrapper $ render assets game
      driveNetwork assets network iomdriver
  where renderWrapper :: Scene -> IO ()
        renderWrapper scene = do
          clear [ColorBuffer]
          scene
          swapBuffers

{-|
  Starting the engine with window parameters described within the provided
  @'WindowConfig'@.
-}
startEngine :: Game a => WindowConfig -> a -> IO ()
startEngine wc game = do
  -- Opening the window
  initialize
  openWindow (Size (fromIntegral $ getWindowWidth wc)
                   (fromIntegral $ getWindowHeight wc))
             [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24]
             (case getWindowFullscreen wc of
                True  -> FullScreen
                False -> Window)

  -- An IORef to keep track of when the window is closed
  closedRef <- newIORef False

  -- Setting window parameters
  windowTitle         $= getWindowTitle wc
  windowCloseCallback $= closeCallback closedRef
  windowSizeCallback  $= resizeCallback

  -- Loading the assets
  assets <- performAssetLoads $ loadAssets game

  -- Creating the input container
  ic <- makeInputContainer

  -- Setting the input callbacks
  mousePosCallback    $= makeMousePosCallback    ic
  mouseButtonCallback $= makeMouseButtonCallback ic
  keyCallback         $= makeKeyCallback         ic

  -- Creating the network
  network <- makeNetwork game $ getInput ic

  -- Starting the network
  time $= 0
  driveNetwork assets network $ runInput closedRef

  -- Closing the window, after all is said and done
  closeWindow

{-|
  Starting the engine with default window parameter.
-}
startEngineDefault :: Game a => a -> IO ()
startEngineDefault = startEngine def
