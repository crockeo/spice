{-|
  This module handles the configuration for the project.
-}
module FRP.Spice.Config where

--------------------
-- Global Imports --
import Data.Default

----------
-- Code --

{-|
  A datatype to configure the window settings when creating an OpenGL context
  using @'FRP.Spice.Engine.startEngine'@ in the engine.
-}
data WindowConfig = WindowConfig { getWindowWidth      :: Int
                                 , getWindowHeight     :: Int
                                 , getWindowFullscreen :: Bool
                                 , getWindowResizable  :: Bool
                                 , getWindowTitle      :: String
                                 }
  deriving (Eq, Show, Read)

{-|
  The default for @'WindowConfig'@

  > getWindowWidth      = 640
  > getWindowHeight     = 480
  > getWindowFullscreen = False
  > getWindowResizeable = False
  > getWindowTitle      = "Spice Application"
-}
defaultWindowConfig :: WindowConfig
defaultWindowConfig = WindowConfig { getWindowWidth      = 640
                                   , getWindowHeight     = 480
                                   , getWindowFullscreen = False
                                   , getWindowResizable  = False
                                   , getWindowTitle      = "Spice Application"
                                   }

instance Default WindowConfig where
  def = defaultWindowConfig
