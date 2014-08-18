module FRP.Spice.Config where

--------------------
-- Global Imports --
import Data.Default

----------
-- Code --

-- The window config
data WindowConfig = WindowConfig { getWindowWidth      :: Int
                                 , getWindowHeight     :: Int
                                 , getWindowFullscreen :: Bool
                                 , getWindowResizable  :: Bool
                                 , getWindowTitle      :: String
                                 }
  deriving (Eq, Show, Read)

-- The default window config
defaultWindowConfig :: WindowConfig
defaultWindowConfig = WindowConfig { getWindowWidth      = 640
                                   , getWindowHeight     = 480
                                   , getWindowFullscreen = False
                                   , getWindowResizable  = False
                                   , getWindowTitle      = "Spice Application"
                                   }

-- A default instance for the window
-- config
instance Default WindowConfig where
  def = defaultWindowConfig
