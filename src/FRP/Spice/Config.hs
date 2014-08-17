module FRP.Spice.Config where

--------------------
-- Global Imports --
import Data.Default

----------
-- Code --

-- The window config
data WindowConfig = WindowConfig { windowWidth      :: Int
                                 , windowHeight     :: Int
                                 , windowFullscreen :: Bool
                                 , windowResizable  :: Bool
                                 , windowTitle      :: String
                                 }
  deriving (Eq, Show, Read)

-- The default window config
defaultWindowConfig :: WindowConfig
defaultWindowConfig = WindowConfig { windowWidth      = 640
                                   , windowHeight     = 480
                                   , windowFullscreen = False
                                   , windowResizable  = False
                                   , windowTitle      = "Spice Application"
                                   }

-- A default instance for the window
-- config
instance Default WindowConfig where
  def = defaultWindowConfig
