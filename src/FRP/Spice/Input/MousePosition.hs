{-|
  The mouse (position) specific section of input.
-}
module FRP.Spice.Input.MousePosition where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import FRP.Elerea.Param
import Data.Default

-------------------
-- Local Imports --
import FRP.Spice.Math.Vector

-- Getting the external for the mouse position
externals :: IO (Signal (Vector Float), Vector Float -> IO ())
externals = external def

-- Getting the signal from the external
signals :: (Signal (Vector Float), Vector Float -> IO ()) -> Signal (Vector Float)
signals = fst

-- Getting the signal from the external
sinks :: (Signal (Vector Float), Vector Float -> IO ()) -> Vector Float -> IO ()
sinks = snd
