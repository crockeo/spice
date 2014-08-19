{-|
  The mouse (button) specific section of input.
-}
module FRP.Spice.Input.Mouse where

--------------------
-- Global Imports --
import qualified Data.Traversable as T (sequence)
import Data.Map.Strict hiding (keys, map)
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import FRP.Elerea.Param
import Control.Monad
import Data.Default

-------------------
-- Local Imports --
import FRP.Spice.Math.Vector

----------
-- Code --

-- Adding an instance of Ord to MouseButton so it can be used with the map
instance Ord MouseButton where
  mb1 <= mb2 = fromEnum mb1 < fromEnum mb2

-- A set of all mouse buttons available via GLFW
buttons :: [MouseButton]
buttons = [ ButtonLeft
          , ButtonRight
          , ButtonMiddle
          ]
          ++
          map ButtonNo [0 .. 7]

-- Getting externals for all of the buttons
externals :: IO (Map MouseButton (Signal Bool, Bool -> IO ()))
externals = liftM fromList $ mapM (\b -> liftM ((,) b) $ external False) buttons

-- Getting the signals from the externals
signals :: Map MouseButton (Signal Bool, Bool -> IO ()) -> Signal (Map MouseButton Bool)
signals = T.sequence . fmap fst

-- Getting the sinks from the externals
sinks :: Map MouseButton (Signal Bool, Bool -> IO ()) -> Map MouseButton (Bool -> IO ())
sinks = fmap snd
