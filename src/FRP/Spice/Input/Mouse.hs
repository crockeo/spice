{-|
  The mouse (button) specific section of input.
-}
module FRP.Spice.Input.Mouse where

--------------------
-- Global Imports --
import qualified Data.Traversable as T (sequence)
import Data.Map.Strict hiding (keys, map)
import Graphics.UI.GLFW as GLFW
import FRP.Elerea.Param
import Control.Monad

----------
-- Code --

{-|
  Adding an instance of Ord to MouseButton so it can be used with the map.
-}
instance Ord MouseButton where
  mb1 <= mb2 = fromEnum mb1 < fromEnum mb2

{-|
  A list of all mouse buttons available via the GLFW api.
-}
buttons :: [MouseButton]
buttons = [ ButtonLeft
          , ButtonRight
          , ButtonMiddle
          ]
          ++
          map ButtonNo [0 .. 7]

{-|
  A map from @'MouseButton'@ to externals created for every button.
-}
externals :: IO (Map MouseButton (Signal Bool, Bool -> IO ()))
externals = liftM fromList $ mapM (\b -> liftM ((,) b) $ external False) buttons

{-|
  Creating the @'Signal'@ of a @'Map'@ from @'MouseButton'@ to @'Bool'@ from a
  @'Map'@ of @'externals'@.
-}
signals :: Map MouseButton (Signal Bool, Bool -> IO ()) -> Signal (Map MouseButton Bool)
signals = T.sequence . fmap fst

{-|
  Making a @'Map'@ from @'MouseButton'@ to sink from a @'Map'@ of
  @'externals'@.
-}
sinks :: Map MouseButton (Signal Bool, Bool -> IO ()) -> Map MouseButton (Bool -> IO ())
sinks = fmap snd
