{-|
  The keyboard specific section of input.
-}
module FRP.Spice.Input.Keyboard where

--------------------
-- Global Imports --
import qualified Data.Traversable as T
import Data.Map.Strict hiding (keys, map)
import Graphics.UI.GLFW as GLFW
import FRP.Elerea.Param
import Control.Monad

----------
-- Code --

{-|
  A list of all possible keys used in the GLFW API.
-}
keys :: [Key]
keys = map toEnum [0 .. 318]

{-|
  A Map from @'Key'@ to the externals created for every single key.
-}
externals :: IO (Map Key (Signal Bool, Bool -> IO ()))
externals = liftM fromList $ mapM (\k -> liftM ((,) k) $ external False) keys

{-|
  Creating the @'Signal'@ of a @'Map'@ from @'Key'@ to @'Bool'@ from a @'Map'@
  of @'externals'@.
-}
signals :: Map Key (Signal Bool, Bool -> IO ()) -> Signal (Map Key Bool)
signals = T.sequence . fmap fst

{-|
  Making a @'Map'@ from @'Key'@ to sink from a @'Map'@ of @'externals'@.
-}
sinks :: Map Key (Signal Bool, Bool -> IO ()) -> Map Key (Bool -> IO ())
sinks = fmap snd
