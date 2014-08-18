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

-- A set of all keys available via GLFW
keys :: [Key]
keys = map toEnum [0 .. 318]

-- Getting the externals for all of the keys
externals :: IO (Map Key (Signal Bool, Bool -> IO ()))
externals = liftM fromList $ mapM (\k -> liftM ((,) k) $ external False) keys

-- Getting the signals from the externals
signals :: Map Key (Signal Bool, Bool -> IO ()) -> Signal (Map Key Bool)
signals = T.sequence . fmap fst

-- Getting the sinks from the externals
sinks :: Map Key (Signal Bool, Bool -> IO ()) -> Map Key (Bool -> IO ())
sinks = fmap snd
