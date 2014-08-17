module FRP.Spice.Input.Keyboard where

--------------------
-- Global Imports --
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
externals = liftM fromList $ sequence $ map (\k -> liftM ((,) k) $ external False) keys

-- Getting the signals from the externals
signals :: Map Key (Signal Bool, Bool -> IO ()) -> IO (Signal (Map Key Bool))
signals map = undefined

-- Getting the sinks from the externals
sinks :: Map Key (Signal Bool, Bool -> IO ()) -> IO (Map Key (Bool -> IO ()))
sinks map = return $ fmap (snd) map

-- Updating the sinks
updateSinks :: Map Key (Bool -> IO ()) -> IO ()
updateSinks map =
  forM_ keys $ \k -> do
    val <- getKey k
    map ! k $ case val of
                Press   -> True
                Release -> False
