module FRP.Spice.Engine.Driver where

--------------------
-- Global Imports --
import Control.Monad

----------
-- Code --

-- Driving a given network
driveNetwork :: (a -> IO (IO ())) -> IO (Maybe a) -> IO ()
driveNetwork network iomdriver = do
  mdriver <- iomdriver

  case mdriver of
    Just driver -> do join $ network driver
                      driveNetwork network iomdriver
    Nothing     -> return ()
