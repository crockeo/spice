{-|
  This module need not be used directly. Refer to @'FRP.Spice.Engine'@ instead.
-}
module FRP.Spice.Engine.Driver where

--------------------
-- Global Imports --
import Control.Monad

----------
-- Code --

{-|
  Driving a network created with the @'FRP.Spice.Engine.Network.makeNetwork'@
  function and a function such as @'FRP.Spice.Engine.RunInput.runInput'@.
-}
driveNetwork :: (a -> IO (IO ())) -> IO (Maybe a) -> IO ()
driveNetwork network iomdriver = do
  mdriver <- iomdriver

  case mdriver of
    Just driver -> do join $ network driver
                      driveNetwork network iomdriver
    Nothing     -> return ()
