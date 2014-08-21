{-|
  This module need not be used directly. Refer to @'FRP.Spice.Engine'@ instead.
-}
module FRP.Spice.Engine.Driver where

--------------------
-- Global Imports --
import Control.Monad

--------------------
-- Global Imports --
import FRP.Spice.Engine.RenderWrapper
import FRP.Spice.Graphics.Scene
import FRP.Spice.Game

----------
-- Code --

{-|
  Driving a network created with the @'FRP.Spice.Engine.Network.makeNetwork'@
  function and a function such as @'FRP.Spice.Engine.RunInput.runInput'@.
-}
driveNetwork :: Game a => (Float -> IO a) -> IO (Maybe Float) -> IO ()
driveNetwork network iomdriver = do
  mdriver <- iomdriver

  case mdriver of
    Just driver -> do state <- network driver
                      renderWrapper $ render state
                      driveNetwork network iomdriver
    Nothing     -> return ()
