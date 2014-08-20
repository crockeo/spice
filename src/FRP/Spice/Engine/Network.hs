{-|
  This module need not be used directly. Refer to
  @'FRP.Spice.Engine.startEngine'@ instead.
-}
module FRP.Spice.Engine.Network where

--------------------
-- Global Imports --
import FRP.Elerea.Param

-------------------
-- Local Imports --
import FRP.Spice.Engine.RenderWrapper
import FRP.Spice.Input
import FRP.Spice.Game

----------
-- Code --

{-|
  Creating a network to be used with the
  @'FRP.Spice.Engine.Driver.driveNetwork'@ function.
-}
makeNetwork :: Game a => Signal Input -> Signal a -> (a -> IO ()) -> IO (Float -> IO (IO ()))
makeNetwork inputSignal gameSignal gameSink =
  start $ memo $ do
    input <- inputSignal
    game  <- gameSignal

    return $ do
      gameSink $ update 0.01 input game
      renderWrapper $ render game
