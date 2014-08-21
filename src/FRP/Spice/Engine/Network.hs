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
  The actual update function to be used.
-}
updateFn :: Game a => (a -> IO ()) -> Float -> Input -> a -> IO () -> IO ()
updateFn gameSink dt input game _ = do
  gameSink $ update dt input game
  renderWrapper $ render game

{-|
  Creating a network to be used with the
  @'FRP.Spice.Engine.Driver.driveNetwork'@ function.
-}
makeNetwork :: Game a => Signal Input -> Signal a -> (a -> IO ()) -> IO (Float -> IO (IO ()))
makeNetwork inputSignal gameSignal gameSink =
  start $ transfer2 (return ()) (updateFn gameSink) inputSignal gameSignal

{-
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
-}
