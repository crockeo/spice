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
import FRP.Spice.Input
import FRP.Spice.Game

----------
-- Code --

{-|
  The raw update function that @'makeNetwork'@ calls.
-}
updateFn :: Game a => Float -> Input -> a -> a
updateFn = update

{-|
  Creating a network to be used with the
  @'FRP.Spice.Engine.Driver.driveNetwork'@ function.
-}
makeNetwork :: Game a => Signal Input -> a -> IO (Float -> IO a)
makeNetwork inputSignal game =
  start $ transfer game updateFn inputSignal
