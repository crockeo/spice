module FRP.Spice.Engine.Network where

--------------------
-- Global Imports --
import Control.Applicative
import FRP.Elerea.Param

-------------------
-- Local Imports --
import FRP.Spice.Input
import FRP.Spice.Game

----------
-- Code --

-- Making the network
makeNetwork :: Game a => Signal Input -> Signal a -> (a -> IO ()) -> IO (Float -> IO (IO ()))
makeNetwork inputSignal gameSignal gameSink =
  start $ memo $ do
    input <- inputSignal
    game  <- gameSignal

    return $ do
      gameSink $ update 0.01 input game
      render game
