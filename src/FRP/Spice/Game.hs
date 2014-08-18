module FRP.Spice.Game where

-------------------
-- Local Imports --
import FRP.Spice.Input

----------
-- Code --

-- The game instance, to be used with updating and rendering
class Game a where
  update :: Input -> a -> a
  render :: a -> IO ()
