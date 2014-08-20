{-|
  This module provides the API which people must use to define a game within
  the scope of Spice.
-}
module FRP.Spice.Game where

-------------------
-- Local Imports --
import FRP.Spice.Graphics.Scene
import FRP.Spice.Input

----------
-- Code --

{-|
  A synonym to make the update function more self-documenting.
-}
type DeltaTime = Float

{-|
  The class which is to be used in the @'FRP.Spice.Engine.startEngine'@
  function. @'update'@ provides the API to update on every tick (purely), an
   @'render'@ provides the API to render every frame.
-}
class Game a where
  update :: DeltaTime -> Input -> a -> a
  render :: a -> Scene
