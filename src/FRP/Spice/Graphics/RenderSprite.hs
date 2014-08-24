{-|
  This module provides functions to render the
  @'FRP.Spice.Graphics.Sprite.Sprite'@ type.
-}
module FRP.Spice.Graphics.RenderSprite where

-------------------
-- Local Imports --
import FRP.Spice.Graphics.Sprite
import FRP.Spice.Graphics.Scene
import FRP.Spice.Math

----------
-- Code --

{-|
  Rendering a given @'Sprite'@ at a given position specified by a @'Vector'@
  @'Float'@.
-}
renderSprite :: Sprite -> Vector Float -> Vector Float -> Scene
renderSprite sprite pos size = fromElements [RenderSprite sprite pos size]
