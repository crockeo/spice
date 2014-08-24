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
renderSprite :: Sprite -> Vector Float -> Scene
renderSprite sprite pos = undefined

{-|
  Rendering a @'Sprite'@, similar to @'renderSprite'@, but with the addition of
  being able to specify the size.
-}
renderSpriteWithSize :: Sprite -> Vector Float -> Vector Float -> Scene
renderSpriteWithSize sprite pos size = undefined

{-|
  Rendering a @'Sprite'@, similarly to @'renderSprite'@, but allowing the
  ability to specify the desired rotation (in the form of a @'Float'@ ranging
  from 0-360. Anything below that range will become 360-n, anything above that
  range will be n-360.)
-}
renderSpriteWithRotation :: Sprite -> Vector Float -> Float -> Scene
renderSpriteWithRotation sprite pos rot = undefined
