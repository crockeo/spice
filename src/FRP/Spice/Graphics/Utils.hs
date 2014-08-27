{-|
  This module provides a set of utilities to ease the burden of writing OpenGL
  rendering functions.
-}
module FRP.Spice.Graphics.Utils where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL

----------
-- Code --

{-|
  Converting a @'Float'@ to a @'GLfloat'@
-}
togl :: Float -> GLfloat
togl = realToFrac

{-|
  Converting a @'GLfloat'@ to a @'Float'@.
-}
fromgl :: GLfloat -> Float
fromgl = realToFrac
