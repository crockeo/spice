{-|
  This module provides an abstraction over the default representations of color
  in the Haskell OpenGL bindings.
-}
module FRP.Spice.Graphics.Color  where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL ( Color4 (..)
                                 , color
                                 )

-------------------
-- Local Imports --
import FRP.Spice.Graphics.Scene
import FRP.Spice.Graphics.Utils

----------
-- Code --

{-|
  Representing a Color using four @'Float'@ representing reg, green, blue, and
  the alpha mask respectively. The @'Float'@s are in a range from 0-1,
  representing @'Int'@s from 0-255.
-}
data Color = Color { getRed   :: Float
                   , getGreen :: Float
                   , getBlue  :: Float
                   , getAlpha :: Float
                   }
  deriving (Eq, Show, Read)

{-|
  Converting a color to an action in a @'Scene'@.
-}
bindColor :: Color -> Scene
bindColor (Color r g b a) =
  color $ Color4 (togl r) (togl g) (togl b) (togl a)

{-|
  A synonym for the @'Color'@ constructor.
-}
color4f :: Float -> Float -> Float -> Float -> Color
color4f = Color

{-|
  Constructing a @'Color'@ from 3 @'Float'@s, defaulting the alpha mask to 1.0.
-}
color3f :: Float -> Float -> Float -> Color
color3f r g b = color4f r g b 1.0

{-|
  Creating a @'Color'@ from 4 @'Int'@s. The ints, similarly to @'color4f'@
  represent red, green, blue, and the alpha mask respectively. The ints should
  be in the range of 0-255. (Note: @'color4i'@ is functionally equivalent (and
  also equivalent in source code) to calling color4f with each of its arguments
  divided by 255.)
-}
color4i :: Int -> Int -> Int -> Int -> Color
color4i r g b a = color4f (fromIntegral r / 255)
                          (fromIntegral g / 255)
                          (fromIntegral b / 255)
                          (fromIntegral a / 255)

{-|
  Constructing a @'Color'@ from 3 @'Int'@s, defaulting the alpha mask to 255.
-}
color3i :: Int -> Int -> Int -> Color
color3i r g b = color4i r g b 255

{-|
  The color black.
-}
black :: Color
black = color3i 0 0 0

{-|
  The color white.
-}
white :: Color
white = color3i 255 255 255

{-|
  The color red.
-}
red :: Color
red = color3i 255 0 0

{-|
  The color green.
-}
green :: Color
green = color3i 0 255 0

{-|
  The color blue.
-}
blue :: Color
blue = color3i 0 0 255
