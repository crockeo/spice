{-|
  This module provides a wrapper to use with a given @'FRP.Spice.Game.Game'@'s
  render function.
-}
module FRP.Spice.Engine.RenderWrapper where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

----------
-- Code --

{-|
  A function to be ran on a @'FRP.Spice.Game.Game'@'s render function to
  provide a bit of a framework around it.
-}
renderWrapper :: IO () -> IO ()
renderWrapper renderfn = do
  clear [ColorBuffer]

  renderfn

  flush
  swapBuffers
