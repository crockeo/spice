{-|
  This module provides a wrapper to use with a given @'FRP.Spice.Game.Game'@'s
  render function.
-}
module FRP.Spice.Engine.RenderWrapper where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

--------------------
-- Global Imports --
import FRP.Spice.Graphics.Scene

----------
-- Code --

{-|
  A function to be ran on a @'FRP.Spice.Game.Game'@'s render function to
  provide a bit of a framework around it. It runs @'clear'@ before the render
  function, and @'flush'@ / @'swapBuffers'@ afterwards.
-}
renderWrapper :: Scene -> IO ()
renderWrapper scene = do
  clear [ColorBuffer]

  renderScene scene

  flush
  swapBuffers
