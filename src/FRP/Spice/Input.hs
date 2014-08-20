{-|
  This module provides everything relating to input in the scope of spice.
-}
module FRP.Spice.Input where

--------------------
-- Global Imports --
import Graphics.UI.GLFW
import Data.Map.Strict

-------------------
-- Local Imports --
import FRP.Spice.Math

----------
-- Code --

{-|
  A container for all of the states themselves. It is used as a @'Signal'@
  @'Input'@ in the @'InputContainer'@ (which is necessary to use it within
  Elerea's FRP network).
-}
data Input = Input { mousePosition :: Vector Float
                   , keyboard      :: Map Key Bool
                   , mouse         :: Map MouseButton Bool
                   }
