{-|
  This module provides everything relating to input in the scope of spice.
-}
module FRP.Spice.Input ( module Rexport
                       , Sinks (..)
                       , Input (..)
                       , InputContainer (..)
                       , makeInputContainer
                       , makeMousePositionCallback
                       , makeKeyboardCallback
                       , makeMouseCallback
                       ) where

--------------------
-- Global Imports --
import Data.Map.Strict hiding (keys, map)
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Control.Applicative
import FRP.Elerea.Param

-------------------
-- Local Imports --
import qualified FRP.Spice.Input.MousePosition as MousePosition
import qualified FRP.Spice.Input.Keyboard as Keyboard
import qualified FRP.Spice.Input.Mouse as Mouse
import FRP.Spice.Math.Vector

-----------------------
-- Rexported Imports --
import Data.Map.Strict  as Rexport ((!))
import Graphics.UI.GLFW as Rexport (Key (..), SpecialKey (..), MouseButton (..))

----------
-- Code --

{-|
  A wrapper around the sinks for the mouse position, the key states, and the
  mouse button states.
-}
data Sinks = Sinks { mousePositionSinks :: Vector Float -> IO ()
                   , keyboardSinks      :: Map Key (Bool -> IO ())
                   , mouseSinks         :: Map MouseButton (Bool -> IO ())
                   }

{-|
  A container for all of the states themselves. It is used as a @'Signal'@
  @'Input'@ in the @'InputContainer'@ (which is necessary to use it within
  Elerea's FRP network).
-}
data Input = Input { mousePosition :: Vector Float
                   , keyboard      :: Map Key Bool
                   , mouse         :: Map MouseButton Bool
                   }

{-|
  A container around @'Sinks'@ and @'Signal'@ @'Input'@ so that one needn't
  pass around a tuple.
-}
data InputContainer = InputContainer { getSinks :: Sinks
                                     , getInput :: Signal Input
                                     }

{-|
  Making an @'InputContainer'@ filled with all necessary externals.
-}
makeInputContainer :: IO InputContainer
makeInputContainer = do
  mousePositionExternals <- MousePosition.externals
  keyboardExternals      <- Keyboard.externals
  mouseExternals         <- Mouse.externals

  let sinks = Sinks { mousePositionSinks = MousePosition.sinks mousePositionExternals
                    , keyboardSinks      = Keyboard.sinks keyboardExternals
                    , mouseSinks         = Mouse.sinks mouseExternals
                    }

  let input = do mps <- fst mousePositionExternals
                 kbs <- Keyboard.signals keyboardExternals
                 ms  <- Mouse.signals mouseExternals

                 return Input { mousePosition = mps
                              , keyboard      = kbs
                              , mouse         = ms
                              }

  return InputContainer { getSinks = sinks
                        , getInput = input
                        }

{-|
  Creating a callback to update the mouse position's state.
-}
makeMousePositionCallback :: InputContainer -> MousePosCallback
makeMousePositionCallback ic (Position x y) =
  mousePositionSinks (getSinks ic) $ Vector (fromIntegral x / 320 - 1) ((-fromIntegral y) / 240 - 1)

{-|
  Creating a callback to update the keyboard's states.
-}
makeKeyboardCallback :: InputContainer -> KeyCallback
makeKeyboardCallback ic key state =
  keyboardSinks (getSinks ic) ! key $ case state of
    Press   -> True
    Release -> False

{-|
  Creating a callback to update the mouse buttons' states.
-}
makeMouseCallback :: InputContainer -> MouseButtonCallback
makeMouseCallback ic button state =
  mouseSinks (getSinks ic) ! button $ case state of
    Press   -> True
    Release -> False
