{-|
  The backend to be used with the input.
-}
module FRP.Spice.Input.Backend where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import FRP.Elerea.Param
import Data.Map.Strict

-------------------
-- Local Imports --
import qualified FRP.Spice.Input.MousePosition as MousePosition
import qualified FRP.Spice.Input.Keyboard      as Keyboard
import qualified FRP.Spice.Input.Mouse         as Mouse
import FRP.Spice.Config
import FRP.Spice.Input
import FRP.Spice.Math

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
makeMousePositionCallback :: WindowConfig -> InputContainer -> MousePosCallback
makeMousePositionCallback wc ic (Position x y) =
  mousePositionSinks (getSinks ic) $
    Vector (  fromIntegral x  / ((fromIntegral $ getWindowWidth  wc) / 2) - 1)
           ((-fromIntegral y) / ((fromIntegral $ getWindowHeight wc) / 2) + 1)

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
