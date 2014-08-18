module FRP.Spice.Input where

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

----------
-- Code --

-- The sinks
data Sinks = Sinks { mousePositionSinks :: Vector Float -> IO ()
                   , keyboardSinks      :: Map Key (Bool -> IO ())
                   , mouseSinks         :: Map MouseButton (Bool -> IO ())
                   }

data Input = Input { mousePosition :: Vector Float
                   , keyboard      :: Map Key Bool
                   , mouse         :: Map MouseButton Bool
                   }

-- The container to go around the
data InputContainer = InputContainer { getSinks :: Sinks
                                     , getInput :: Signal Input
                                     }

-- Creating the input container for use in the program
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

-- Creating callbacks for each type
makeMousePositionCallback :: InputContainer -> MousePosCallback
makeMousePositionCallback ic (Position x y) =
  mousePositionSinks (getSinks ic) $ Vector (fromIntegral x / 320 - 1) ((-fromIntegral y) / 240 - 1)

makeKeyboardCallback :: InputContainer -> KeyCallback
makeKeyboardCallback ic key state =
  keyboardSinks (getSinks ic) ! key $ case state of
    Press   -> True
    Release -> False

makeMouseCallback :: InputContainer -> MouseButtonCallback
makeMouseCallback ic button state =
  mouseSinks (getSinks ic) ! button $ case state of
    Press   -> True
    Release -> False
