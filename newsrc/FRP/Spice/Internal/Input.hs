{-|
  This module provides functions having to do with the creating and handling of
  input.
-}
module FRP.Spice.Internal.Input ( makeInputContainer
                                , makeMousePosCallback
                                , makeMouseButtonCallback
                                , makeKeyCallback
                                ) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Data.Map.Strict ((!))

-------------------
-- Local Imports --
import FRP.Spice.Internal.Types

----------
-- Code --

{-|
  An intentionally orphan instance of @'Ord'@ for @'MouseButton'@ so that it
  may be used in a Map.
-}
instance Ord MouseButton where
  mb1 <= mb2 =
    fromEnum mb1 <= fromEnum mb2

{-|
  Making a given @'InputContainer'@.
-}
makeInputContainer :: IO InputContainer
makeInputContainer = error "makeInputContainer undefined."

{-|
  Making the mousePosCallback function.
-}
makeMousePosCallback :: InputContainer -> MousePosCallback
makeMousePosCallback ic (Position x y) = do
  (Size w h) <- get windowSize
  (mousePosSink $ getSinks ic) $
    Vector (  fromIntegral x  / 320 - ((fromIntegral w) / 640))
           ((-fromIntegral y) / 240 + ((fromIntegral h) / 480))

{-|
  Making the mouseButtonCallback function.
-}
makeMouseButtonCallback :: InputContainer -> MouseButtonCallback
makeMouseButtonCallback ic inputMouseButton state =
  (mouseButtonSink $ getSinks ic) ! inputMouseButton $
    case state of
      Press   -> True
      Release -> False

{-|
  Making the keyCallback function.
-}
makeKeyCallback :: InputContainer -> KeyCallback
makeKeyCallback ic inputKey state =
  (keySink $ getSinks ic) ! inputKey $
    case state of
      Press   -> True
      Release -> False
