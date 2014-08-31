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
import Graphics.UI.GLFW as GLFW

-------------------
-- Local Imports --
import FRP.Spice.Internal.Types

----------
-- Code --

{-|
  Making a given @'InputContainer'@.
-}
makeInputContainer :: IO InputContainer
makeInputContainer = error "makeInputContainer undefined."

{-|
  Making the mousePosCallback function.
-}
makeMousePosCallback :: InputContainer -> MousePosCallback
makeMousePosCallback _ = undefined

{-|
  Making the mouseButtonCallback function.
-}
makeMouseButtonCallback :: InputContainer -> MouseButtonCallback
makeMouseButtonCallback _ = undefined

{-|
  Making the keyCallback function.
-}
makeKeyCallback :: InputContainer -> KeyCallback
makeKeyCallback _ = undefined
