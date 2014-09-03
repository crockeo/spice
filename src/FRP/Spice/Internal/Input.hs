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
import qualified Data.Map.Strict as Map
import qualified Data.Traversable as T
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Data.Map.Strict ((!))
import Control.Applicative
import FRP.Elerea.Param
import Control.Monad

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
  Making a list of externals from a given list.
-}
makeExternals :: Ord a => [a] -> IO (Map.Map a (Signal Bool, Bool -> IO ()))
makeExternals l = liftM Map.fromList $ mapM (\k -> liftM ((,) k) $ external False) l

{-|
  The external for mouse position.
-}
mousePosExternal :: IO (Signal (Vector Float), Vector Float -> IO ())
mousePosExternal = external $ Vector 0 0

{-|
  A list of all mouse buttons in the GLFW API.
-}
mouseButtons :: [MouseButton]
mouseButtons = [ ButtonLeft
               , ButtonRight
               , ButtonMiddle
               ]
               ++
               map ButtonNo [0 .. 7]

{-|
  A @'Map.Map'@ of @'MouseButton'@ to externals created from the @'mouseButton'@.
-}
mouseButtonExternal :: IO (Map.Map MouseButton (Signal Bool, Bool -> IO ()))
mouseButtonExternal = makeExternals mouseButtons

{-|
  A list of all keys in the GLFW API.
-}
keys :: [Key]
keys = map toEnum [0 .. 318]

{-|
  A @'Map.Map'@ of @'Key'@ to externals created from the @'key'@.
-}
keyExternal :: IO (Map.Map Key (Signal Bool, Bool -> IO ()))
keyExternal = makeExternals keys

{-|
  Making a given @'InputContainer'@.
-}
makeInputContainer :: IO InputContainer
makeInputContainer = do
  (mpSing, mpSink) <- mousePosExternal

  mouseButtonExternal <- mouseButtonExternal
  keyExternal         <- keyExternal

  let mbSing = Map.map fst mouseButtonExternal
      mbSink = Map.map snd mouseButtonExternal
      kSing  = Map.map fst keyExternal
      kSink  = Map.map snd keyExternal

      sing = do
        a <- mpSing
        b <- T.sequence mbSing
        c <- T.sequence kSing

        return $ Input a b c

      sink = Sinks { mousePosSink    = mpSink
                   , mouseButtonSink = mbSink
                   , keySink         = kSink
                   }

  return $ InputContainer sink sing


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
