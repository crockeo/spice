{-|
  This module provides every single type defined within the spice framework.
  This allows people to write functions on data structures without being
  concerned about recursive imports (and thereby being unable to compile) due
  to the fact that this file does not and WILL NEVER import any other local libraries.
-}
module FRP.Spice.Internal.Types where

--------------------
-- Global Imports --
import qualified Data.Map.Strict as Map
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Control.Applicative
import FRP.Elerea.Param
import Data.Default
import Data.Monoid

----------
-- Code --

{-|
  A data type that can be composed (so long as the data stored is a Monoid) in
  do-notation.
-}
data DoListT a b = DoListT a b

{-|
  A type synonym for the @'DoListT'@ that should be used.
-}
type DoList a = DoListT a ()

{-|
  A Functor instance to satisfy the Applicative's requirements.
-}
instance Functor (DoListT a) where
  fmap fn (DoListT a b) = DoListT a $ fn b

{-|
  An Applicative instance to satisfy the Monad's requirements in the future.
-}
instance Monoid a => Applicative (DoListT a) where
  pure b = DoListT mempty b
  (DoListT a' bfn) <*> (DoListT a b) =
    DoListT (a `mappend` a') $ bfn b

{-|
  The Monad instance to allow the DoList to compose through do-notation.
-}
instance Monoid a => Monad (DoListT a) where
  return = pure
  (DoListT a b) >>= fn =
    let (DoListT a' b') = fn b in
      DoListT (mappend a a') b'

{-|
  The config that is used to define the GLFW window's properties.
-}
data WindowConfig = WindowConfig { getWindowWidth      :: Int
                                 , getWindowHeight     :: Int
                                 , getWindowFullscreen :: Bool
                                 , getWindowTitle      :: String
                                 }

{-|
  The default state for a @'WindowConfig'@.
-}
instance Default WindowConfig where
  def = WindowConfig { getWindowWidth      = 640
                     , getWindowHeight     = 480
                     , getWindowFullscreen = False
                     , getWindowTitle      = "Spice Application"
                     }

{-|
  A data type representing a color as 4 floats (r, g, b, a) representing red,
  green, blue, and the alpha channel respectively. The floats should range from
  0 to 1, representing 0 to 255 in more classical representations.
-}
data Color = Color { colorRed   :: Float
                   , colorGreen :: Float
                   , colorBlue  :: Float
                   , colorAlpha :: Float
                   }

{-|
  A data type that stores two values. It can be used to perform basic vector
  logic. It should be noted that the @'Vector'@ logic provided in this library
  is not linear-algebra style vector logic.
-}
data Vector a = Vector a a

{-|
  The default state for a @'Vector'@.
-}
instance Default a => Default (Vector a) where
  def = Vector def def

{-|
  A functor instance to apply a function onto both values stored in a
  @'Vector'@.
-}
instance Functor Vector where
  fmap fn (Vector x y) = Vector (fn x) (fn y)

{-|
  An applicative instance to allow one to write functions on @'Vector'@s that
  have separate functions for both values.
-}
instance Applicative Vector where
  pure a = Vector a a
  (Vector fnx fny) <*> (Vector x y) =
    Vector (fnx x) (fny y)

{-|
  A wrapper around the sinks for the mouse position, mouse buttons, and
  keyboard keys.
-}
data Sinks = Sinks { mousePosSink    :: Vector Float -> IO ()
                   , mouseButtonSink :: Map.Map MouseButton (Bool -> IO ())
                   , keySink         :: Map.Map Key         (Bool -> IO ())
                   }

{-|
  A data structure that represents the current state of the input. Used in the
  @'InputContainer'@ along with the @'Sink'@ to handle all input -- updating
  and referencing.
-}
data Input = Input { mousePos    :: Vector Float
                   , mouseButton :: Map.Map MouseButton Bool
                   , key         :: Map.Map Key         Bool
                   }

{-|
  The container for both @'Sink'@ and @'Input'@. The @'Input'@ is stored within
  a @'Signal'@ so it may be used within the FRP/Elerea part of the framework.
-}
data InputContainer = InputContainer { getSinks :: Sinks
                                     , getInput :: Signal Input
                                     }

{-|
  Containing all of the necessary information for rendering an image on screen
  (aside from the position where the sprite should be rendered.)
-}
data Sprite = Sprite { spriteTex  :: TextureObject
                     , spriteSize :: Vector Float
                     }

{-|
  Representing the loading of an asset into the game framework.
-}
data LoadAsset = LoadSprite FilePath

{-|
  A @'DoList'@ synonym for @'LoadAsset'@.
-}
type LoadAssets = DoList [LoadAsset]

{-|
  Storing the loaded assets in a single data structure.
-}
data Assets = Assets { sprites :: Map.Map FilePath Sprite }

{-|
  The default state for an @'Assets'@.
-}
instance Default Assets where
  def = Assets { sprites = Map.fromList [] }

{-|
  A type synonym to imply that functions performed in this function should
  solely render.
-}
type Scene = IO ()

{-|
  A type synonym to make the delta time (in the @'Game'@ definition) more self
  documenting.
-}
type DeltaTime = Float

{-|
  The requirements for a given data structure to be used as a game within the
  framework.
-}
class Game a where
  update :: DeltaTime -> Input -> a -> a
  render :: Assets -> a -> Scene
  loadAssets :: a -> LoadAssets
