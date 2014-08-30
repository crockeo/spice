{-|
  This module contains all of the functions to create and start the engine. The
  only exposed function is @'startEngine'@ seeing as every other function is to
  be used solely internally within the module.
-}
module FRP.Spice.Internal.Engine ( startEngine
                                 , startEngineDefault
                                 ) where

--------------------
-- Global Imports --
import Data.Default

-------------------
-- Local Imports --
import FRP.Spice.Internal.Types

----------
-- Code --

{-|
  Starting the engine with window parameters described within the provided
  @'WindowConfig'@.
-}
startEngine :: Game a => WindowConfig -> a -> IO ()
startEngine = error "startEngine undefined"

{-|
  Starting the engine with default window parameters.
-}
startEngineDefault :: Game a => a -> IO ()
startEngineDefault = startEngine def