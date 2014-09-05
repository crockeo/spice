{-|
  This module, similarly to the Graphics module, only it provides the ability
  for one to play sounds.
-}
module FRP.Spice.Internal.Sound ( playSound
                                , loopSound
                                ) where

-------------------
-- Local Imports --
import FRP.Spice.Internal.Types

----------
-- Code --

{-|
  Playing a sound.
-}
playSound :: Sound -> Scene
playSound _ = return ()

{-|
  Looping a sound.
-}
loopSound :: Sound -> Scene
loopSound _ = return ()
