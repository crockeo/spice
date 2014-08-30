module FRP.Spice.Internal.Math.VectorOps where

--------------------
-- Global Imports --
import Control.Applicative

-------------------
-- Local Imports --
import FRP.Spice.Internal.Types

----------
-- Code --

{-|
  Adding two @'Vector'@s.
-}
(^+) :: Num a => Vector a -> Vector a -> Vector a
(^+) v1 v2 = pure (+) <*> v1 <*> v2

{-|
  Multiplying two @'Vector'@s (not a dot product, but rather multiplying the
  first value in the first vector by the second value in the second vector,
  and the same with with the second value.)
-}
(^*) :: Num a => Vector a -> Vector a -> Vector a
(^*) v1 v2 = pure (*) <*> v1 <*> v2

{-|
  Adding a @'Vector'@ and a given number. Effectively the same as calling (^+)
  on a @'Vector'@ and a @'Vector'@ n n.
-}
(^+>) :: Num a => Vector a -> a -> Vector a
(^+>) v n = pure (+) <*> v <*> pure n

{-|
  Multiplying a @'Vector'@ by a given number. Effecitvely the same as calling
  (^*) on a @'Vector'@ and a @'Vector'@ n n.
-}
(^*>) :: Num a => Vector a -> a -> Vector a
(^*>) v n = pure (*) <*> v <*> pure n
