{-|
  This module provides a number of functions having to do with mathematical
  calculation in the context of the Spice library.
-}
module FRP.Spice.Internal.Math where

--------------------
-- Global Imports --
import Control.Applicative

-------------------
-- Local Imports --
import FRP.Spice.Internal.Types

----------
-- Code --

{-|
  The cardinal directions.
-}
up, down, left, right :: Num a => Vector a
up    = Vector ( 0) ( 1)
down  = Vector ( 0) (-1)
left  = Vector (-1) ( 0)
right = Vector ( 1) ( 0)


{-|
  Adding two @'Vector'@s.
-}
infixl 6 ^+
(^+) :: Num a => Vector a -> Vector a -> Vector a
(^+) v1 v2 = pure (+) <*> v1 <*> v2

{-|
  Subtracting a @'Vector'@ from another @'Vector'@.
-}
infixl 6 ^-
(^-) :: Num a => Vector a -> Vector a -> Vector a
(^-) v1 v2 = pure (-) <*> v1 <*> v2

{-|
  Multiplying two @'Vector'@s (not a dot product, but rather multiplying the
  first value in the first vector by the second value in the second vector,
  and the same with with the second value.)
-}
infixl 7 ^*
(^*) :: Num a => Vector a -> Vector a -> Vector a
(^*) v1 v2 = pure (*) <*> v1 <*> v2

{-|
  Adding a @'Vector'@ and a given number. Effectively the same as calling (^+)
  on a @'Vector'@ and a @'Vector'@ n n.
-}
infixl 6 ^+>
(^+>) :: Num a => Vector a -> a -> Vector a
(^+>) v n = pure (+) <*> v <*> pure n

{-|
  Subtracting a number from a @'Vector'@. Effectively the same as calling (^-)
  on a @'Vector'@ and a @'Vector'@ n n.
-}
infixl 6 ^->
(^->) :: Num a => Vector a -> a -> Vector a
(^->) v n = pure (-) <*> v <*> pure n

{-|
  Multiplying a @'Vector'@ by a given number. Effecitvely the same as calling
  (^*) on a @'Vector'@ and a @'Vector'@ n n.
-}
infixl 7 ^*>
(^*>) :: Num a => Vector a -> a -> Vector a
(^*>) v n = pure (*) <*> v <*> pure n
