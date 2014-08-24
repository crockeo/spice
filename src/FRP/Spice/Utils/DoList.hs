{-|
  This module provides a list that can be composed and concatenated through
  do-notation.


-}
module FRP.Spice.Utils.DoList where

--------------------
-- Global Imports --
import Control.Applicative
import Data.Monoid

----------
-- Code --

{-|
  The datastructure itself. Almost equivalent to a tuple of length 2 without
  and more context.
-}
data DoListT a b = DoListT a b

{-|
  A more commonly used synonym for DoListT
-}
type DoList a = DoListT a ()

{-|
  Getting the elements from a @'DoListT'@.
-}
values :: DoListT a b -> a
values (DoListT a _) = a

{-|
  Constructing a do-list from a @'Monoid'@.
-}
fromValues :: Monoid a => a -> DoListT a ()
fromValues a = DoListT a ()

{-|
  A functor instance to satisfy @'Applicative'@'s requirements.
-}
instance Functor (DoListT a) where
  fmap fn (DoListT a b) =
    DoListT a $ fn b

{-|
  An applicative instance to satisfy @'Monad'@'s requirements.
-}
instance Monoid a => Applicative (DoListT a) where
  pure b = DoListT mempty b
  (DoListT _ fn) <*> dl = fmap fn dl

{-|
  A monad instance to perform the actual composition of the list. List is a
  loose word seeing as the monad instance simply needs an instance of
  @'Monoid'@.
-}
instance Monoid a => Monad (DoListT a) where
  return = pure
  (DoListT a b) >>= fn =
    let (DoListT a' b') = fn b in
      DoListT (mappend a a') b'
