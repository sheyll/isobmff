{-# LANGUAGE UndecidableInstances #-}

-- / Utilities to provide better error messages
--
-- When dealing with type level programming in Haskell, especially when involving
-- undecidable instances, one is often confronted with irritating compiler
-- behaviour. This module shall mitigate the problems.
--
-- A group of types that all 'From' into the same type. This
-- is an /open/ and /extensible/ alternative to defining an algebraic data type
-- and using the promoted constructors. 'A' goes hand-in-hand with 'To' for
-- better readability.
--
-- For example:
--
-- @
-- data PrettyPrinter c where
--   RenderText :: Symbol -> PrettyPrinter Symbol
--   WithColor :: Color -> PrettyPrinter c -> PrettyPrinter c
--
-- data Color = Black | White
--
-- data ColoredText :: Color -> Symbol -> 'To' (PrettyPrinter Symbol)
--
-- type instance 'From' (ColoredText c txt) = 'WithColor c ('RenderText txt)
-- @
module Data.Kind.Extra
  ( type To
  , type Konst
  , type From
  , type (:->)
  , type Apply
  , type ($)
  , type (:>>=:)
  , type (:>>>:)
  , type (:^>>>:)
  , type (:>>>^:)
  , type Extract
  , type Optional
  , type FoldMap
  , type Fun1
  , type Fun2
  , type Fun3
  , type Fun4
  ) where

import Data.Kind (type Type)
import Data.Type.Equality ((:~:))

-- | Indicates that a type constructs another.
type To foo = (foo -> Type :: Type)

-- | An open type family to turn /symbolic/ type representations created with
-- 'A' or 'To' into the actual types.
type family From (t :: foo -> Type) :: foo

-- | A @Konst foo@, @'To' foo@.
data Konst (a :: k) (b :: k)

type instance From ((:~:) a) = a

type instance From (Konst f) = f

-- | A symbolic type-level function.
data (:->) foo bar

-- | An open family of functions from @foo@ to @bar@
type family Apply (f :: To (foo :-> bar)) (x :: foo) :: bar

-- | An open family of functions from @foo@ to @bar@
-- TODO type family App (f :: To (a -> b)) (x :: To a) :: To b

-- | An alias for 'Apply'
type f $ x = Apply f x

-- | Compose functions
data (:>>>:) :: To (good :-> better) -> To (better :-> best) -> To (good :-> best)
infixl 1 :>>>:
type instance Apply (f :>>>: g) x = g $ (f $ x)

-- | From Input & Compose
data (:^>>>:) :: To (good :-> better) -> To (better :-> best) -> To (To good :-> best)
infixl 1 :^>>>:
type instance Apply (f :^>>>: g) x = g $ (f $ From x)

-- | Compose and 'Konst'
data (:>>>^:) :: To (good :-> better) -> To (better :-> best) -> To (good :-> To best)
infixl 1 :>>>^:
type instance Apply (f :>>>^: g) x = Konst (g $ (f $ x))

-- | A function that applies 'From'
data Extract :: To (To x :-> x)
type instance Apply Extract x = From x

-- | From and ApplyCompose functions
data (:>>=:) :: To foo -> To (foo :-> To bar) -> To bar
infixl 1 :>>=:
type instance From (x :>>=: f) = From (f $ From x)

-- | Either use the value from @Just@ or return a fallback value(types(kinds))
data Optional :: To t -> To (s :-> To t) -> To (Maybe s :-> To t)

type instance Apply (Optional fallback f) ('Just s) = f $ s
type instance Apply (Optional fallback f) 'Nothing = fallback

-- | Map over the elements of a list and fold the result.
type family
  FoldMap
          (append :: To (bar :-> To (bar :-> bar)))
          (zero :: bar)
          (f :: To (foo :-> bar))
          (xs :: [(foo :: Type)]) :: (bar :: Type) where
  FoldMap append zero f '[]       = zero
  FoldMap append zero f (x ': xs) = append $ (f $ x) $ FoldMap append zero f xs

--  TODONT safe coercions (with undecidable instances) could be done only with a
--  type-level equivilant of a type class dictionary, A good place for that
--  might be 'A'. For now I only had trouble with 'CoerceTo' because it is open
--  and the compiler often used up __all__ main memory when an instance was
--  missing.

-- | Like @TyCon1@ from Data.Singletons
data Fun1 :: (a -> To b)
            -> To (a :-> To b)
type instance Apply (Fun1 f) x = (f x)

data Fun2 :: (a -> b -> To c)
            -> To (a :-> To (b :-> To c))
type instance Apply (Fun2 f) x = Fun1 (f x)

data Fun3 :: (a -> b -> c -> To d)
            -> To (a :-> To (b :-> To (c :-> To d)))
type instance Apply (Fun3 f) x = Fun2 (f x)

data Fun4 :: (a -> b -> c -> d -> To e)
            -> To (a :-> To (b :-> To (c :-> To (d :-> To e))))
type instance Apply (Fun4 f) x = Fun3 (f x)
