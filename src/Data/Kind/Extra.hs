{-# LANGUAGE UndecidableInstances #-}

-- / Utilities to provide better error messages
--
-- When dealing with type level programming in Haskell, especially when involving
-- undecidable instances, one is often confronted with irritating compiler
-- behaviour. This module shall mitigate the problems.
--
-- A group of types that all 'From' into the same type. This
-- is an /open/ and /extensible/ alternative to defining an algebraic data type
-- and using the promoted constructors. 'A' goes hand-in-hand with 'Extends' for
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
-- data ColoredText :: Color -> Symbol -> 'Extends' (PrettyPrinter Symbol)
--
-- type instance 'From' (ColoredText c txt) = 'WithColor c ('RenderText txt)
-- @
module Data.Kind.Extra
  ( type Extends
  , type Konst
  , (:~:)
  , type From
  , type Apply
  , Labelled
  , Named
  , Name
  , Anonymous
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
import GHC.TypeLits (Symbol)

-- | Indicates that a type constructs another.
type Extends a = (a -> Type :: Type)

-- | An open type family to turn /symbolic/ type representations created with
-- 'A' or 'Extends' into the actual types.
type family From (t :: a -> Type) :: a

-- | A @Konst a@, @'Extends' a@.
type Konst (a :: k) = ((:~:) a :: Extends k)
type instance From ((:~:) a) = a

-- | Phantom type for things that have a name
data Named s

-- | Assign a name to something that has no name
data Name :: Symbol -> Extends s -> Extends (Named s)

-- | Remove tha name of a 'NamedStructure' to get to a 'Structure'
data Anonymous (x :: Extends (Named s)) :: Extends s

-- | Assign a symbol to any type in a group.
data Labelled (s :: Symbol) :: Extends a -> Extends a

type instance From (Labelled s t) = From t

-- | An open family of functions from @a@ to @b@
type family Apply (f :: Extends (a -> b)) (x :: a) :: b

-- | An alias for 'Apply'
type f $ x = Apply f x

-- | Compose functions
data (:>>>:) :: Extends (good -> better) -> Extends (better -> best) -> Extends (good -> best)
infixl 1 :>>>:
type instance Apply (f :>>>: g) x = g $ (f $ x)

-- | From Input & Compose
data (:^>>>:) :: Extends (good -> better) -> Extends (better -> best) -> Extends (Extends good -> best)
infixl 1 :^>>>:
type instance Apply (f :^>>>: g) x = g $ (f $ From x)

-- | Compose and 'Konst'
data (:>>>^:) :: Extends (good -> better) -> Extends (better -> best) -> Extends (good -> Extends best)
infixl 1 :>>>^:
type instance Apply (f :>>>^: g) x = Konst (g $ (f $ x))

-- | A function that applies 'From'
data Extract :: Extends (Extends x -> x)
type instance Apply Extract x = From x

-- | From and ApplyCompose functions
data (:>>=:) :: Extends a -> Extends (a -> Extends b) -> Extends b
infixl 1 :>>=:
type instance From (x :>>=: f) = From (f $ From x)

-- | Either use the value from @Just@ or return a fallback value(types(kinds))
data Optional :: Extends t -> Extends (s -> Extends t) -> Extends (Maybe s -> Extends t)

type instance Apply (Optional fallback f) ('Just s) = f $ s
type instance Apply (Optional fallback f) 'Nothing = fallback

-- | Map over the elements of a list and fold the result.
type family
  FoldMap
          (append :: Extends (b -> Extends (b -> b)))
          (zero :: b)
          (f :: Extends (a -> b))
          (xs :: [(a :: Type)]) :: (b :: Type) where
  FoldMap append zero f '[]       = zero
  FoldMap append zero f (x ': xs) = append $ (f $ x) $ FoldMap append zero f xs

--  TODONT safe coercions (with undecidable instances) could be done only with a
--  type-level equivilant of a type class dictionary, A good place for that
--  might be 'A'. For now I only had trouble with 'CoerceTo' because it is open
--  and the compiler often used up __all__ main memory when an instance was
--  missing.

-- | Like @TyCon1@ from Data.Singletons
data Fun1 :: (a -> Extends b)
            -> Extends (a -> Extends b)
type instance Apply (Fun1 f) x = (f x)

data Fun2 :: (a -> b -> Extends c)
            -> Extends (a -> Extends (b -> Extends c))
type instance Apply (Fun2 f) x = Fun1 (f x)

data Fun3 :: (a -> b -> c -> Extends d)
            -> Extends (a -> Extends (b -> Extends (c -> Extends d)))
type instance Apply (Fun3 f) x = Fun2 (f x)

data Fun4 :: (a -> b -> c -> d -> Extends e)
            -> Extends (a -> Extends (b -> Extends (c -> Extends (d -> Extends e))))
type instance Apply (Fun4 f) x = Fun3 (f x)
