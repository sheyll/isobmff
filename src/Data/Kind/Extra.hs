{-# LANGUAGE UndecidableInstances #-}

-- / Utilities to provide better error messages
--
-- When dealing with type level programming in Haskell, especially when involving
-- undecidable instances, one is often confronted with irritating compiler
-- behaviour. This module shall mitigate the problems.
--
-- A group of types that all 'From' into the same type. This
-- is an /open/ and /extensible/ alternative to defining an algebraic data type
-- and using the promoted constructors. 'A' goes hand-in-hand with 'IsA' for
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
-- data ColoredText :: Color -> Symbol -> 'IsA' (PrettyPrinter Symbol)
--
-- type instance 'From' (ColoredText c txt) = 'WithColor c ('RenderText txt)
-- @
module Data.Kind.Extra
  ( type IsA
  , type IsAn
  , type Konst
  , type From
  , type (:->)
  , type Id
  , type Apply
  , type (^*^)
  , type ($~)
  , type (:>>=:)
  , type (:>>>:)
  , type (:^>>>:)
  , type (:>>>^:)
  , type (:^>>>^:)
  , type Extract
  , type Optional
  , type FoldMap
  , type Fun1
  , type Fun2
  , type Fun3
  , type Fun4
  ) where

import Data.Kind (type Type)

-- | Type alias for 'A' such that @data Point2 x y :: A Vec2 -> Type@ becomes
-- @data Point2 x y :: IsA Vec2@
type IsA foo = (foo -> Type :: Type)

-- | An alias to 'IsA'
type IsAn oo = (IsA oo :: Type)

-- | An open type family to turn /symbolic/ type representations created with
-- 'A' or 'IsA' into the actual types.
type family From (t :: foo -> Type) :: foo

-- | A @Konst foo@, @'IsA' foo@.
data Konst (a :: k) (b :: k)

type instance From (Konst f) = f

-- | A symbolic type-level function.
data (:->) foo bar

-- | An open family of functions from @foo@ to @bar@
type family (f :: IsA (foo :-> bar)) $~ (x :: foo) :: bar
infixl 0 $~

-- | An alias for '$~'
type Apply f x = f $~ x

-- | Identity
data Id :: IsA (foo :-> foo)
type instance Id $~ x = x

-- | Symbolic function application
type (^*^) (f :: IsA (foo :-> bar)) (x :: IsA foo) = f $~ (From x)
infixl 0 ^*^

-- | Compose functions
data (:>>>:) :: IsA (good :-> better) -> IsA (better :-> best) -> IsA (good :-> best)
infixl 1 :>>>:
type instance (f :>>>: g) $~ x = g $~ (f $~ x)

-- | From Input & Compose
data (:^>>>:) :: IsA (good :-> better) -> IsA (better :-> best) -> IsA (IsA good :-> best)
infixl 1 :^>>>:
type instance (f :^>>>: g) $~ x = g $~ (f $~ From x)

-- | Compose and 'Konst'
data (:>>>^:) :: IsA (good :-> better) -> IsA (better :-> best) -> IsA (good :-> IsA best)
infixl 1 :>>>^:
type instance (f :>>>^: g) $~ x = Konst (g $~ (f $~ x))

-- | From compose and return
data (:^>>>^:) :: IsA (good :-> better) -> IsA (better :-> best) -> IsA (IsA good :-> IsA best)
infixl 1 :^>>>^:
type instance (f :^>>>^: g) $~ x = Konst (g $~ (f $~ From x))

-- | A function that applies 'From'
data Extract :: IsA (IsA x :-> x)
type instance Extract $~ x = From x

-- | From and ApplyCompose functions
data (:>>=:) :: IsA foo -> IsA (foo :-> IsA bar) -> IsA bar
infixl 1 :>>=:
type instance From (x :>>=: f) = From (f $~ From x)

-- | Either use the value from @Just@ or return a fallback value(types(kinds))
data Optional :: IsA t -> IsA (s :-> IsA t) -> IsA (Maybe s :-> IsA t)

type instance (Optional fallback f) $~ ('Just s) = f $~ s
type instance (Optional fallback f) $~ 'Nothing = fallback

-- | Map over the elements of a list and fold the result.
type family
  FoldMap
          (append :: IsA (bar :-> IsA (bar :-> bar)))
          (zero :: bar)
          (f :: IsA (foo :-> bar))
          (xs :: [(foo :: Type)]) :: (bar :: Type) where
  FoldMap append zero f '[]       = zero
  FoldMap append zero f (x ': xs) = append $~ (f $~ x) $~ FoldMap append zero f xs

--  TODONT safe coercions (with undecidable instances) could be done only with a
--  type-level equivilant of a type class dictionary, A good place for that
--  might be 'A'. For now I only had trouble with 'CoerceTo' because it is open
--  and the compiler often used up __all__ main memory when an instance was
--  missing.

-- | Like @TyCon1@ from Data.Singletons
data Fun1 :: (a -> IsA b)
            -> IsA (a :-> IsA b)
type instance (Fun1 f) $~ x = (f x)

data Fun2 :: (a -> b -> IsA c)
            -> IsA (a :-> IsA (b :-> IsA c))
type instance (Fun2 f) $~ x = Fun1 (f x)

data Fun3 :: (a -> b -> c -> IsA d)
            -> IsA (a :-> IsA (b :-> IsA (c :-> IsA d)))
type instance (Fun3 f) $~ x = Fun2 (f x)

data Fun4 :: (a -> b -> c -> d -> IsAn e)
            -> IsA (a :-> IsA (b :-> IsA (c :-> IsA (d :-> IsAn e))))
type instance (Fun4 f) $~ x = Fun3 (f x)
