{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Question where

class T1 a where
 type T0 a b
 t1 :: a -> b -> T0 a b

data Foo = MkFoo

instance T1 Foo where
  type T0 Foo b = Foo -> b
  t1 MkFoo b = \MkFoo -> b

data Bar = MkBar Foo

instance T1 Bar where
  type T0 Bar b = b
  t1 (MkBar foo) b = t1 foo b foo

data Bar1 a = MkBar1 a

instance (forall b . (T0 foo b) ~ (foo -> b), T1 foo) => T1 (Bar1 foo) where
  type T0 (Bar1 foo) b = b
  t1 (MkBar1 foo) b = t1 foo b foo

{-                    ^^^^^^^^^^^^

I get this error, but don't know why:

QuantifiedConstraintsQuestion.hs:27:23: error:
    * Couldn't match expected type `foo -> b'
                  with actual type `T0 foo b'
    * The function `t1' is applied to three arguments,
      but its type `foo -> b -> T0 foo b' has only two
      In the expression: t1 foo b foo
      In an equation for `t1': t1 (MkBar1 foo) b = t1 foo b foo
    * Relevant bindings include
        b :: b (bound at QuantifiedConstraintsQuestion.hs:27:19)
        foo :: foo (bound at QuantifiedConstraintsQuestion.hs:27:14)
        t1 :: Bar1 foo -> b -> T0 (Bar1 foo) b
          (bound at QuantifiedConstraintsQuestion.hs:27:3)
   |
27 |   t1 (MkBar1 foo) b = t1 foo b foo
   |                       ^^^^^^^^^^^^

-}
