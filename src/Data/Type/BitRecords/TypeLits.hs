{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.TypeLits where

import           Data.Kind.Extra
import           Data.Type.Pretty
import           GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality (type (==))



data TypeLit = MkTypeLit

type ToTypeLit (t :: To TypeLit) = t 'MkTypeLit

-- | Calculate the number of bits require to represent the given type literal value
type family TypeLitBits (value :: k) :: Nat

-- | An empty type literal
data EmptyLiteral :: To TypeLit where
  MkEmptyLiteral ::EmptyLiteral 'MkTypeLit

type instance ToPretty 'MkEmptyLiteral = 'PrettyEmpty
type instance TypeLitBits 'MkEmptyLiteral = 0

-- | A Signed type literal number
data SignedNatLiteral :: To TypeLit where
  Positive ::Nat -> SignedNatLiteral 'MkTypeLit
  Negative ::Nat -> SignedNatLiteral 'MkTypeLit

type instance ToPretty ('Positive x) = PutNat x
type instance ToPretty ('Negative x) = PutStr "-" <++> PutNat x
type instance TypeLitBits ('Positive v) = NatBits v + 1
type instance TypeLitBits ('Negative v) = NatBits v + 1

-- | Passthrough predefined type literals
data NatLiteral :: To TypeLit where
  MkNat ::Nat -> NatLiteral 'MkTypeLit

type instance ToPretty ('MkNat x) = PutBits x
type instance TypeLitBits ('MkNat v ) = NatBits v

-- | A Bool literal
data FlagLiteral :: To TypeLit where
  MkFlag ::Bool -> FlagLiteral 'MkTypeLit

type instance ToPretty ('MkFlag x ) = ToPretty x
type instance TypeLitBits ('MkFlag v) = 1

-- | A literal bit sequence. The sequence is validated, only
-- zeros and ones are allowed.
type family BitsLiteral (x :: [Nat])
             :: ValidBitsLiteral (Length x) 'MkTypeLit
  where BitsLiteral xs = 'MkValidBitsLiteral (ValidateBits xs xs)

type XXX = BitsLiteral '[1,0,1]

type family ValidateBits (all :: [Nat]) (x :: [Nat]) :: [Nat] where
  ValidateBits ls '[] = '[]
  ValidateBits ls (0 ': xs) = 0 ': ValidateBits ls xs
  ValidateBits ls (1 ': xs) = 1 ': ValidateBits ls xs
  ValidateBits ls (bad ': xs) =
    TypeError ('Text "Error while validating the literal bit sequence: "
              ':<>: 'ShowType ls
              ':<>: 'Text ". Bit literals may only contain 1's or 0's, but not: "
              ':<>: 'ShowType bad)

-- | A literal bit sequence, use 'BitsLiteral' to construct this.
data ValidBitsLiteral (bitCount :: Nat) :: To TypeLit where
  MkValidBitsLiteral :: [Nat] -> ValidBitsLiteral n 'MkTypeLit

type instance ToPretty ('MkValidBitsLiteral bs) = ToPrettyBitsLiteral bs

type family ToPrettyBitsLiteral bs where
  ToPrettyBitsLiteral '[] = 'PrettyEmpty
  ToPrettyBitsLiteral (x ': xs) = PutNat x <++> ToPrettyBitsLiteral xs

type instance TypeLitBits ('MkValidBitsLiteral bs) = Length bs

type family Length xs where
  Length '[] = 0
  Length (x ': xs) = 1 + Length xs

type family NatBits (value :: Nat) :: Nat where
  NatBits 0 = 1
  NatBits n = Log2 n + 1
