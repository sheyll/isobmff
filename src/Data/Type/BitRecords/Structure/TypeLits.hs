{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Structure.TypeLits where

import           Data.Kind.Extra
import           Data.Type.Pretty
import           GHC.TypeLits


data TypeLit = MkTypeLit

-- | Calculate the number of bits require to represent the given type literal value
type family TypeLitBits (t :: Extends TypeLit) :: Nat

-- | An empty type literal
data EmptyLiteral :: Extends TypeLit

type instance ToPretty EmptyLiteral = 'PrettyEmpty
type instance TypeLitBits EmptyLiteral = 0

-- | A signed type literal number
data NegativeNatLiteral ::Nat -> Extends TypeLit

type instance ToPretty (NegativeNatLiteral x) = PutStr "-" <++> PutNat x
type instance TypeLitBits (NegativeNatLiteral v) = NatBits v + 1

-- | Passthrough predefined type literals
data NatLiteral :: Nat -> Extends TypeLit

type instance ToPretty (NatLiteral x) = PutBits x
type instance TypeLitBits (NatLiteral v) = NatBits v

-- | A Bool literal
data FlagLiteral :: Bool -> Extends TypeLit

type instance ToPretty (FlagLiteral x) = ToPretty x
type instance TypeLitBits (FlagLiteral v) = 1

-- | A literal bit sequence. The sequence is validated, only
-- zeros and ones are allowed.
type family BitsLiteral (x :: [Nat]) :: Extends TypeLit
  where BitsLiteral xs = ValidBitsLiteral (ValidateBits xs xs)

type XXX = BitsLiteral '[1, 0, 1]

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
data ValidBitsLiteral :: [Nat] -> Extends TypeLit

type instance ToPretty (ValidBitsLiteral bs) =
  ToPrettyBitsLiteral bs

type family ToPrettyBitsLiteral bs where
  ToPrettyBitsLiteral '[] = 'PrettyEmpty
  ToPrettyBitsLiteral (x ': xs) = PutNat x <++> ToPrettyBitsLiteral xs

type instance TypeLitBits (ValidBitsLiteral bs) = Length bs

type family Length xs where
  Length '[] = 0
  Length (x ': xs) = 1 + Length xs

type family NatBits (value :: Nat) :: Nat where
  NatBits 0 = 1
  NatBits n = Log2 n + 1
