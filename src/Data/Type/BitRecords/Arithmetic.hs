{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Arithmetic where

import           Data.Type.Bool
import           GHC.TypeLits

-- | Efficient 'Mod' operation for power of 2 values. Note that x must be
-- representable by 'ModPow2Bits' bits.
type ModPow2 value power = FromBits (TakeLastN power (ToBits value ModPow2Bits))

type TakeLastN n xs = TakeLastNImplRev n xs '[]

type family TakeLastNImplRev (n :: Nat) (xs :: [t]) (acc :: [t]) :: [t] where
  TakeLastNImplRev n '[] acc = TakeLastNImplTakeNRev n acc '[]
  TakeLastNImplRev n (x ': xs) acc =
    TakeLastNImplRev n xs (x ': acc)

type family TakeLastNImplTakeNRev (n :: Nat) (rs :: [t]) (acc :: [t]) :: [t] where
  TakeLastNImplTakeNRev n '[] acc = acc
  TakeLastNImplTakeNRev 0 rs acc = acc
  TakeLastNImplTakeNRev n (r ': rs) acc = TakeLastNImplTakeNRev (n-1) rs (r ': acc)


-- | Maximum number of bits an argument @x@ of 'ModPow2' may occupy.
type ModPow2Bits = 32

-- * Bit manipulation

type family TestHighBit (x :: Nat) (n :: Nat) :: Bool where
  TestHighBit x n = ((2 ^ n) <=? x) -- x > 2^n

type ToBits x n = ToBits_ x n 'False
type family ToBits_ (x :: Nat) (n :: Nat) (started :: Bool) :: [Bool] where
  ToBits_ x 0 started = '[]
  ToBits_ x n started = ToBitsInner (TestHighBit x (n - 1)) x (n - 1) started
type family
  ToBitsInner (highBitSet :: Bool) (x :: Nat) (n :: Nat) (started :: Bool) :: [Bool] where
  ToBitsInner 'True  x n started = 'True  ': ToBits_ (x - 2^n) n 'True
  ToBitsInner 'False x n 'False  =           ToBits_ x         n 'False
  ToBitsInner 'False x n 'True   = 'False ': ToBits_ x         n 'True

type FromBits bits = FromBits_ bits 0
type family FromBits_ (bits :: [Bool]) (acc :: Nat) :: Nat where
  FromBits_ '[] acc = acc
  FromBits_ ('False ': rest) acc = FromBits_ rest (acc + acc)
  FromBits_ ('True  ': rest) acc = FromBits_ rest (1 + acc + acc)

type family
  ShiftBitsR (bits :: [Bool]) (n :: Nat) :: [Bool] where
  ShiftBitsR bits 0 = bits
  ShiftBitsR '[] n = '[]
  ShiftBitsR '[e] 1 = '[]
  ShiftBitsR (e ': rest) 1 = e ': ShiftBitsR rest 1
  ShiftBitsR (e ': rest) n = ShiftBitsR (ShiftBitsR (e ': rest) 1) (n - 1)

type family
  GetMostSignificantBitIndex (highestBit :: Nat) (n :: Nat) :: Nat where
  GetMostSignificantBitIndex          0 n = 1
  GetMostSignificantBitIndex highestBit n =
    If  (2 ^ (highestBit + 1) <=? n)
        (TypeError ('Text "number to big: "
                    ':<>: 'ShowType n
                    ':<>: 'Text " >= "
                    ':<>: 'ShowType (2 ^ (highestBit + 1))))
        (If (2 ^ highestBit <=? n)
            highestBit
            (GetMostSignificantBitIndex (highestBit - 1) n))

-- | Shift a type level natural to the right. This useful for division by powers
-- of two.
type family
  ShiftR (xMaxBits :: Nat) (x :: Nat) (bits :: Nat) :: Nat where
  ShiftR xMaxBits x n =
    FromBits
      (ShiftBitsR
        (ToBits x
                (1 + GetMostSignificantBitIndex xMaxBits x))
        n)
