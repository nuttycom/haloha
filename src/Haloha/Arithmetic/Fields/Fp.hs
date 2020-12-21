module Haloha.Arithmetic.Fields.Fp where

-- import qualified Algebra.Additive as Additive

import Basement.Types.Word128 (Word128 (..))
import Data.Bits ((.&.), shiftR)
import Haloha.Arithmetic.Fields.Types (Choice)
import Prelude

-- | A 256-bit finite field over the prime
-- 0x40000000000000000000000000000000224698fc094cf91b992d30ed00000001
-- represented as 4 64-bit words in little-endian order.
--
-- This is the field of definition for the Pallas member of the
-- Pallas/Vesta (Pasta) curve cycle.
--
-- Operations implemented on this type are *not* constant-time, so
-- this field is unsuitable for
data Fp = Fp !Word64 !Word64 !Word64 !Word64
  deriving (Eq, Show)

fromWord64 :: Word64 -> Fp
fromWord64 w = Fp w 0 0 0

-- TODO: real impl
constantTimeEq :: Fp -> Fp -> Choice
constantTimeEq = (==)

-- TODO: real impl
-- called `conditional_select` in halo2
constantTimeSelect :: Fp -> Fp -> Choice -> Fp
constantTimeSelect a b p = if p then a else b

modulus :: Fp
modulus =
  Fp
    0x992d30ed00000001
    0x224698fc094cf91b
    0x0000000000000000
    0x4000000000000000

-- INV = -(p^{-1} mod 2^64) mod 2^64
inv :: Word64
inv = 0x992d30ecffffffff

-- R = 2^256 mod p
r :: Fp
r =
  Fp
    0x34786d38fffffffd
    0x992c350be41914ad
    0xffffffffffffffff
    0x3fffffffffffffff

-- R^2 = 2^512 mod p
r2 :: Fp
r2 =
  Fp
    0x8c78ecb30000000f
    0xd7d30dbd8b0de0e7
    0x7797a99bc3c95d18
    0x096d41af7b9cb714

-- R^3 = 2^768 mod p
r3 :: Fp
r3 =
  Fp
    0xf185a5993a9e10f9
    0xf6a68f3b6ac5b1d1
    0xdf8d1014353fd42c
    0x2ae309222d2d9910

s :: Word32
s = 32

-- GENERATOR^t where t * 2^s + 1 = p
-- with t odd. In other words, this
-- is a 2^s root of unity.
--
-- `GENERATOR = 5 mod p` is a generator
-- of the p - 1 order multiplicative
-- subgroup.
rootOfUnity :: Fp
rootOfUnity =
  Fp
    0xbdad6fabd87ea32f
    0xea322bf2b7bb7584
    0x362120830561f81a
    0x2bce74deac30ebda

-- GENERATOR^{2^s} where t * 2^s + 1 = p
-- with t odd. In other words, this
-- is a t root of unity.
--
-- `GENERATOR = 5 mod p` is a generator
-- of the p - 1 order multiplicative
-- subgroup.
delta :: Fp
delta =
  Fp
    0x6a6ccd20dd7b9ba2
    0xf5e4f3f13eee5636
    0xbd455b7112a5049d
    0x0a757d0f0006ab6c

zero :: Fp
zero = Fp 0 0 0 0

one :: Fp
one = r

fpSub :: Fp -> Fp -> Fp
fpSub (Fp a0 a1 a2 a3) (Fp b0 b1 b2 b3) =
  let (d0, borrow0) = sbb a0 b0 0
      (d1, borrow1) = sbb a1 b1 borrow0
      (d2, borrow2) = sbb a2 b2 borrow1
      (d3, borrow3) = sbb a3 b3 borrow2
      -- If underflow occurred on the final limb, borrow''' = 0xfff...fff, otherwise
      -- borrow''' = 0x000...000. Thus, we use it as a mask to conditionally add the modulus.
      (Fp m0 m1 m2 m3) = modulus
      (d0', carry0) = adc d0 (m0 .&. borrow3) 0
      (d1', carry1) = adc d1 (m1 .&. borrow3) carry0
      (d2', carry2) = adc d2 (m2 .&. borrow3) carry1
      (d3', _) = adc d3 (m3 .&. borrow3) carry2
   in Fp d0' d1' d2' d3'

fpAdd :: Fp -> Fp -> Fp
fpAdd (Fp a0 a1 a2 a3) (Fp b0 b1 b2 b3) =
  let (d0, carry0) = adc a0 b0 0
      (d1, carry1) = adc a1 b1 carry0
      (d2, carry2) = adc a2 b2 carry1
      (d3, _) = adc a3 b3 carry2
   in fpSub (Fp d0 d1 d2 d3) modulus

-- | Compute a + b + carry, returning the result and the new carry over.
adc :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
adc a b carry =
  let (Word128 hi lo) = Word128 0 a + Word128 0 b + Word128 0 carry
   in (lo, hi)

-- Compute a - (b + borrow), returning the result and the new borrow.
sbb :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
sbb a b borrow =
  let (Word128 hi lo) = Word128 0 a - (Word128 0 b + Word128 0 (shiftR borrow 63))
   in (lo, hi)

w8 :: Integral i => i -> Word8
w8 = fromIntegral

w16 :: Integral i => i -> Word16
w16 = fromIntegral

adcb :: Word8 -> Word8 -> Word8 -> (Word8, Word8)
adcb a b carry =
  let r16 = w16 a + w16 b + w16 carry
   in (w8 r16, w8 (shiftR r16 8))

sbbb :: Word8 -> Word8 -> Word8 -> (Word8, Word8)
sbbb a b borrow =
  let r16 = w16 a - (w16 b + w16 (shiftR borrow 7))
   in (w8 r16, w8 (shiftR r16 8))

fpNegate :: Fp -> Fp
fpNegate = undefined
-- instance Additive.C Fp where
--   zero = zero
--   (+) = fpAdd
--   (-) = fpSub
--   negate = fpNegate
