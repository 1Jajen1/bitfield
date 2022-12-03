{-# LANGUAGE
  DerivingStrategies
, GeneralizedNewtypeDeriving
, KindSignatures
, DataKinds
, PolyKinds
, ScopedTypeVariables
, TypeApplications
, TypeFamilies
, UndecidableInstances
, MultiParamTypeClasses
, TypeSynonymInstances
, FlexibleInstances
, TypeOperators
, FlexibleContexts
, AllowAmbiguousTypes
, StandaloneDeriving
, DerivingVia
, CPP
#-}
{-# OPTIONS_GHC
  -Wno-unticked-promoted-constructors
  -Wno-redundant-constraints
#-}
{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
-- |
-- Module      : Data.Bitfield.Internal
-- Copyright   : (c) Jannis Overesch 2022-2022
-- License     : MIT
-- Maintainer  : overesch.jannis@gmail.com
module Data.Bitfield.Internal (
  Bitfield(..)
, unwrap
, get, set
, pack, unpack
, HasFixedBitSize(..)
, AsRep(..)
, ViaIntegral(..)
, GenericEnum(..)
, Fits
) where

#include "MachDeps.h"

import Data.Bits
import Data.Int
import Data.Kind
import Data.Proxy
import Data.Word

import GHC.Generics
import GHC.Records
import GHC.TypeLits as Nat

-- | A generic Bitfield
-- 
-- Represents type @a@ with type @rep@.
--
-- Technically this allows any representation and any type to represent, however all methods
-- are written with the implicit assumption of a representation with an 'Integral' and 'Bits' instance.
-- The type to represent is also assumed to have a `Generic` instance and be a single constructor with named fields.
-- 
-- @a@'s fields are also required to have an instance of 'AsRep' and 'FiniteBits'. This is provided for the most common
-- types ('Int'/'Word' (and variants) and 'Bool'). 
newtype Bitfield (rep :: Type) (a :: Type) = Bitfield rep
  deriving newtype Eq

-- | Access the underlying representation of the 'Bitfield'
unwrap :: Bitfield rep a -> rep
unwrap (Bitfield rep) = rep
{-# INLINE unwrap #-}

-- | Access a single field
get :: forall name x rep a . (Fits rep a, HasField name (Bitfield rep a) x) => Bitfield rep a -> x
get = getField @name
{-# INLINE get #-}

-- | Change a single field 
set :: forall name x rep a . (Fits rep a, HasField name (Bitfield rep a) x, GOffset name (Rep a), Bits rep, AsRep rep x) => Bitfield rep a -> x -> Bitfield rep a
set (Bitfield rep) x = Bitfield $ (toRep rep x off)
  where
    off = case offset (Proxy @name) (Proxy @(Rep a)) of
      Left _ -> error "Data.Bitfield.set:Failed to find offset"
      Right n -> n
{-# INLINE set #-}

-- | Unpack the 'Bitfield' and return the full datatype
unpack :: forall rep a . (Fits rep a, Generic a, GPackBitfield rep (Rep a)) => Bitfield rep a -> a
unpack (Bitfield b) = case unpackI (Proxy @(Rep a)) 0 b of (_, a) -> to a
{-# INLINE unpack #-}

-- | Pack a datatype into a bitfield
--
-- Beware that updates should be done with 'set', as 'pack' will recreate the entire 'Bitfield'
-- from scratch. The following will most likely *not* be optimised: @pack $ (unpack bitfield) { example = True }@
pack :: forall rep a . (Fits rep a, Generic a, Bits rep, GPackBitfield rep (Rep a)) => a -> Bitfield rep a
pack a = case packI (Proxy @(Rep a)) 0 zeroBits (from a) of (_, b) -> Bitfield b
{-# INLINE pack #-}

instance (Fits rep a, Generic a, GPackBitfield rep (Rep a), Show a) => Show (Bitfield rep a) where
  show = show . unpack

--
class GPackBitfield rep (f :: p -> Type) where
  unpackI :: forall x . Proxy f -> Int -> rep -> (Int, f x)
  packI :: forall x . Proxy f -> Int -> rep -> f x -> (Int, rep)

instance GPackBitfield r f => GPackBitfield r (M1 s m f) where
  unpackI _ off b = M1 <$> unpackI (Proxy @f) off b
  {-# INLINE unpackI #-}
  packI _ off r (M1 f) = packI (Proxy @f) off r f
  {-# INLINE packI #-}
instance (HasFixedBitSize a, AsRep rep a) => GPackBitfield rep (K1 c a) where
  unpackI _ off r = (fixedBitSize @a + off, K1 $ fromRep r off)
  {-# INLINE unpackI #-}
  packI _ off r (K1 a) = (fixedBitSize @a + off, toRep r a off)
  {-# INLINE packI #-}
instance (GPackBitfield r f, GPackBitfield r g) => GPackBitfield r (f :*: g) where
  unpackI _ off rep =
    case unpackI (Proxy @f) off rep of
      (off', l) -> case unpackI (Proxy @g) off' rep of
        (off'', r) -> (off'', l :*: r)
  {-# INLINE unpackI #-}
  packI _ off rep (l :*: r) =
    case packI (Proxy @f) off rep l of
      (off', rep') -> packI (Proxy @g) off' rep' r
  {-# INLINE packI #-}

class GOffset (name :: Symbol) (f :: p -> Type) where
  offset :: Proxy name -> Proxy f -> Either Int Int

instance GOffset name f => GOffset name (D1 m f) where
  offset pn _ = offset pn (Proxy @f)
  {-# INLINE offset #-}
instance GOffset name f => GOffset name (C1 m f) where
  offset pn _ = offset pn (Proxy @f)
  {-# INLINE offset #-}
instance (GOffset name f, GOffset name g) => GOffset name (f :*: g) where
  offset pn _ = case offset pn (Proxy @f) of
    Left i -> case offset pn (Proxy @g) of
      Left j -> Left $ i + j
      Right j -> Right $ i + j
    Right i -> Right i
  {-# INLINE offset #-}
instance GOffset name (S1 (MetaSel (Just name) su ss ds) (K1 c a)) where
  offset _ _ = Right 0
  {-# INLINE offset #-}
instance {-# OVERLAPS #-} HasFixedBitSize a => GOffset name (S1 m (K1 c a)) where
  offset _ _ = Left $ fixedBitSize @a
  {-# INLINE offset #-}

instance (HasField (name :: Symbol) a x, GOffset name (Rep a), AsRep rep x) => HasField name (Bitfield rep a) x where
  getField (Bitfield rep) = fromRep rep off
    where
      off = case offset (Proxy @name) (Proxy @(Rep a)) of
        Left _ -> error "Data.Bitfield.getField:Failed to find offset"
        Right n -> n
  {-# INLINE getField #-}

-- | Types with a fixed bitsize. This could be a type family as well, but having
-- it as a typeclass provides nicer error messages when one forgets to write an
-- instance for it.
class KnownNat (BitSize a) => HasFixedBitSize (a :: Type) where
  type BitSize a :: Nat

fixedBitSize :: forall a. HasFixedBitSize a => Int
fixedBitSize = fromIntegral $ natVal (Proxy @(BitSize a))

-- | Typeclass which converts @rep@ and @a@ into each other (at specified offsets).
class HasFixedBitSize a => AsRep rep a where
  fromRep :: rep -> Int -> a
  toRep :: rep -> a -> Int -> rep

-- Flatten nested bitfields
instance KnownNat (BitSize r2) => HasFixedBitSize (Bitfield r2 a) where
  type BitSize (Bitfield r2 a) = BitSize r2

instance AsRep r1 r2 => AsRep r1 (Bitfield r2 a) where
  fromRep r1 off = Bitfield $ fromRep r1 off
  {-# INLINE fromRep #-}
  toRep r1 (Bitfield r2) off = toRep r1 r2 off
  {-# INLINE toRep #-}

instance HasFixedBitSize Bool where
  type BitSize Bool = 1 

instance Bits a => AsRep a Bool where
  fromRep r off = testBit r off
  {-# INLINE fromRep #-}
  toRep rep True off = setBit rep off
  toRep rep False off = clearBit rep off
  {-# INLINE toRep #-}

-- | Newtype wrapper with an 'AsRep' instance for 'Integral' representations and types.
--
-- The example below shows how to derive a 5 bit int field via a newtype:
-- 
-- @
-- newtype SmallInt = SmallInt Int
--   deriving (HasFixedBitSize, AsRep r) via (ViaIntegral 5 Int)
-- @
newtype ViaIntegral (sz :: Nat) a = ViaIntegral a
  deriving newtype (Eq, Ord, Bits, Real, Enum, Num, Integral)

instance KnownNat sz => HasFixedBitSize (ViaIntegral sz n) where
  type BitSize (ViaIntegral sz n) = sz

instance (Bits a, Integral a, Bits n, Integral n, KnownNat sz) => AsRep a (ViaIntegral sz n) where
  fromRep r off = mask .&. (fromIntegral $ unsafeShiftR r off)
    where
      mask = (unsafeShiftL (bit 0) (fixedBitSize @(ViaIntegral sz n))) - 1
  {-# INLINE fromRep #-}
  toRep rep n off = (mask .&. rep) .|. (unsafeShiftL (fromIntegral n) off)
    where
      mask = complement $ unsafeShiftL ((unsafeShiftL (bit 0) (fixedBitSize @(ViaIntegral sz n))) - 1) off
  {-# INLINE toRep #-}

instance HasFixedBitSize Int8  where type BitSize Int8  = 8
instance HasFixedBitSize Int16 where type BitSize Int16 = 16
instance HasFixedBitSize Int32 where type BitSize Int32 = 32
instance HasFixedBitSize Int64 where type BitSize Int64 = 64

instance HasFixedBitSize Int where type BitSize Int = SIZEOF_HSINT Nat.* 8

instance HasFixedBitSize Word8  where type BitSize Word8  = 8
instance HasFixedBitSize Word16 where type BitSize Word16 = 16
instance HasFixedBitSize Word32 where type BitSize Word32 = 32
instance HasFixedBitSize Word64 where type BitSize Word64 = 64

instance HasFixedBitSize Word where type BitSize Word = SIZEOF_HSINT Nat.* 8

deriving via (ViaIntegral 8  Int8 ) instance (Bits r, Integral r) => AsRep r Int8
deriving via (ViaIntegral 16 Int16) instance (Bits r, Integral r) => AsRep r Int16
deriving via (ViaIntegral 32 Int32) instance (Bits r, Integral r) => AsRep r Int32
deriving via (ViaIntegral 64 Int64) instance (Bits r, Integral r) => AsRep r Int64

deriving via (ViaIntegral (SIZEOF_HSINT Nat.* 8) Int) instance (Bits r, Integral r) => AsRep r Int

deriving via (ViaIntegral 8  Word8 ) instance (Bits r, Integral r) => AsRep r Word8
deriving via (ViaIntegral 16 Word16) instance (Bits r, Integral r) => AsRep r Word16
deriving via (ViaIntegral 32 Word32) instance (Bits r, Integral r) => AsRep r Word32
deriving via (ViaIntegral 64 Word64) instance (Bits r, Integral r) => AsRep r Word64

deriving via (ViaIntegral (SIZEOF_HSINT Nat.* 8) Word) instance (Bits r, Integral r) => AsRep r Word

-- | Deriving via helper for 'Enum' types. Requires that type to also have an instance of 'Generic'.
--
-- @
-- data AEnum = A1 | A2 | A3
--   deriving stock (Enum, Generic)
--   deriving (HasFixedBitSize, AsRep r) via (GenericEnum AEnum)
-- @
newtype GenericEnum a = GenericEnum a
  deriving newtype Eq

instance KnownNat (RoundUpLog2 (EnumSz (Rep a))) => HasFixedBitSize (GenericEnum a) where
  type BitSize (GenericEnum a) = RoundUpLog2 (EnumSz (Rep a))

instance (Generic a, Enum a, Bits rep, Integral rep, KnownNat (RoundUpLog2 (EnumSz (Rep a)))) => AsRep rep (GenericEnum a) where
  fromRep rep off =
    let ViaIntegral i = fromRep @rep @(ViaIntegral (BitSize (GenericEnum a)) Int) rep off
    in GenericEnum $ toEnum i
  {-# INLINE fromRep #-}
  toRep rep (GenericEnum x) off =
    let x' = ViaIntegral @(BitSize (GenericEnum a)) $ fromEnum x
    in toRep rep x' off
  {-# INLINE toRep #-}

-- Ugly way to check if we need to round up. Basically if he 2^log2(sz) /= sz then sz is not a power of two and was rounded down in log2.
type family RoundUpLog2 (sz :: Nat) :: Nat where
  RoundUpLog2 sz = RoundUpLog2' sz (2 ^ (Log2 sz)) (Log2 sz)

type family RoundUpLog2' (sz :: Nat) (sz' :: Nat) (log2 :: Nat) :: Nat where
  RoundUpLog2' sz sz log2 = log2
  RoundUpLog2' sz sz' log2 = log2 + 1

type family EnumSz (f :: p -> Type) :: Nat where
  EnumSz (M1 i s f) = EnumSz f
  EnumSz (f :+: g) = EnumSz f + EnumSz g
  EnumSz U1 = 1
  EnumSz (f :*: g) = TypeError (Text "Deriving a generic AsRep instance only supports sum types with empty constructors")
  EnumSz (K1 c a)  = TypeError (Text "Deriving a generic AsRep instance only supports sum types with empty constructors")

type family ReqSz (f :: p -> Type) :: Nat where
  ReqSz (M1 i s f) = ReqSz f
  ReqSz (K1 c a) = BitSize a
  ReqSz (f :*: g) = ReqSz f + ReqSz g

-- | Constraint which checks if a datatype fits.
type Fits rep a = FitsPred (ReqSz (Rep a) <=? BitSize rep) rep a

type family FitsPred (b :: Bool) (rep :: Type) (a :: Type) :: Constraint where
  FitsPred True _ _ = ()
  FitsPred False rep a = TypeError (Text "Datatype " :<>: ShowType a :<>: Text " needs " :<>: ShowType (ReqSz (Rep a)) :<>: Text " bits, but the given representation " :<>: ShowType rep :<>: Text " has " :<>: ShowType (BitSize rep))
