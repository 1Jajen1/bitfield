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
, AsRep(..)
, ViaIntegral(..)
, BitSize
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
-- @a@'s fields are also required to have an instance of the internal class 'AsRep'. This is provided for the most common
-- types ('Int'/'Word' (and variants) and 'Bool'). 
newtype Bitfield (rep :: Type) (a :: Type) = Bitfield rep
  deriving newtype Eq

-- | Access the underlying representation of the bitfield
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
pack :: forall rep a . (Fits rep a, Generic a, Num rep, GPackBitfield rep (Rep a)) => a -> Bitfield rep a
pack a = case packI (Proxy @(Rep a)) 0 0 (from a) of (_, b) -> Bitfield b
{-# INLINE pack #-}

instance (Fits rep a, Generic a, GPackBitfield rep (Rep a), Show a) => Show (Bitfield rep a) where
  show = show . unpack

--
class GPackBitfield rep (f :: p -> Type) where
  unpackI :: forall x . Proxy f -> Int -> rep -> (Int, f x)
  packI :: forall x . Proxy f -> Int -> rep -> f x -> (Int, rep)

instance GPackBitfield r f => GPackBitfield r (D1 m f) where
  unpackI _ off b = M1 <$> unpackI (Proxy @f) off b
  {-# INLINE unpackI #-}
  packI _ off r (M1 f) = packI (Proxy @f) off r f
  {-# INLINE packI #-}
instance GPackBitfield r f => GPackBitfield r (C1 m f) where
  unpackI _ off b = M1 <$> unpackI (Proxy @f) off b
  {-# INLINE unpackI #-}
  packI _ off r (M1 f) = packI (Proxy @f) off r f
  {-# INLINE packI #-}
instance (FiniteBits a, AsRep rep a) => GPackBitfield rep (S1 m (K1 c a)) where
  unpackI _ off r = (finiteBitSize (undefined :: a) + off, M1 . K1 $ fromRep r off)
  {-# INLINE unpackI #-}
  packI _ off r (M1 (K1 a)) = (finiteBitSize (undefined :: a) + off, toRep r a off)
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
--
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
instance {-# OVERLAPS #-} FiniteBits a => GOffset name (S1 m (K1 c a)) where
  offset _ _ = Left $ finiteBitSize (undefined :: a)
  {-# INLINE offset #-}

instance (HasField (name :: Symbol) a x, GOffset name (Rep a), AsRep rep x) => HasField name (Bitfield rep a) x where
  getField (Bitfield rep) = fromRep rep off
    where
      off = case offset (Proxy @name) (Proxy @(Rep a)) of
        Left _ -> error "Data.Bitfield.getField:Failed to find offset"
        Right n -> n
  {-# INLINE getField #-}

-- | Typeclass which converts @rep@ and @a@ into each other (at specified offsets).
class AsRep rep a where
  fromRep :: rep -> Int -> a
  toRep :: rep -> a -> Int -> rep

instance Bits a => AsRep a Bool where
  fromRep r off = testBit r off
  {-# INLINE fromRep #-}
  toRep rep True off = setBit rep off
  toRep rep False off = clearBit rep off
  {-# INLINE toRep #-}

-- | Newtype wrapper with an 'AsRep' instance for 'Integral' representations and types.
newtype ViaIntegral a = ViaIntegral a
  deriving newtype (Eq, Ord, Bits, Real, Enum, Num, Integral)

instance (Bits a, Integral a, FiniteBits n, Integral n) => AsRep a (ViaIntegral n) where
  fromRep r off = mask .&. (fromIntegral $ unsafeShiftR r off)
    where
      mask = (unsafeShiftL (bit 0) (finiteBitSize (undefined :: n))) - 1
  {-# INLINE fromRep #-}
  toRep rep n off = (mask .&. rep) .|. (unsafeShiftL (fromIntegral n) off)
    where
      mask = complement $ unsafeShiftL ((unsafeShiftL (bit 0) (finiteBitSize (undefined :: n))) - 1) off
  {-# INLINE toRep #-}

deriving via (ViaIntegral Int8 ) instance (Bits r, Integral r) => AsRep r Int8
deriving via (ViaIntegral Int16) instance (Bits r, Integral r) => AsRep r Int16
deriving via (ViaIntegral Int32) instance (Bits r, Integral r) => AsRep r Int32
deriving via (ViaIntegral Int64) instance (Bits r, Integral r) => AsRep r Int64
deriving via (ViaIntegral Int  ) instance (Bits r, Integral r) => AsRep r Int

deriving via (ViaIntegral Word8 ) instance (Bits r, Integral r) => AsRep r Word8
deriving via (ViaIntegral Word16) instance (Bits r, Integral r) => AsRep r Word16
deriving via (ViaIntegral Word32) instance (Bits r, Integral r) => AsRep r Word32
deriving via (ViaIntegral Word64) instance (Bits r, Integral r) => AsRep r Word64
deriving via (ViaIntegral Word  ) instance (Bits r, Integral r) => AsRep r Word

-- | Type level information about the bitsize of elements
--
-- Used to verify that a datatype fits into a representation, which guarantees safe access to fields.
type family BitSize (a :: Type) :: Nat

type instance BitSize Int8  = 8
type instance BitSize Int16 = 16
type instance BitSize Int32 = 32
type instance BitSize Int64 = 64

type instance BitSize Word8  = 8
type instance BitSize Word16 = 16
type instance BitSize Word32 = 32
type instance BitSize Word64 = 64

type instance BitSize Bool = 1

type instance BitSize Int  = SIZEOF_HSINT Nat.* 8
type instance BitSize Word = SIZEOF_HSINT Nat.* 8

type family ReqSz (f :: p -> Type) :: Nat where
  ReqSz (M1 i s f) = ReqSz f
  ReqSz (K1 c a) = BitSize a
  ReqSz (f :*: g) = ReqSz f + ReqSz g

-- | Constraint which checks if a datatype fits.
type Fits rep a = FitsPred (ReqSz (Rep a) <=? BitSize rep) rep a

type family FitsPred (b :: Bool) (rep :: Type) (a :: Type) :: Constraint where
  FitsPred True _ _ = ()
  FitsPred False rep a = TypeError (Text "Datatype " :<>: ShowType a :<>: Text " needs " :<>: ShowType (ReqSz (Rep a)) :<>: Text " bits, but the given representation " :<>: ShowType rep :<>: Text " has " :<>: ShowType (BitSize rep))
