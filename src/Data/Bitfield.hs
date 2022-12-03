-- |
-- Module      : Data.Bitfield
-- Copyright   : (c) Jannis Overesch 2022-2022
-- License     : MIT
-- Maintainer  : overesch.jannis@gmail.com
-- 
-- Generic and easy to use bitfields. Pack and unpack records into compact representations.
-- The implementation is generic over the representation and the record, however it is
-- assumed that the representation is some integral type and that the record has an instance of
-- 'GHC.Generics.Generic'.
--
-- Due to possible overlap with the method names, it is recommended to use this module with a qualified import.
--
-- @
-- import Data.Bitfield
--
-- data Example = Example { one :: Bool, two :: Bool, three :: Bool, four :: Word8 } deriving (Show, Generic)
-- 
-- x :: Bitfield Word16 Example
-- x = pack $ Example True False True 4
--
-- >>> x
-- Example { one = True, two = False, three = True, four = 4 }
-- >>> get \@"two" x
-- False
-- >>> set \@"three" x False
-- Example { one = True, two = False, three = False, four = 4 }
-- @
--
-- The values in the bitfield will be in whatever order the 'GHC.Generics.Generic' instance defines them in.
-- This is usually the order in which they are defined. 
--
-- Access with @OverloadedRecordDot@ is also supported:
--
-- @
-- {-# LANGUAGE OverloadedRecordDot #-}
--
-- import Data.Bitfield
--
-- data Example = Example { one :: Bool, two :: Bool, three :: Bool } deriving (Show, Generic)
-- 
-- x :: Bitfield Word8 Example
-- x = pack $ Example True False True
--
-- >>> x.one
-- True
-- @
-- 
-- 'Bitfield' supports a variety of field types as long as those implement 'HasFixedBitSize' and 'AsRep'.
-- These instances are usually derived via 'GenericEnum' or 'ViaIntegral'.
--
-- @
-- data AEnum = A1 | A2 | A3
--   deriving stock (Generic, Enum)
--   deriving (HasFixedBitSize, AsRep r) via (GenericEnum AEnum)
--
-- newtype SmallInt = SmallInt Int
--   deriving (HasFixedBitSize, AsRep r) via (ViaIntegral 5 Int)
--
-- data Example = Example { a :: AEnum, b :: SmallInt }
--
-- x :: Bitfield Word8 Example
-- x = pack $ Example A2 (SmallInt 3)
-- @
--
-- It is also possible to nest 'Bitfield's, but they are not unpacked, the representation of the nested field is used directly.
--
-- @
-- data Nested = Nested { a :: Bool, b :: Bool }
-- data Example = Example { one :: Bitfield Word8 Nested, other :: Bool }
--
-- -- This bitfield requires at least 9 bits because the field "one" requires 8 bits.
-- x :: Bitfield Word16 Example
-- x = Pack $ Example (pack $ Nested True True) False
-- @

module Data.Bitfield (
-- * Bitfield
  Bitfield(Bitfield)
, unwrap
-- * General use
, get, set
, pack, unpack
-- * Custom fields
, HasFixedBitSize(..)
, AsRep(..)
, ViaIntegral(..)
, GenericEnum(..)
) where

import Data.Bitfield.Internal
