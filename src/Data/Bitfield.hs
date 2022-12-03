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

module Data.Bitfield (
  Bitfield(Bitfield)
, get, set
, pack, unpack
, unwrap
) where

import Data.Bitfield.Internal
