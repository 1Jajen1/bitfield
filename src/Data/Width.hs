{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
-- |
-- Module      : Data.Width
-- Copyright   : (c) Jannis Overesch 2022-2022
-- License     : MIT
-- Maintainer  : overesch.jannis@gmail.com
--
-- Newtype helper to fix the size of a value to a specific bitsize.
module Data.Width (
  Width(..)
) where

import Data.Bitfield.Internal

import Data.Bits
import Data.Kind
import Data.Proxy

import GHC.TypeLits

-- | Type with a custom 'FiniteBits' instance with @n@ as the bit size. 
--
-- Convenient for shrinking 'Int'\/'Word' to some range not covered by 'Data.Int.Int8'\/'Data.Int.Int16'\/'Data.Int.Int32'.
-- 
-- The following example datatype only occupies a single byte when packed into a 'Bitfield'.
--
-- @
-- import Data.Width
-- import Data.Bitfield
--
-- data Example = Example { first :: Bool, second :: Width 5 Word, third :: Bool, fourth :: Bool }
--
-- x :: Bitfield Word8 Example
-- @
--
-- It is *not* safe to pass around values that do not fit in the set bitsize. Writing such a value to a bitfield
-- can write over other values. If you aren't sure if a value fits, either use a smart constructor to check
-- or mask the value before setting to truncate it.
newtype Width (n :: Nat) (a :: Type) = UnsafeWidth { unWidth :: a }
  deriving newtype (Eq, Ord, Num, Real, Enum, Bits, Integral)

instance (FiniteBits a, KnownNat n) => FiniteBits (Width n a) where
  finiteBitSize _ = fromIntegral $ natVal (Proxy @n)
  {-# INLINE finiteBitSize #-}
  countLeadingZeros (UnsafeWidth a) = countLeadingZeros a
  {-# INLINE countLeadingZeros #-}
  countTrailingZeros (UnsafeWidth a) = countTrailingZeros a
  {-# INLINE countTrailingZeros #-}

deriving via (ViaIntegral (Width n a)) instance (FiniteBits a, KnownNat n, Integral a, Bits r, Integral r) => AsRep r (Width n a)

type instance BitSize (Width n a) = n
