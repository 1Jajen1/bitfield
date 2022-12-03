{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE OverloadedRecordDot #-}
#endif
module BitfieldSpec where

import Test.Syd
import Test.Syd.Validity

import Test.QuickCheck

import Data.Word

import Data.Bitfield as Bitfield
import GHC.Generics
import Data.Bits (unsafeShiftL)

data Example = Example { a :: Bool, b :: Word8, c :: Word16, d :: Bool } deriving stock (Eq, Show, Generic)

instance GenValid Example where
instance Validity Example where

spec :: Spec
spec = do
  it "pack should work on simple examples" $ do
    let t1 = pack @Word32 $ Example False 0 0 False
    let t2 = pack @Word32 $ Example True 0 0 False
    let t3 = pack @Word32 $ Example False 0 0 True
    let t4 = pack @Word32 $ Example True 12 0 False
    (Bitfield.unwrap t1) `shouldBe` 0
    (Bitfield.unwrap t2) `shouldBe` 1
    (Bitfield.unwrap t3) `shouldBe` (unsafeShiftL 1 25)
    (Bitfield.unwrap t4) `shouldBe` 25

  it "get @name b == name (unpack b)" $ forAllValid $ \(t :: Example) ->
    let bf = Bitfield.pack @Word32 t
    in get @"d" bf === d t

#if __GLASGOW_HASKELL__ >= 902
  it "get @name b == b.name" $ forAllValid $ \(t :: Example) ->
    let bf = Bitfield.pack @Word32 t
    in get @"d" bf === bf.d

  it "get @name b == (unpack b).name" $ forAllValid $ \(t :: Example) ->
    let bf = Bitfield.pack @Word32 t
    in (get @"b" bf === (unpack bf).b)
#endif
  
  it "get @name (set @name b x) == x" $ forAllValid $ \(t :: Example, x :: Word16) ->
    x === Bitfield.get @"c" (Bitfield.set @"c" (Bitfield.pack @Word32 t) x)
  
  it "unpack . pack = id" $ forAllValid $ \(t :: Example) ->
    t === (unpack $ pack @Word32 t)
