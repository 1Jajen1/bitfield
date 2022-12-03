# bitfield

[![Hackage](https://img.shields.io/hackage/v/bitfield.svg?logo=haskell)](https://hackage.haskell.org/package/bitfield)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Generic and easy to use bitfields. Pack and unpack records into compact representations.

## Example

```haskell
import Data.Bitfield

data Example = Example { one :: Word8, two :: Bool, three :: Bool } deriving (Show, Generic)
 
x :: Bitfield Word16 Example
x = pack $ Example 5 False True

>>> x
"Example { one = 5, two = False, three = True }"
>>> get @"two" x
"False"
-- Requires OverloadedRecordDot
>>> x.two
"False"
>>> set @"three" x False
"Example { one = 5, two = False, three = False }"
```

`x` is represented using `Word16` instead of the full heap object `Example` and thus takes far less memory.

## Custom fields

There are two important typeclasses for working with types in bitfields: `AsRep` and `HasFixedBitSize`.

The most common way to use them is to derive them via either an underlying `Enum` or `Integral` instance.

#### Via `Enum`

```haskell
data AEnum = A1 | A2 | A3
  deriving stock (Generic, Enum)
  deriving (HasFixedBitSize, AsRep rep) via (GenericEnum AEnum)
```

This still requires a `Generic` instance to count the constructors of `AEnum` for the `HasFixedBitSize` instance. The resulting field has a size of `Log2 NumConstructors` rounded up (`2` bits for `AEnum`). The actual value will be constructed using the `Enum` instance, so unless that is derived, the constructor order won't matter.

#### Via `Integral`

```haskell
newtype SmallInt = UnsafeSmallInt Int
  deriving (HasFixedBitSize, AsRep rep) via (ViaIntegral 5 Int)
```

This creates a `HasFixedBitSize` instance with a fixed bit size of `5` and a `AsRep` instance which reads and writes `5` bit values.

Note: The value is not truncated nor is it otherwise checked that the underlying value actually fits into the specified bit size. If it is too large it will write over other values!

## Type safety

Any operation on a `Bitfield` checks (on the type level) if that `Bitfield` is valid. Any field operation also requires a `HasField` instance, guaranteeing that such a field exists. The following examples will not compile:

```haskell
data Bad1 = Bad1 { a :: Bool, b :: Word8, c :: Bool }

x :: Bitfield Word8 Bad1
x = pack $ Bad1 True 1 True
-- Datatype Bad1 needs 10 bits, but the given representation Word8 has 8

y :: Bitfield Word16 Bad1 -> Word8
y b = get @"test" b
-- No instance HasField "test" Bad1 Word8
```
