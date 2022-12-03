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
>>> set @"three" x False
"Example { one = 5, two = False, three = False }"
```

`x` is represented using `Word16` instead of `Example` and thus takes far less memory.

## Fixing the size with `Data.Width`

```haskell
import Data.Bitfield
import Data.Width

data Example = Example { one :: Width 3 Int, two :: Width 5 Int }

x :: Bitfield Word8 Example
```

This bitfield will be represented by a single byte, giving `one` 3 bits and `two` 5.

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
