[![Build Status](https://img.shields.io/travis/int-index/num-non-negative.svg)](https://travis-ci.org/int-index/num-non-negative)
[![Hackage](https://img.shields.io/hackage/v/num-non-negative.svg)](https://hackage.haskell.org/package/num-non-negative)

Non-negative numbers:

```
ghci> import Numeric.NonNegative
ghci> 2 + 3 :: NonNegative Double
5.0
ghci> 2 - 3 :: NonNegative Double
*** Exception: arithmetic underflow
```
