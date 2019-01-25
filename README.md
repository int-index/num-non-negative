Non-negative numbers:

```
ghci> import Numeric.NonNegative
ghci> 2 + 3 :: NonNegative Double
5.0
ghci> 2 - 3 :: NonNegative Double
*** Exception: arithmetic underflow
```
