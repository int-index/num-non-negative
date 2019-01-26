{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies,
             MultiParamTypeClasses, UndecidableInstances,
             FlexibleInstances, TypeApplications,
             ScopedTypeVariables #-}

{- |

Non-negative numbers:

@
ghci> import Numeric.NonNegative
ghci> 2 + 3 :: NonNegative Double
5.0
ghci> 2 - 3 :: NonNegative Double
*** Exception: arithmetic underflow
@

-}

module Numeric.NonNegative
  ( NonNegative(),
    getNonNegative,
    toNonNegative,
    unsafeToNonNegative
  ) where

import Control.Exception
import Data.Coerce
import Inj

-- | An opaque newtype around a number @n@ that asserts that @n >= 0@.
-- The constructor is not exported to maintain the invariant.
newtype NonNegative a = NonNegative a
  deriving newtype (Eq, Ord, Show, Real, Integral)

-- | Unwrap the newtype.
getNonNegative :: NonNegative a -> a
getNonNegative (NonNegative a) = a

instance (Inj p a, Ord a, Num a) => Inj p (NonNegative a) where
  inj = unsafeToNonNegative . inj

-- | Check if a number is non-negative and return 'Nothing' if it is negative.
toNonNegative :: (Ord a, Num a) => a -> Maybe (NonNegative a)
toNonNegative d =
  if d >= 0 then Just (NonNegative d) else Nothing

-- | Check if a number is non-negative and throw 'Underflow' if it is negative.
unsafeToNonNegative :: (Ord a, Num a) => a -> NonNegative a
unsafeToNonNegative d =
  if d >= 0 then NonNegative d else throw Underflow

-- | Throws 'Underflow'.
instance (Ord a, Num a) => Num (NonNegative a) where
  (+) = coerce ((+) @a)
  NonNegative a - NonNegative b = unsafeToNonNegative (a - b)
  (*) = coerce ((*) @a)
  negate _ = throw Underflow
  abs = id
  signum = coerce (signum @a)
  fromInteger = unsafeToNonNegative . fromInteger

-- | Throws 'Underflow'.
instance (Ord a, Fractional a) => Fractional (NonNegative a) where
  (/) = coerce ((/) @a)
  recip = coerce (recip @a)
  fromRational = unsafeToNonNegative . fromRational

-- | Throws 'Underflow'.
instance (Ord a, Num a, Enum a) => Enum (NonNegative a) where
  succ = coerce (succ @a)
  pred (NonNegative a) = unsafeToNonNegative (pred a)
  toEnum = unsafeToNonNegative . toEnum
  fromEnum = coerce (fromEnum @a)
  enumFrom = coerce (enumFrom @a)
  enumFromThen (NonNegative n) (NonNegative n')
    | n' < n = coerce (takeWhile (>=0) (enumFromThen n n'))
    | otherwise = coerce (enumFromThen n n')
  enumFromTo = coerce (enumFromTo @a)
  enumFromThenTo = coerce (enumFromThenTo @a)
