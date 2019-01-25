{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies,
             MultiParamTypeClasses, UndecidableInstances,
             FlexibleInstances #-}

module Numeric.NonNegative
  ( NonNegative(),
    getNonNegative,
    toNonNegative,
    unsafeToNonNegative
  ) where

import Control.Exception
import Inj

newtype NonNegative a = NonNegative a
  deriving newtype (Eq, Ord, Show, Real)

getNonNegative :: NonNegative a -> a
getNonNegative (NonNegative a) = a

instance (Inj p a, Ord a, Num a) => Inj p (NonNegative a) where
  inj = unsafeToNonNegative . inj

toNonNegative :: (Ord a, Num a) => a -> Maybe (NonNegative a)
toNonNegative d =
  if d >= 0 then Just (NonNegative d) else Nothing

unsafeToNonNegative :: (Ord a, Num a) => a -> NonNegative a
unsafeToNonNegative d =
  if d >= 0 then NonNegative d else throw Underflow

instance (Ord a, Num a) => Num (NonNegative a) where
  NonNegative a + NonNegative b = NonNegative (a + b)
  NonNegative a - NonNegative b = unsafeToNonNegative (a - b)
  NonNegative a * NonNegative b = NonNegative (a * b)
  negate _ = throw Underflow
  abs = id
  signum (NonNegative a) = NonNegative (signum a)
  fromInteger = unsafeToNonNegative . fromInteger

instance (Ord a, Fractional a) => Fractional (NonNegative a) where
  NonNegative a / NonNegative b = NonNegative (a / b)
  recip (NonNegative a) = NonNegative (recip a)
  fromRational = unsafeToNonNegative . fromRational
