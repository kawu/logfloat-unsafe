{-# LANGUAGE ForeignFunctionInterface #-}

-- | Computations in logarithmic scale.

module Data.Number.LogFloat.Unsafe
( LogFloat (..)
, zero
, one
) where

import Prelude hiding (sum, product)
import Data.List (foldl')

newtype LogFloat = LogFloat { unLog :: Double }
	deriving (Show, Read, Eq, Ord)

foreign import ccall unsafe "math.h log1p"
    log1p :: Double -> Double

{-# INLINE zero' #-}
zero' :: Double
zero' = -(1/0)

{-# INLINE one' #-}
one' :: Double
one' = 0

{-# INLINE zero #-}
zero :: LogFloat
zero = LogFloat zero' 

{-# INLINE one #-}
one :: LogFloat
one = LogFloat one'

{-# INLINE isZero' #-}
isZero' :: Double -> Bool
isZero' = (zero'==)

{-# INLINE isZero #-}
isZero :: LogFloat -> Bool
isZero = isZero' . unLog

{-# INLINE logFloat #-}
logFloat :: Double -> LogFloat
logFloat = LogFloat . log

{-# INLINE fromLogFloat #-}
fromLogFloat :: LogFloat -> Double
fromLogFloat = exp . unLog

instance Num LogFloat where
    LogFloat x * LogFloat y = LogFloat $ x + y
    LogFloat x + LogFloat y
        | isZero' x = LogFloat $ y
        | x > y     = LogFloat $ x + log1p(exp(y - x))
        | otherwise = LogFloat $ y + log1p(exp(x - y))
    LogFloat x - LogFloat y = error "LogFloat: (-) not supported"
    negate _    = error "LogFloat: negate not supported"
    abs         = id
    signum (LogFloat x)
        | x > zero' = 1
        | otherwise = 0
    fromInteger x
        | x == 0    = zero
        | x > 0     = LogFloat . log . fromInteger $ x
        | otherwise = error "LogFloat: fromInteger on negative argument"

instance Fractional LogFloat where
    LogFloat x / LogFloat y = LogFloat $ x - y
    fromRational x
        | x == 0    = zero
        | x > 0     = LogFloat . log . fromRational $ x
        | otherwise = error "LogFloat: fromRational on negative argument"
