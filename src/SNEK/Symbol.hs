module SNEK.Symbol
( KS(..)
, TS(..)
, VS(..)
) where

import SNEK.Type (K, T)

-- | Kind symbol.
data KS = KS { ksK :: K } deriving (Show)

-- | Type symbol.
data TS = TS { tsT :: T } deriving (Show)

-- | Value symbol.
data VS = VS { vsT :: T } deriving (Show)
