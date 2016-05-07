module SNEK.Symbol
( KS(..)
, TS(..)
, VS(..)
) where

import SNEK.Type (K, T)

data KS = KS { ksK :: K } deriving (Show)

data TS = TS { tsT :: T } deriving (Show)

data VS = VS { vsT :: T } deriving (Show)
