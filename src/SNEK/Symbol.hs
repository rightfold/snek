module SNEK.Symbol
( KS(..)
, TS(..)
, VS(..)
) where

import SNEK.Type (K, T)

data KS = KS K

data TS = TS T

data VS = VS T
