module SNEK.Data
( Data
, Datum(..)
) where

import Data.Map (Map)

type Data = [Datum]

data Datum
  = Symbol String
  | List [Datum]
  | Array [Datum]
  | Dict (Map Datum Datum)
  deriving (Eq, Ord, Show)
