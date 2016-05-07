module SNEK.Data
( Data
, Datum(..)
) where

type Data = [Datum]

data Datum
  = Symbol String
  | List [Datum]
  | Array [Datum]
  deriving (Eq, Show)
