module SNEK.Read
( readData
) where

import SNEK.Data (Data)
import SNEK.Read.Lex (alexScanTokens)
import SNEK.Read.Parse (parse)

-- | Turn a string into a sequence of data.
readData :: String -> Maybe Data
readData = Just . parse . alexScanTokens
