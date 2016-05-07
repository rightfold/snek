module SNEK.Read
( readData
) where

import SNEK.Data (Data)
import SNEK.Read.Lex (alexScanTokens)
import SNEK.Read.Parse (parse)

readData :: String -> Maybe Data
readData = Just . parse . alexScanTokens
