module Main where

import Control.Lens ((%~))
import Data.Function ((&))
import SNEK.Check (checkVE, emptyE, eKSs, eTSs, eVSs, runCheck)
import SNEK.Parse (parseVE)
import SNEK.Read (readData)
import SNEK.Symbol (KS(..), TS(..), VS(..))
import SNEK.Type ((~->~), K(..), T(..))

import qualified Data.Map as Map

main :: IO ()
main = interact $ \text ->
  case readData text of
    Just data_ ->
      case mapM parseVE data_ of
        Right ast ->
          case mapM (\e -> runCheck (checkVE e) env) ast of
            Right tast -> show tast ++ "\n"
            Left  err  -> show err ++ "\n"
        Left  err -> show err ++ "\n"
    Nothing -> show "read error\n"

  where env = emptyE
              & eKSs %~ Map.insert "*" (KS TypeK)
              & eTSs %~ Map.insert "bool" (TS BoolT)
              & eVSs %~ Map.insert "not" (VS (BoolT ~->~ BoolT))
