module Main where

import Control.Lens ((%~))
import Data.Function ((&))
import SNEK.Check (checkVE, emptyE, eKSs, eTSs, eVSs, runCheck, veT)
import SNEK.Parse (parseVE)
import SNEK.PHP (runPHPGen, ve2PHPM)
import SNEK.Read (readData)
import SNEK.Symbol (KS(..), TS(..), VS(..))
import SNEK.Type ((~->~), K(..), prettyT, T(..))

import qualified Data.Map as Map

main :: IO ()
main = interact $ \text ->
  case readData text of
    Just data_ ->
      case mapM parseVE data_ of
        Right ast ->
          case mapM check ast of
            Right tast -> concat tast
            Left  err  -> show err ++ "\n"
        Left  err -> show err ++ "\n"
    Nothing -> show "read error\n"

  where check e = case runCheck (checkVE e) env of
                    Left  er -> Left er
                    Right te -> Right $ runPHPGen (ve2PHPM te)
        env = emptyE
              & eKSs %~ Map.insert "*" (KS TypeK)
              & eKSs %~ Map.insert "->" (KS FuncK)
              & eTSs %~ Map.insert "->" (TS FuncT)
              & eTSs %~ Map.insert "bool" (TS BoolT)
              & eVSs %~ Map.insert "not" (VS (BoolT ~->~ BoolT))
              & eVSs %~ Map.insert "true" (VS BoolT) -- this will be a literal later
              & eVSs %~ Map.insert "false" (VS BoolT) -- this will be a literal later
