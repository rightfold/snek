module Main where

import Control.Lens ((%~))
import Control.Monad.Reader (runReader)
import Control.Monad.Trans.Either (runEitherT)
import Data.Function ((&))
import SNEK.Check (checkVE, emptyE, eTSs, eVSs)
import SNEK.Parse (parseVE)
import SNEK.Read (readData)
import SNEK.Symbol (TS(..), VS(..))
import SNEK.Type ((~->~), T(..))

import qualified Data.Map as Map

main :: IO ()
main = interact $ \text ->
  case readData text of
    Just data_ ->
      case mapM parseVE data_ of
        Right ast ->
          case mapM (\e -> runReader (runEitherT (checkVE e)) env) ast of
            Right tast -> show tast ++ "\n"
            Left  err  -> show err ++ "\n"
        Left  err -> show err ++ "\n"
    Nothing -> show "read error\n"

  where env = emptyE
              & eVSs %~ Map.insert "not" (VS (BoolT ~->~ BoolT))
              & eTSs %~ Map.insert "bool" (TS BoolT)
