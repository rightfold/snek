module Main where

import Control.Monad.Reader (runReader)
import Control.Monad.Trans.Either (runEitherT)
import SNEK.Check (checkVE, emptyE)
import SNEK.Parse (parseVE)
import SNEK.Read (readData)

main :: IO ()
main = interact $ \text ->
  case readData text of
    Just data_ ->
      case mapM parseVE data_ of
        Right ast ->
          case mapM (\e -> runReader (runEitherT (checkVE e)) emptyE) ast of
            Right tast -> show tast ++ "\n"
            Left  err  -> show err ++ "\n"
        Left  err -> show err ++ "\n"
    Nothing -> show "read error\n"
