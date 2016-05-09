module Main where

import Control.Lens ((%~))
import Control.Monad (foldM_)
import Data.Function ((&))
import SNEK.Check (checkVE, emptyE, eKSs, eTSs, eVSs, runCheck, veT)
import SNEK.Parse (parseVE)
import SNEK.PHP (runPHPGen, ve2PHPM)
import SNEK.Read (readData)
import SNEK.Symbol (KS(..), TS(..), VS(..))
import SNEK.Type ((~->~), K(..), prettyT, T(..))
import System.Directory (makeAbsolute)
import System.Environment (getArgs)

import qualified Data.Map as Map

main :: IO ()
main = do
  sourceFiles <- getArgs >>= mapM makeAbsolute
  foldM_ go Map.empty sourceFiles
  where go ts file = do
          text <- readFile file
          let [datum] = fromJust $ readData text
          let ast = fromRight $ parseVE datum
          let tast = fromRight $ check ast
          let t = veT tast
          putStrLn $ runPHPGen (ve2PHPM tast)
          return $ Map.insert file t ts
          where check e = runCheck (checkVE e) env

                env = emptyE file ts
                      & eKSs %~ Map.insert "*" (KS TypeK)
                      & eKSs %~ Map.insert "->" (KS FuncK)
                      & eTSs %~ Map.insert "bool" (TS BoolT)
                      & eTSs %~ Map.insert "->" (TS FuncT)

                fromRight (Right x) = x
                fromRight (Left x) = error (show x)

                fromJust (Just x) = x
                fromJust _ = error "nein"
