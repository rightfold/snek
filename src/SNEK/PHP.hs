module SNEK.PHP
( PHPGen
, runPHPGen
, ve2PHPS
, ve2PHPE
) where

import Control.Monad.Reader (ask, local, Reader, runReader)
import Data.Set (Set)
import SNEK.AST (VE(..))

import qualified Data.Set as Set

type PHPGen = Reader (Set String)

runPHPGen :: PHPGen a -> a
runPHPGen g = runReader g Set.empty

ve2PHPS :: (String -> String) -> VE ks ts vs -> PHPGen String
ve2PHPS r e = r <$> ve2PHPE e

ve2PHPE :: VE ks ts vs -> PHPGen String
ve2PHPE (NameVE _ s) = return $ "$" ++ s
ve2PHPE (ValueLambdaVE p _ b) = do
  use <- mkUse <$> ask
  b' <- local (Set.insert p) $ ve2PHPS (\e -> "return " ++ e ++ ";\n") b
  return $ "function($" ++ p ++ ")" ++ use ++ " {\n" ++ indent b' ++ "}"
  where mkUse vs | Set.null vs = ""
                 | otherwise   = " use(" ++ (Set.toList vs >>= ("$" ++)) ++ ")"
ve2PHPE (TypeLambdaVE _ _ _ b) = ve2PHPE b
ve2PHPE (ValueApplyVE f a) = do
  f' <- ve2PHPE f
  a' <- ve2PHPE a
  return $ "(" ++ f' ++ ")(" ++ a' ++ ")"
ve2PHPE (TypeApplyVE f _) = ve2PHPE f

indent :: String -> String
indent = unlines . map ("    " ++) . lines
