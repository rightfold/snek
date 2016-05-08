module SNEK.PHP
( PHPGen
, runPHPGen
, ve2PHPM
, ve2PHPS
, ve2PHPE
) where

import Control.Monad (forM)
import Control.Monad.Reader (ask, local, Reader, runReader)
import Data.List (intercalate)
import Data.Set (Set)
import SNEK.AST (VE(..))

import qualified Data.Map as Map
import qualified Data.Set as Set

type PHPGen = Reader (Set String)

runPHPGen :: PHPGen a -> a
runPHPGen g = runReader g Set.empty

ve2PHPM :: VE ks ts vs -> PHPGen String
ve2PHPM e = do
  s <- ve2PHPS (\e' -> "return " ++ e' ++ ";\n") e
  return $ "<?php\nreturn (function() {\n" ++ indent s ++ "})();\n"

ve2PHPS :: (String -> String) -> VE ks ts vs -> PHPGen String
ve2PHPS r (LetVE n v b) =
  local (Set.insert n) $
    (++) <$> ve2PHPS (assign n) v
         <*> ve2PHPS r b
ve2PHPS r (LetRecVE bds b) =
  local (\s -> foldl (\s (n, _, _) -> Set.insert n s) s bds) $
    (++) <$> (concat <$> forM bds (\(n, _, v) -> ve2PHPS (assign n) v))
         <*> ve2PHPS r b
ve2PHPS r e = r <$> ve2PHPE e

ve2PHPE :: VE ks ts vs -> PHPGen String
ve2PHPE (NameVE _ s) = return $ "$" ++ s
ve2PHPE (StructVE fs) = do
  fs' <- mapM ve2PHPE fs
  return $ "(object)[\n"
        ++ indent (Map.toList fs' >>= \(f, v) -> "'" ++ f ++ "' => " ++ v ++ ",\n")
        ++ "]"
ve2PHPE (StructReadVE f s) = do
  s' <- ve2PHPE s
  return $ "(" ++ s' ++ ")->" ++ f
ve2PHPE (ValueLambdaVE p _ b) = do
  use <- mkUse <$> ask
  b' <- local (Set.insert p) $ ve2PHPS (\e -> "return " ++ e ++ ";\n") b
  return $ "function($" ++ p ++ ")" ++ use ++ " {\n" ++ indent b' ++ "}"
  where mkUse vs | Set.null vs = ""
                 | otherwise   = " use(" ++ intercalate ", " (map ("&$" ++) (Set.toList vs)) ++ ")"
ve2PHPE (TypeLambdaVE _ _ _ b) = ve2PHPE b
ve2PHPE (ValueApplyVE f a) = do
  f' <- ve2PHPE f
  a' <- ve2PHPE a
  return $ "(" ++ f' ++ ")(" ++ a' ++ ")"
ve2PHPE (TypeApplyVE f _) = ve2PHPE f

indent :: String -> String
indent = unlines . map ("    " ++) . lines

assign :: String -> String -> String
assign n e = "$" ++ n ++ " = " ++ e ++ ";\n"
