module SNEK.Parse
( ParseError(..)

, parseTE
, parseVE
) where

import SNEK.AST (TE(..), VE(..))
import SNEK.Data (Datum(..))

data ParseError
  = CallWithoutCallee
  | CallWithoutArgument
  deriving (Eq, Show)

parseTE :: Datum -> Either ParseError (TE ())
parseTE (Symbol name) = return $ NameTE () name
parseTE _ = error "not yet implemented"

parseVE :: Datum -> Either ParseError (VE () () ())
parseVE (Symbol name)    = return $ NameVE () name
parseVE (List  [])       = Left CallWithoutCallee
parseVE (Array [])       = Left CallWithoutCallee
parseVE (List  [_])      = Left CallWithoutArgument
parseVE (Array [_])      = Left CallWithoutArgument
parseVE (List  (f : as)) = do
  f' <- parseVE f
  as' <- mapM parseVE as
  return $ foldl ValueApplyVE f' as'
parseVE (Array (f : as)) = do
  f' <- parseVE f
  as' <- mapM parseTE as
  return $ foldl TypeApplyVE f' as'
