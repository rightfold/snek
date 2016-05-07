{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module SNEK.Parse
( ParseError(..)

, parseTE
, parseVE
) where

import Control.Monad (forM, when)
import Data.List.Split (chunksOf)
import SNEK.AST (TE(..), VE(..))
import SNEK.Data (Datum(..))

data ParseError
  = CallWithoutCallee
  | CallWithoutArgument
  | IllFormedSpecialForm
  deriving (Eq, Show)

parseTE :: Datum -> Either ParseError (TE ())
parseTE (Symbol name) = return $ NameTE () name
parseTE _ = error "not yet implemented"

parseVE :: Datum -> Either ParseError (VE () () ())
parseVE (Symbol name)    = return $ NameVE () name
parseVE (List  [])       = throwError CallWithoutCallee
parseVE (Array [])       = throwError CallWithoutCallee
parseVE (List  [_])      = throwError CallWithoutArgument
parseVE (Array [_])      = throwError CallWithoutArgument
parseVE (List  (f : as)) = do
  special <- parseVESpecial f as
  case special of
    Just ve -> return ve
    Nothing -> do
      f' <- parseVE f
      as' <- mapM parseVE as
      return $ foldl ValueApplyVE f' as'
parseVE (Array (f : as)) = do
  f' <- parseVE f
  as' <- mapM parseTE as
  return $ foldl TypeApplyVE f' as'

parseVESpecial :: Datum -> [Datum] -> Either ParseError (Maybe (VE () () ()))
parseVESpecial (Symbol "fn") as = Just <$> go
  where go = case as of
          [List ps, b] -> do
            ps' <- parseParams ps
            b' <- parseVE b
            return $ foldr (\(p, t) -> ValueLambdaVE p t) b' ps'
          _ -> throwError IllFormedSpecialForm
        parseParams ps = do
          let pairs = (chunksOf 2 ps)
          when (null pairs) (throwError IllFormedSpecialForm)
          forM pairs $ \case
            [Symbol n, t] -> (n,) <$> parseTE t
            _ -> throwError IllFormedSpecialForm
parseVESpecial _ _ = return Nothing

throwError :: a -> Either a b
throwError = Left
