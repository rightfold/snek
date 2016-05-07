{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module SNEK.Parse
( ParseError(..)

, parseTE
, parseVE
) where

import Control.Monad (forM, when)
import Data.List.Split (chunksOf)
import SNEK.AST (KE(..), TE(..), VE(..))
import SNEK.Data (Datum(..))

data ParseError
  = CallWithoutCallee
  | CallWithoutArgument
  | IllFormedSpecialForm
  deriving (Eq, Show)

parseKE :: Datum -> Either ParseError (KE ())
parseKE (Symbol name) = return $ NameKE () name
parseKE _ = error "not yet implemented"

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
          [List  ps, b] -> go' ValueLambdaVE ps b parseTE
          [Array ps, b] -> go' TypeLambdaVE  ps b parseKE
          _ -> throwError IllFormedSpecialForm
        go' node ps b parseTK = do
          ps' <- parseParams ps parseTK
          b' <- parseVE b
          return $ foldr (\(p, t) -> node p t) b' ps'
        parseParams ps parseTK = do
          let pairs = (chunksOf 2 ps)
          when (null pairs) (throwError IllFormedSpecialForm)
          forM pairs $ \case
            [Symbol n, t] -> (n,) <$> parseTK t
            _ -> throwError IllFormedSpecialForm
parseVESpecial _ _ = return Nothing

throwError :: a -> Either a b
throwError = Left
