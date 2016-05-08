{-# LANGUAGE LambdaCase #-}
module SNEK.Type
( S(..)

, K(..)
, (*->*)

, T(..)
, (~->~)

, tK

, replaceVarT

, prettyK
, prettyT
) where

import Control.Lens ((%=), _1, _2)
import Control.Monad.State (evalState, gets, State)
import Data.List (sortOn)
import Data.Map (Map)

import qualified Data.Map as Map

infixr 5 *->*
infixr 5 ~->~

-- | Sort.
data S
  = KindS
  deriving (Eq, Show)

-- | Kind.
data K
  = TypeK
  | FuncK
  | RowK K
  | ApplyK K K
  deriving (Eq, Show)

(*->*) :: K -> K -> K
k *->* l = ApplyK (ApplyK FuncK k) l

-- | Type.
data T
  = BoolT
  | FuncT
  | StructT (Map String T)
  | ApplyT T T
  | VarT Int K
  | UniversalT Int K T
  deriving (Show)

instance Eq T where
  t == u = normalize t `eq` normalize u
    where eq :: T -> T -> Bool
          BoolT            `eq` BoolT               = True
          FuncT            `eq` FuncT               = True
          StructT fs       `eq` StructT fs'         =
            let sfs  = sortOn fst (Map.toList fs)
                sfs' = sortOn fst (Map.toList fs')
             in    length fs == length fs'
                && all (\((n, t), (n', t')) -> n == n' && t `eq` t')
                       (sfs `zip` sfs')
          ApplyT f a       `eq` ApplyT f' a'        = f `eq` f' && a `eq` a'
          VarT i k         `eq` VarT i' k'          = i == i' && k == k'
          UniversalT i k t `eq` UniversalT i' k' t' = i == i' && k == k' && t `eq` t'
          _                `eq` _                   = False

          normalize :: T -> T
          normalize t = evalState (go t) (0, Map.empty)
            where go :: T -> State (Int, Map Int Int) T
                  go BoolT              = return BoolT
                  go FuncT              = return FuncT
                  go (StructT fs)       = StructT <$> mapM go fs
                  go (ApplyT f a)       = ApplyT <$> go f <*> go a
                  go (VarT i k)         = gets (Map.lookup i . snd) >>= \case
                                            Just j  -> return $ VarT j k
                                            Nothing -> return $ VarT i k
                  go (UniversalT i k t) = do
                    i' <- do { _1 %= pred; gets fst }
                    _2 %= Map.insert i i'
                    UniversalT i' k <$> go t


(~->~) :: T -> T -> T
t ~->~ u = ApplyT (ApplyT FuncT t) u

-- | Return the kind of a type. The type must be well-kinded.
tK :: T -> K
tK BoolT = TypeK
tK FuncT = TypeK *->* TypeK *->* TypeK
tK (StructT _) = TypeK
tK (ApplyT c a) =
  case tK c of
    (ApplyK (ApplyK FuncK _) r) -> r
    _ -> error "tK: ill-kinded type"
tK (VarT _ k) = k

-- | Replace a 'VarT' everywhere.
replaceVarT :: Int -- ^ The ID of the 'VarT'.
            -> T   -- ^ The type to replace the 'VarT' in.
            -> T   -- ^ The type to replace the 'VarT' by.
            -> T
replaceVarT _ BoolT              _ = BoolT
replaceVarT _ FuncT              _ = FuncT
replaceVarT i (ApplyT c a)       b = ApplyT (replaceVarT i c b) (replaceVarT i a b)
replaceVarT i v@(VarT j k)       b = if i == j then b else v
replaceVarT i (UniversalT j k t) b = UniversalT j k (replaceVarT i t b)

prettyK :: K -> String
prettyK TypeK = "*"
prettyK FuncK = "->"
prettyK (ApplyK f a) = "(" ++ prettyK f ++ " " ++ prettyK a ++ ")"

prettyT :: T -> String
prettyT BoolT = "bool"
prettyT FuncT = "->"
prettyT (ApplyT f a) = "(" ++ prettyT f ++ " " ++ prettyT a ++ ")"
prettyT (VarT i k) = "[__" ++ show i ++ " " ++ prettyK k ++ "]"
prettyT (UniversalT i k t) =
  "(forall __" ++ show i ++ " " ++ prettyK k ++ " " ++ prettyT t ++ ")"
