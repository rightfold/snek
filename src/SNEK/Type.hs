module SNEK.Type
( K(..)
, (*->*)

, T(..)
, (~->~)

, tK

, replaceVarT
) where

infixr 5 *->*
infixr 5 ~->~

data K
  = TypeK
  | FuncK
  | ApplyK K K
  deriving (Eq, Show)

(*->*) :: K -> K -> K
k *->* l = ApplyK (ApplyK FuncK k) l

data T
  = BoolT
  | FuncT
  | ApplyT T T
  | VarT Int K
  | UniversalT Int K T
  deriving (Eq, Show)

(~->~) :: T -> T -> T
t ~->~ u = ApplyT (ApplyT FuncT t) u

-- | Return the kind of a type. The type must be well-kinded.
tK :: T -> K
tK BoolT = TypeK
tK FuncT = TypeK *->* TypeK *->* TypeK
tK (ApplyT c a) =
  case tK c of
    (ApplyK (ApplyK FuncK _) r) -> r
    _ -> error "tK: ill-kinded type"
tK (VarT _ k) = k

replaceVarT :: Int -> T -> T -> T
replaceVarT _ BoolT              _ = BoolT
replaceVarT _ FuncT              _ = FuncT
replaceVarT i (ApplyT c a)       b = ApplyT (replaceVarT i c b) (replaceVarT i a b)
replaceVarT i v@(VarT j k)       b = if i == j then b else v
replaceVarT i (UniversalT j k t) b = UniversalT j k (replaceVarT i t b)
