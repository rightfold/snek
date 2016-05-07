module SNEK.Type
( K(..)
, (*->*)

, T(..)
, (~->~)

, tK
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
