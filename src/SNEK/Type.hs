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
  | ApplyK K K
  deriving (Eq, Show)

(*->*) :: K -> K -> K
k *->* l = ApplyK (ApplyK FuncK k) l

-- | Type.
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
