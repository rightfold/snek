module SNEK.AST
( KE(..)
, TE(..)
, VE(..)
) where

-- | Kind expression.
data KE ks
  -- | Reference to named kind.
  = NameKE ks String
  deriving (Show)

-- | Type expression.
data TE ts
  -- | Reference to named type.
  = NameTE ts String
  deriving (Show)

-- | Value expression.
data VE ks ts vs
  -- | Reference to named value.
  = NameVE vs String

  -- | Value-dependent value.
  | ValueLambdaVE String (TE ts) (VE ks ts vs)

  -- | Type-dependent value.
  | TypeLambdaVE String (KE ks) (VE ks ts vs)

  -- | Apply value-dependent value.
  | ValueApplyVE (VE ks ts vs) (VE ks ts vs)

  -- | Apply type-dependent value.
  | TypeApplyVE (VE ks ts vs) (TE ts)
  deriving (Show)
