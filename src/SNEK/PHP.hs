module SNEK.PHP
( ve2PHPS
, ve2PHPE
) where

import SNEK.AST (VE(..))

ve2PHPS :: (String -> String) -> VE ks ts vs -> String
ve2PHPS r e = r (ve2PHPE e)

ve2PHPE :: VE ks ts vs -> String
ve2PHPE (NameVE _ s) = "$" ++ s
ve2PHPE (ValueLambdaVE p _ b) =
     "function($" ++ p ++ ") {\n"
  ++ indent (ve2PHPS (\e -> "return " ++ e ++ ";\n") b)
  ++ "}"
ve2PHPE (TypeLambdaVE _ _ _ b) = ve2PHPE b
ve2PHPE (ValueApplyVE f a) = "(" ++ ve2PHPE f ++ ")(" ++ ve2PHPE a ++ ")"
ve2PHPE (TypeApplyVE f _) = ve2PHPE f

indent :: String -> String
indent = unlines . map ("    " ++) . lines
