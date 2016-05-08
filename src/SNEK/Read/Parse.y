{
module SNEK.Read.Parse
( parse
) where

import SNEK.Data (Datum(..))
import SNEK.Read.Lex (Token(..))

import qualified Data.Map as Map
}

%name parse
%tokentype { Token }
%error { error . show }

%token
  identifier          { Identifier $$ }

  '{'                 { BraceLeft }
  '}'                 { BraceRight }
  '['                 { BracketLeft }
  ']'                 { BracketRight }
  '('                 { ParenLeft }
  ')'                 { ParenRight }

%%

Data :            { [] }
     | Datum Data { $1 : $2 }

Datum : Symbol { $1 }
      | List   { $1 }
      | Array  { $1 }
      | Dict   { $1 }

Symbol : identifier { Symbol $1 }

List : '(' Data ')' { List $2 }

Array : '[' Data ']' { Array $2 }

Dict : '{' DatumPairs '}' { Dict (Map.fromList $2) }

DatumPairs :                        { [] }
           | Datum Datum DatumPairs { ($1, $2) : $3 }
