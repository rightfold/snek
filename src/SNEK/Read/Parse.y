{
module SNEK.Read.Parse
( parse
) where

import SNEK.Data (Datum(..))
import SNEK.Read.Lex (Token(..))
}

%name parse
%tokentype { Token }
%error { error . show }

%token
  identifier          { Identifier $$ }

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

Symbol : identifier { Symbol $1 }

List : '(' Data ')' { List $2 }

Array : '[' Data ']' { Array $2 }
