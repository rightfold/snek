{
module SNEK.Read.Lex where
}

%wrapper "basic"

tokens :-
  $white              ;
  \,                  ;
  \;.*                ;

  [a-zA-Z_=>\*\.\-]+  { Identifier }

  \[                  { const BracketLeft }
  \]                  { const BracketRight }
  \(                  { const ParenLeft }
  \)                  { const ParenRight }

{
data Token
  = Identifier String

  | BracketLeft
  | BracketRight
  | ParenLeft
  | ParenRight
  deriving (Eq, Show)
}
