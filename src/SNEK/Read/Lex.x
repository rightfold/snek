{
module SNEK.Read.Lex where
}

%wrapper "basic"

tokens :-
  $white              ;
  \,                  ;
  \;.*                ;

  [a-zA-Z_=>\*\.\-]+  { Identifier }

  \{                  { const BraceLeft }
  \}                  { const BraceRight }
  \[                  { const BracketLeft }
  \]                  { const BracketRight }
  \(                  { const ParenLeft }
  \)                  { const ParenRight }

{
data Token
  = Identifier String

  | BraceLeft
  | BraceRight
  | BracketLeft
  | BracketRight
  | ParenLeft
  | ParenRight
  deriving (Eq, Show)
}
