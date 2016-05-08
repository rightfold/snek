{
module SNEK.Read.Lex where
}

%wrapper "basic"

tokens :-
  $white              ;
  \,                  ;
  \;.*                ;
  \#\!.*              ; -- TODO: document, not comment

  [a-zA-Z_=>\*\.\-]+  { Identifier }

  \#t                 { const (BoolLiteral True) }
  \#f                 { const (BoolLiteral False) }

  \{                  { const BraceLeft }
  \}                  { const BraceRight }
  \[                  { const BracketLeft }
  \]                  { const BracketRight }
  \(                  { const ParenLeft }
  \)                  { const ParenRight }

{
data Token
  = Identifier String

  | BoolLiteral Bool

  | BraceLeft
  | BraceRight
  | BracketLeft
  | BracketRight
  | ParenLeft
  | ParenRight
  deriving (Eq, Show)
}
