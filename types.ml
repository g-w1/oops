type token =
  (* Keyword Tokens*)
  (* kword for set*)
  | Kset
  (* kword for Change*)
  | Kchange
  (* export*)
  | Kexport
  (* to*)
  | Kto
  (* If*)
  | Kif
  (* Loop*)
  | Kloop
  (* break*)
  | Kbreak
  (* Function*)
  | Kfunc
  (* Return*)
  | Kreturn
  (* External*)
  | Kextern
  (* Iden tokens*)
  (* Identifier token*)
  | Iden of string
  (* IntLit token*)
  | IntLit of string
  (* CharLit token *)
  | CharLit of char
  (* "srtlit" token *)
  | StrLit of string
  (* EndOfLine token (.)*)
  | EndOfLine
  (* EndOfFile*)
  | Eof
  (* grouping*)
  (* Left paren*)
  | Lparen
  (* Right paren*)
  | Rparen
  (* | Open block ('')*)
  | Comma
  (* Close block ('!')*)
  | ExclaimMark
  (* Binops*)
  (* '+'*)
  | BoPlus
  (* '-'*)
  | BoMinus
  (* '*'*)
  | BoMul
  (* '>'*)
  | BoG
  (* '<'*)
  | BoL
  (* '>='*)
  | BoGe
  (* '<='*)
  | BoLe
  (* '=='*)
  | BoE
  (* '!='*)
  | BoNe
  (* and*)
  | BoAnd
  (* or*)
  | BoOr
  (* [*)
  | OpenBrak
  (* ]*)
  | CloseBrak
  (* Special people*)
  (* @*)
  | AtSign

type lexer_error = UnexpectedChar of {found: char; pos: int}
                 | ExpectedChar of { found: char; expected: char; pos: int }
                 | ExpectedCharFoundEof of {expected: char; pos: int; }
