type token =
  (* Keyword Tokens*)
  | Kexport (** export*)
  | Kto (** to*)
  | Kif (** If*)
  | Kloop (** Loop*)
  | Kbreak (** break*)
  | Kfunc (** Function*)
  | Kreturn (** Return*)
  | Kextern (** External*)
  (* Iden tokens*)
  | Iden of string (** Identifier token*)
  | IntLit of string (** IntLit token*)
  | CharLit of char (** CharLit token *)
  | StrLit of string (** "srtlit" token *)
  | EndOfLine (** EndOfLine token (.)*)
  | Eof (** EndOfFile*)
  (* grouping*)
  | Lparen (** Left paren*)
  | Rparen (** Right paren*)
  | Comma (* Open block ('')*)
  | ExclaimMark (** Close block ('!')*)
  (* Binops*)
  | BoSet (** := *)
  | BoAss (** <- *)
  | BoPlus (** '+'*)
  | BoMinus (** '-'*)
  | BoMul (** '*'*)
  | BoG (** '>'*)
  | BoL (** '<'*)
  | BoGe (** '>='*)
  | BoLe (** '<='*)
  | BoE (** '=='*)
  | BoNe (** '!='*)
  | BoAnd (** and*)
  | BoOr (** or*)
  | OpenBrak (** [*)
  | CloseBrak (** ]*)
  (* Special people*)
  | AtSign (** @*)
  | Colon (** :*)

type lexer_error =
  | UnexpectedChar of {found: char; pos: int}
  | ExpectedChar of { found: char; expected: char; pos: int }
  | ExpectedCharFoundEof of {expected: char; pos: int; }
  | ExpectedAlphaNumFoundEof of {pos: int;}

type tokens_and_locs =
  { tokens: token list;
    locations: int list;
  }