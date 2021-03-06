open Base


type lexerstate = Start
                | InWord of string
                | InNum of string
                | SawLessThan
                | SawGreaterThan
                | SawBang
                | InComment
                | SawColon
                | InCharLit
                | InCharLitAteChar of char
                | InCharLitForwardSlash
                | InStrLit of string
                | InStrLitForwardSlash of string

let get_kword s = let open Token_types in match s with
  | "external" | "External" -> Kextern
  | "to" -> Kto
  | "If" | "if" -> Kif
  | "Loop" | "loop" -> Kloop
  | "Break" | "break" -> Kbreak
  | "and" | "And" -> BoAnd
  | "or" | "Or" -> BoOr
  | "function" | "Function" -> Kfunc
  | "return" | "Return" -> Kreturn
  | "export" | "Export" -> Kexport
  | s -> Iden s

type lexer = {str: string; state: lexerstate; pos: int; toks: Token_types.token array; locs: int array}

let single_tok_tok c = let open Token_types in match c with
  | '.' -> Some EndOfLine
  | ',' -> Some Comma
  | '(' -> Some Lparen
  | ')' -> Some Rparen
  | '+' -> Some BoPlus
  | '*' -> Some BoMul
  | '-' -> Some BoMinus
  | '[' -> Some OpenBrak
  | '@' -> Some AtSign
  | ']' -> Some CloseBrak
  | '=' -> Some BoE
  | _ -> None

let state_of_char c = match c with
  | '!' -> Some SawBang
  | '>' -> Some SawGreaterThan
  | '<' -> Some SawLessThan
  | '{' -> Some InComment
  | ':' -> Some SawColon
  | '"' -> Some (InStrLit "")
  | '\'' -> Some (InStrLit "")
  | _ -> None


let rec _lex lexer =
  let finish_token lexer tok = _lex { lexer with state=Start; pos=lexer.pos + 1; toks=Array.append lexer.toks [|tok|]; locs=Array.append lexer.locs [|lexer.pos|] } in
  let change_state  lexer state = _lex { lexer with state=state; pos=lexer.pos + 1 } in
  let finish_and_put_back lexer tok = _lex { lexer with state=Start; pos=lexer.pos; toks=Array.append lexer.toks [|tok|]; locs=Array.append lexer.locs [|lexer.pos|] } in
  let incr lexer = _lex { lexer with pos=lexer.pos + 1 } in
  let eat_char lexer c t =
    if lexer.pos >= String.length lexer.str then
      Error (Token_types.ExpectedCharFoundEof { expected=c; pos=lexer.pos;})
    else
      let ch = (String.get lexer.str lexer.pos) in
      if Char.(c = ch) then
        finish_token lexer t
      else
        Error (ExpectedChar {expected=c; found=ch; pos=lexer.pos}) in
  if lexer.pos >= String.length lexer.str then
    match lexer.state with
    | SawBang -> finish_token lexer ExclaimMark
    | InWord s -> finish_token lexer (get_kword s)
    | InNum s -> finish_token lexer (IntLit s)
    | SawGreaterThan -> finish_token lexer BoG
    | SawLessThan -> finish_token lexer BoL
    | InCharLit -> Error (ExpectedAlphaNumFoundEof {pos=lexer.pos})
    | InCharLitAteChar _ -> Error (ExpectedCharFoundEof {expected='\''; pos=lexer.pos})
    | InStrLit _ | InStrLitForwardSlash _ -> Error (ExpectedCharFoundEof {expected='"'; pos=lexer.pos})
    | _ -> Ok ({Token_types.tokens=Array.append lexer.toks [|Eof|]; locations=Array.append lexer.locs [|-1|]})
  else
    let c = String.get lexer.str lexer.pos in
    match lexer.state with
    | Start ->
      (match c with
       | 'a'..'z' | 'A' .. 'Z' | '_' -> change_state lexer (InWord (Char.to_string c))
       | '0'..'9' -> change_state lexer (InNum (Char.to_string c))
       | '\'' -> change_state lexer InCharLit
       | '\n' | ' ' -> incr lexer
       | '/' -> finish_token lexer BoDiv
       | c -> (match single_tok_tok c with
           | Some t -> finish_token lexer t
           | None -> (match state_of_char c with
               | Some state -> change_state lexer state
               | None -> Error (UnexpectedChar {found=c; pos=lexer.pos}))))
    | InComment -> (match c with
        | '}' -> change_state lexer Start
        | _ -> incr lexer)
    | InWord s -> (match c with
        | 'a'..'z'|'A'..'Z'|'0'..'9'|'_' -> change_state lexer (InWord (s ^ String.make 1 c))
        | _ -> finish_and_put_back lexer (get_kword s))
    | InNum s -> (match c with
        | '0'..'9'|'_' -> change_state lexer (InNum (s ^ Char.to_string c))
        | _ -> finish_and_put_back lexer (IntLit s))
    | InCharLit -> (match c with
        | '\\' -> change_state lexer InCharLitForwardSlash
        | c -> change_state lexer (InCharLitAteChar c))
    | InCharLitForwardSlash -> (match c with
        | 'n' -> change_state lexer (InCharLitAteChar '\n')
        | 't' -> change_state lexer (InCharLitAteChar '\t')
        | _ -> Error (UnexpectedChar {found=c; pos=lexer.pos}))
    | InCharLitAteChar c -> eat_char lexer '\'' (CharLit c)
    | InStrLit s-> (match c with
        | '\\' -> change_state lexer (InStrLitForwardSlash s)
        | '"' -> finish_token lexer (StrLit s)
        | c -> change_state lexer (InStrLit (s ^ Char.to_string c)))
    | InStrLitForwardSlash s -> (match c with
        | 'n' -> change_state lexer (InStrLit (s ^ Char.to_string '\n'))
        | 't' -> change_state lexer (InStrLit (s ^ Char.to_string '\t'))
        | c -> Error (UnexpectedChar {found=c; pos=lexer.pos}))
    | SawGreaterThan -> (match c with
        | '=' -> finish_token lexer BoGe
        | _ -> finish_and_put_back lexer BoG)
    | SawBang -> (match c with
        | '=' -> finish_token lexer BoNe
        | _ -> finish_and_put_back lexer ExclaimMark)
    | SawColon -> (match c with
        | '=' -> finish_token lexer BoSet
        | _ -> finish_and_put_back lexer Colon)
    | SawLessThan -> (match c with
        | '=' -> finish_token lexer BoLe
        | '-' -> finish_token lexer BoAss
        | _ -> finish_and_put_back lexer BoL)

let lex str = _lex { str; state=Start; pos=0; toks=[||]; locs=[||]}
