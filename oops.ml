open Core
open Base

let read_file fname = In_channel.read_all fname

exception Lex_error of (char * int)

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

type lexerstate = Start
                | Inword
                | InNum
                | SawLessThan
                | SawEquals
                | SawGreaterThan
                | SawBang
                | InComment
                | InCharLit
                | InCharLitForwardSlash
                | InStrLit
                | InStrLitForwardSlash

let get_kword s = match s with
  | "Set" | "set" -> Some Kset
  | "external" | "External" -> Some Kextern
  | "Change" | "change" -> Some Kchange
  | "to" -> Some Kto
  | "If" | "if" -> Some Kif
  | "Loop" | "loop" -> Some Kloop
  | "Break" | "break" -> Some Kbreak
  | "and" | "And" -> Some BoAnd
  | "or" | "Or" -> Some BoOr
  | "function" | "Function" -> Some Kfunc
  | "return" | "Return" -> Some Kreturn
  | "export" | "Export" -> Some Kexport
  | _ -> None

type lexer = { state: lexerstate; intermediate_str: string; pos: int}

let defaultlexer = { state=Start; intermediate_str=""; pos=0 }

let single_tok_tok c = match c with
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
  | _ -> None

let state_of_char c = match c with
  | '!' -> Some SawBang
  | '>' -> Some SawGreaterThan
  | '<' -> Some SawLessThan
  | '=' -> Some SawEquals
  | '{' -> Some InComment
  | '"' -> Some InStrLit
  | '\'' -> Some InStrLit
  | _ -> None

type lexer_error = UnexpectedChar of (char * int)



let rec lex s lexer toks =
  if lexer.pos >= String.length s
  then
    Ok toks
  else
    let c = String.get s lexer.pos in
    let finish_token s lexer toks tok = lex s { state=Start; intermediate_str=""; pos=lexer.pos + 1 } (toks@[tok]) in
    let change_state s lexer toks state = lex s { state=state; intermediate_str=lexer.intermediate_str; pos=lexer.pos + 1 } toks in
    match lexer.state with
    | Start ->
      (match c with
       | 'a'..'z' | 'A' .. 'Z' | '_' -> lex s { state=Inword; intermediate_str=Char.to_string c; pos=lexer.pos + 1} toks
       | '0'..'9' -> lex s { state=InNum; intermediate_str=Char.to_string c; pos=lexer.pos + 1} toks
       | '\n' | ' ' -> lex s { state=lexer.state; intermediate_str=""; pos=lexer.pos + 1 } toks
       | c -> (match single_tok_tok c with
           | Some t -> finish_token s lexer toks t
           | None -> (match state_of_char c with
               | Some state -> change_state s lexer toks state
               | None -> Error (UnexpectedChar (c, lexer.pos)))
         )
      )
    | _ -> Error (UnexpectedChar (c, lexer.pos))
