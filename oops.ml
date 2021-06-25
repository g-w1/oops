open Core
open Base


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

let string_of_token t = match t with
  | Kset -> "set"
  | Kchange -> "change"
  | Kexport -> "export"
  | Kto -> "to"
  | Kif -> "if"
  | Kloop -> "loop"
  | Kbreak -> "break"
  | Kfunc -> "func"
  | Kreturn -> "return"
  | Kextern -> "extern"
  | Iden s -> s
  | IntLit s -> s
  | EndOfLine -> "."
  | Eof -> "EOF"
  | Lparen -> "("
  | Rparen -> ")"
  | Comma -> ","
  | ExclaimMark -> "!"
  | BoPlus -> "+"
  | BoMinus -> "-"
  | BoMul -> "*"
  | BoG -> ">"
  | BoL -> "<"
  | BoGe -> ">="
  | BoLe -> "<="
  | BoE -> "="
  | BoNe -> "!="
  | BoAnd -> "and"
  | BoOr -> "or"
  | OpenBrak -> "["
  | CloseBrak -> "]"
  | AtSign -> "@"

(* TODO make inword/innum capture their own
 * intermediate_str instead of storing in lexer *)
type lexerstate = Start
                | InWord of string
                | InNum of string
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

type lexer = {str: string; state: lexerstate; pos: int; toks: token list}

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

let rec lex lexer =
  if lexer.pos >= String.length lexer.str then
    Ok lexer.toks
  else
    let c = String.get lexer.str lexer.pos in
    let finish_token lexer tok = lex { lexer with state=Start; pos=lexer.pos + 1; toks=lexer.toks@[tok] } in
    let change_state  lexer state = lex { lexer with state=state; pos=lexer.pos + 1 } in
    let incr lexer = lex { lexer with pos=lexer.pos + 1 } in
    match lexer.state with
    | Start ->
      (match c with
       | 'a'..'z' | 'A' .. 'Z' | '_' -> change_state lexer (InWord "")
       | '0'..'9' -> change_state lexer (InNum "")
       | '\n' | ' ' -> incr lexer
       | c -> (match single_tok_tok c with
           | Some t -> finish_token lexer t
           | None -> (match state_of_char c with
               | Some state -> change_state lexer state
               | None -> Error (UnexpectedChar (c, lexer.pos)))))
    | InComment -> (match c with
        | '}' -> change_state lexer Start
        | _ -> incr lexer)
    | _ -> Error (UnexpectedChar (c, lexer.pos))

let test_ez_file = In_channel.read_all "test.ez"

let mylexer = {str=test_ez_file; state=Start; pos=0; toks=[]}

let () = match lex mylexer with
  | Error UnexpectedChar (c, i) -> Out_channel.printf "unexpected char '%c' at %d\n" c i
  | Ok l -> let _ = (List.map ~f:(fun f -> Out_channel.print_string (string_of_token f); Out_channel.print_string " ") l) in Out_channel.print_endline ""
