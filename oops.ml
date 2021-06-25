open Core
open Base


let toks_and_locs = match Tokenizer.lex (In_channel.read_all "test.ez") with
  | Error Token_types.UnexpectedChar {found; pos} -> Out_channel.printf "unexpected char '%c' at %d\n" found pos; None
  | Error Token_types.ExpectedChar {found; expected;  pos} -> Out_channel.printf "expected char '%c' at %d, found '%c'\n" expected pos found; None
  | Error Token_types.ExpectedCharFoundEof {expected; pos} -> Out_channel.printf "expected char '%c' at %d, found EOF\n" expected pos; None
  | Error Token_types.ExpectedAlphaNumFoundEof {pos} -> Out_channel.printf "expected an alphanumeric char at %d, found EOF\n" pos; None
  | Ok l -> Some l
let () = match toks_and_locs with
  | Some l ->
    let _ = (List.map ~f:(fun f -> Out_channel.print_string (Tokenizer.string_of_token f); Out_channel.print_string " ") l.tokens) in Out_channel.print_endline "";
    let _ = Parser.parse l.tokens in ()
  | None -> ()
