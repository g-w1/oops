open Core
open Base


let () = match Tokenizer.lex (In_channel.read_all "test.ez") with
  | Error UnexpectedChar {found; pos} -> Out_channel.printf "unexpected char '%c' at %d\n" found pos
  | Error ExpectedChar {found; expected;  pos} -> Out_channel.printf "expected char '%c' at %d, found '%c'\n" expected pos found
  | Error ExpectedCharFoundEof {expected; pos} -> Out_channel.printf "expected char '%c' at %d, found EOF\n" expected pos
  | Error ExpectedAlphaNumFoundEof {pos} -> Out_channel.printf "expected an alphanumeric char at %d, found EOF\n" pos
  | Ok l -> let _ = (List.map ~f:(fun f -> Out_channel.print_string (Tokenizer.string_of_token f); Out_channel.print_string " ") l) in Out_channel.print_endline ""
