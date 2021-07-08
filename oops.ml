open Core
open Result.Monad_infix

let print_token t = Out_channel.print_string (Token_types.show_token t); Out_channel.print_string " "
let print_tokens (tl:Token_types.tokens_and_locs) = ignore (List.map ~f:print_token tl.tokens); Out_channel.print_endline ""

let print_token_error = function
  | Token_types.UnexpectedChar {found; pos} -> Out_channel.printf "unexpected char '%c' at %d\n" found pos
  | Token_types.ExpectedChar {found; expected;  pos} -> Out_channel.printf "expected char '%c' at %d, found '%c'\n" expected pos found
  | Token_types.ExpectedCharFoundEof {expected; pos} -> Out_channel.printf "expected char '%c' at %d, found EOF\n" expected pos
  | Token_types.ExpectedAlphaNumFoundEof {pos} -> Out_channel.printf "expected an alphanumeric char at %d, found EOF\n" pos

let () = match (Tokenizer.lex (In_channel.read_all "test.ez") >>| fun l -> print_tokens l; Parser.parse l.tokens) with
  | Ok eo -> (match eo with
      | Ok o -> ignore (List.map ~f:(fun t -> Out_channel.print_string (Ast_types.show_node_tag t)) (snd o))
      | Error e -> Out_channel.print_endline (Ast_types.show_ast_error e))
  | Error e -> print_token_error e
