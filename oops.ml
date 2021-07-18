open Core
let print_token t = Out_channel.print_string (Token_types.show_token t); Out_channel.print_string " "
let print_tokens (tl:Token_types.tokens_and_locs) = ignore (Array.map ~f:print_token tl.tokens); Out_channel.print_endline ""

let print_token_error = function
  | Token_types.UnexpectedChar {found; pos} -> Out_channel.printf "unexpected char '%c' at %d\n" found pos
  | Token_types.ExpectedChar {found; expected;  pos} -> Out_channel.printf "expected char '%c' at %d, found '%c'\n" expected pos found
  | Token_types.ExpectedCharFoundEof {expected; pos} -> Out_channel.printf "expected char '%c' at %d, found EOF\n" expected pos
  | Token_types.ExpectedAlphaNumFoundEof {pos} -> Out_channel.printf "expected an alphanumeric char at %d, found EOF\n" pos

let print_ast_error e tokens =
  let get t = Token_types.show_token(tokens.(Token_types.TokenIndex.to_int t)) in
  match e with
  | Ast_types.InvalidInTl t -> "invalid token in top level" ^ (get t)
  | Ast_types.InvalidStartOfExpr t -> "invalid start of expr " ^ Token_types.show_token(tokens.(Token_types.TokenIndex.to_int t))
  | Ast_types.ExpectedFound (ex, f) -> Printf.sprintf "expected %s, found %s" (Token_types.show_token(ex)) (get f)
  | Ast_types.ExpectedFoundStr (ex, f) -> Printf.sprintf "expected %s, found %s" ex (get f)
  | Ast_types.SetWithNonIdentIsInvalid _ -> "TODO print ast nodes: set with non ident is invalid"
  | Ast_types.InvalidTODOSPECIFIC -> "TODO"

let () = let mlexed =  (Tokenizer.lex (In_channel.read_all "test.ez")) in match mlexed with
  | Ok lexed -> print_tokens lexed;  (match Parser.parse lexed.tokens with
      | Ok o -> ignore (Array.map ~f:(fun t -> Out_channel.print_string (Ast_types.show_node_tag t)) (snd o));
        Out_channel.print_endline ""
      | Error e -> Out_channel.print_endline (print_ast_error e lexed.tokens))
  | Error e -> print_token_error e
