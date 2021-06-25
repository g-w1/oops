open Types
val string_of_token : token -> string
val lex : string -> (token list, Types.lexer_error) result
