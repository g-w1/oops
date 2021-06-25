open Token_types

val string_of_token : token -> string
val lex : string -> (tokens_and_locs, lexer_error) result
