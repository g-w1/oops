open Token_types

val lex : string -> (tokens_and_locs, lexer_error) result
