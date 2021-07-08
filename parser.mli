(** parse an ez file at toplevel
 ** first tuple is the top level decls, second is the ast*)
val parse : Token_types.token list -> ((Ast_types.NodeIndex.t list * Ast_types.node_tag list), Ast_types.ast_error) result
