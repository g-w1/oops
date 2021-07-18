(** parse an ez file at toplevel
 ** first tuple is the top level decls, second is the ast*)
val parse : Token_types.token array -> ((Ast_types.NodeIndex.t list * Ast_types.node_tag array), Ast_types.ast_error) result
