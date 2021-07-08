open Core
open Result.Let_syntax
(* open Result.Monad_infix *)

module TokenIndex = Token_types.TokenIndex
module NodeIndex = Ast_types.NodeIndex

type parser = {
  input: Token_types.token array;
  pos: int; (** position in input *)
  tl_nodes: Ast_types.NodeIndex.t list; (** toplevel nodes *)
  ast: Ast_types.node_tag list (** all the nodes! *)
}
(* helper funcs *)
let inc p = {p with pos=p.pos+1}
let curtok p = p.input.(p.pos)
let expect_eat_token p t =
  if Token_types.token_eq t (curtok p) then
    Ok (inc p)
  else
    Error (Ast_types.ExpectedFound (t, (curtok p)))

let add_node p node = { p with ast=p.ast@[node] }, NodeIndex.of_int ((List.length p.ast) + 1)
let add_tl_node p nodeindex = { p with tl_nodes=p.tl_nodes@[nodeindex] }
(* indiv parsing funcs *)
let p_iden p = match curtok p with
  | Iden _ -> Ok (add_node (inc p) (Ast_types.Ident (TokenIndex.of_int p.pos)))
  | found -> Error (Ast_types.ExpectedFound ((Token_types.Iden "ident"),  found))

let rec p_params p prev =
  let%bind (p, paramname) = p_iden p in
  let%bind p = expect_eat_token p Colon in
  let%bind (p, typename) = p_iden p in
  match curtok p with
  | Comma -> p_params (inc p) (prev@[(paramname, typename)])
  | Rparen -> Ok (p, prev@[(paramname, typename)])
  | found -> Error (Ast_types.ExpectedFound (Rparen, found))

let p_fnproto p =
  let%bind p = expect_eat_token p Kfunc in
  let%bind (p, name) = p_iden p in
  let%bind p = expect_eat_token p Lparen in
  let%bind (p, params) = p_params p [] in
  let%bind p = expect_eat_token p Rparen in
  let%map (p, ret) = p_iden p in
  p, {Ast_types.name; params; ret}

let p_body p =
  let%bind p = expect_eat_token p Comma in
  (* TODO stuff in body *)
  let%map p = expect_eat_token p ExclaimMark in
  p, []

(* the muscle *)
let rec parse_tl p =
  let open Token_types in
  if token_eq Eof (curtok p) then
    Ok (p.tl_nodes, p.ast)
  else
    match p.input.(p.pos) with
    | Kfunc ->
      let%bind (p, fnproto) = p_fnproto p in
      let%bind (p, body) = p_body p in
      let (p, fn) = add_node p (Function (fnproto, body)) in
      let p = add_tl_node p fn in
      parse_tl p
    | t -> Error (Ast_types.InvalidInTl t)
let parse input = parse_tl {input=Array.of_list input; pos=0; ast=[]; tl_nodes=[]}
