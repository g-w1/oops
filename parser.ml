open Core
open Result.Let_syntax
(* open Result.Monad_infix *)

module TokenIndex = Token_types.TokenIndex
module NodeIndex = Ast_types.NodeIndex

type parser = {
  input: Token_types.token array;
  pos: int; (** position in input *)
  tl_nodes: Ast_types.NodeIndex.t list; (** toplevel nodes *)
  ast: Ast_types.node_tag array (** all the nodes! *)
}
(* helper funcs *)
let inc p = {p with pos=p.pos+1}
let curtok p = p.input.(p.pos)
let tokindex p = TokenIndex.of_int p.pos
let expect_eat_token p t =
  if Token_types.token_eq t (curtok p) then
    Ok (inc p)
  else
    Error (Ast_types.ExpectedFound (t, (tokindex p)))

let add_node p node = { p with ast=Array.append p.ast [|node|] }, NodeIndex.of_int ((Array.length p.ast) + 1)
let add_tl_node p nodeindex = { p with tl_nodes=p.tl_nodes@[nodeindex] }
(* indiv parsing funcs *)
let p_iden p = match curtok p with
  | Iden _ -> Ok (add_node (inc p) (Ast_types.Iden (tokindex p)))
  | _ -> Error (Ast_types.ExpectedFoundStr ("identifier",  (tokindex p)))

let rec p_params p prev =
  let%bind (p, paramname) = p_iden p in
  let%bind p = expect_eat_token p Colon in
  let%bind (p, typename) = p_iden p in
  match curtok p with
  | Comma -> p_params (inc p) (prev@[(paramname, typename)])
  | Rparen -> Ok (p, prev@[(paramname, typename)])
  | _ -> Error (Ast_types.ExpectedFound (Rparen, (tokindex p)))

let p_fnproto p =
  let%bind p = expect_eat_token p Kfunc in
  let%bind (p, name) = p_iden p in
  let%bind p = expect_eat_token p Lparen in
  let%bind (p, params) = p_params p [] in
  let%bind p = expect_eat_token p Rparen in
  let%map (p, ret) = p_iden p in
  p, {Ast_types.name; params; ret}

let p_expr p =
  match curtok p with
  | IntLit _ ->  let (p, node) = add_node p (Ast_types.Number (tokindex p)) in Ok (p, node)
  | Iden _ ->  let (p, node) = add_node p (Ast_types.Iden (tokindex p)) in Ok (p, node)
  | _ -> Error (Ast_types.InvalidStartOfExpr (tokindex p))

let p_block_stmt p =
  let%bind (p, e1) = p_expr p in
  let p = inc p in
  let t = curtok p in
  match t with
  | BoAss -> let%bind (p, e2) = p_expr (inc p) in Ok (add_node p (Ast_types.Change (e1, e2)))
  | BoSet -> let%bind (p, e2) = p_expr (inc p) in
    let n = p.ast.((NodeIndex.to_int e1) - 1) in
    Out_channel.print_endline (Ast_types.show_node_tag n);
    (match n with
     | Iden tindex -> Ok (add_node p (Ast_types.Set (tindex, e2)))
     | _ -> Error (Ast_types.SetWithNonIdentIsInvalid e2))
  | _ -> let%bind p = expect_eat_token p EndOfLine in
    Ok (p, e1)

let rec p_block_stmts p stmts =
  if Token_types.token_eq (curtok p) ExclaimMark then
    Ok (p, stmts)
  else
    let%bind (p, stmt) = p_block_stmt p in
    (p_block_stmts p (stmts@[stmt]))


let p_block p =
  let%bind p = expect_eat_token p Comma in
  let%bind (p, stmts) = p_block_stmts p [] in
  let%map p = expect_eat_token p ExclaimMark in
  (p, stmts)


(* the muscle *)
let rec parse_tl p =
  let open Token_types in
  if token_eq Eof (curtok p) then
    Ok (p.tl_nodes, p.ast)
  else
    match p.input.(p.pos) with
    | Kfunc ->
      let%bind (p, fnproto) = p_fnproto p in
      let%bind (p, body) = p_block p in
      let (p, fn) = add_node p (Function (fnproto, body)) in
      let p = add_tl_node p fn in
      parse_tl p
    | _ -> Error (Ast_types.InvalidInTl (tokindex p))
let parse input = parse_tl {input=Array.of_list input; pos=0; ast=[||]; tl_nodes=[]}
