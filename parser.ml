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
let print_node p node =
  Out_channel.print_endline (Ast_types.show_node_tag p.ast.(NodeIndex.to_int node - 1))

let print_tok tok =
  Out_channel.print_endline (Token_types.show_token tok)

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

let bin_op_pres p =
  print_tok (curtok p);
  match curtok p with
  | BoMul -> 15
  | BoDiv -> 15
  | BoPlus -> 10
  | BoMinus -> 10
  | BoG -> 5
  | BoL -> 5
  | BoLe -> 5
  | BoGe -> 5
  | BoE -> 5
  | BoNe -> 5
  | BoOr -> 5
  | BoAnd -> 5
  | _ -> -1

let p_primary_expr p =
  match curtok p with
  | IntLit _ ->  let (p, node) = add_node p (Ast_types.Number (tokindex p)) in Ok (p, node)
  | Iden _ ->  let (p, node) = add_node p (Ast_types.Iden (tokindex p)) in Ok (p, node)
  | _ -> Error (Ast_types.InvalidStartOfExpr (tokindex p))

let abop_of_tbop = let open Token_types in let open Ast_types in function
    | BoPlus -> Add
    | BoMinus -> Sub
    | BoMul -> Mul
    | BoDiv -> Div
    | BoG -> Gt
    | BoL -> Lt
    | BoLe -> Lte
    | BoGe -> Gte
    | BoE -> Equ
    | BoNe -> Ne
    | BoAnd -> And
    | BoOr -> Or
    | _ -> assert false (* unreachable *)


let rec p_bin_op_rhs p passed_pres old_lhs =
  let rec loop p pres lhs =
    if pres < passed_pres then
      ((Out_channel.printf "%d\n" pres);
       (print_node p lhs);
       Ok (p, lhs))
    else
      (* must be a binop, because other things have -1 pres *)
      let bin_op = curtok p in
      let p = inc p in
      let%bind (p, rhs) = p_primary_expr p in
      let next_pres = bin_op_pres p in
      if pres < next_pres then
        let%bind (p, rhs) = p_bin_op_rhs p (pres + 1) rhs in
        let (p, lhs) = add_node p (Ast_types.BinOp (abop_of_tbop bin_op, (lhs, rhs))) in
        loop p next_pres lhs
      else
        let (p, lhs) = add_node p (Ast_types.BinOp (abop_of_tbop bin_op, (lhs, rhs))) in
        loop p next_pres lhs
  in
  let p = inc p in
  loop p (bin_op_pres p) old_lhs


let p_expr p =
  let%bind (p, lhs) = p_primary_expr p in
  p_bin_op_rhs p 0 lhs

let p_block_stmt p =
  let%bind (p, e1) = p_expr p in
  Out_channel.print_endline "here";
  match curtok p with
  | BoAss -> let%bind (p, e2) = p_expr (inc p) in Ok (add_node p (Ast_types.Change (e1, e2)))
  | BoSet -> let%bind (p, e2) = p_expr (inc p) in
    let n = p.ast.((NodeIndex.to_int e1) - 1) in
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
let parse input = parse_tl {input; pos=0; ast=[||]; tl_nodes=[]}
