module NodeIndex : Util.Index = struct
  type t = int [@@deriving show]
  let of_int x = x
  let to_int x = x
end

type bin = (NodeIndex.t * NodeIndex.t) [@@deriving show]
type fnparams = (NodeIndex.t * NodeIndex.t) list [@@deriving show]
type fnproto = { name: NodeIndex.t; params: fnparams; ret: NodeIndex.t}[@@deriving show]
type block = NodeIndex.t list [@@deriving show]
type unop = Deref | Ref [@@deriving show]
type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Gt
  | Lt
  | Equ
  | Lte
  | Gte
  | Ne
  | And
  | Or
[@@deriving show]
type node_tag =
  (* tl stuff *)
  | Function of fnproto * block

  (* stmts *)
  | Set of (Token_types.TokenIndex.t * NodeIndex.t)
  | Change of (NodeIndex.t * NodeIndex.t)

  (* expressions *)
  | BinOp of binop * bin
  | ArrayAccess of bin

  | Unop of unop * NodeIndex.t

  | Number of Token_types.TokenIndex.t
  | Iden of Token_types.TokenIndex.t
  | String of Token_types.TokenIndex.t


[@@deriving show]

type ast_error =
  | InvalidInTl of Token_types.TokenIndex.t
  | InvalidStartOfExpr of Token_types.TokenIndex.t
  | ExpectedFound of Token_types.token * Token_types.TokenIndex.t
  | ExpectedFoundStr of string * Token_types.TokenIndex.t
  | SetWithNonIdentIsInvalid of Token_types.TokenIndex.t
  | InvalidTODOSPECIFIC

