module NodeIndex : Util.Index = struct
  type t = int [@@deriving show]
  let of_int x = x
  let to_int x = x
end

type bin = (NodeIndex.t * NodeIndex.t) [@@deriving show]
type fnparams = (NodeIndex.t * NodeIndex.t) list [@@deriving show]
type fnproto = { name: NodeIndex.t; params: fnparams; ret: NodeIndex.t}[@@deriving show]
type block = NodeIndex.t list [@@deriving show]
type node_tag =
  (* tl stuff *)
  | Function of fnproto * block

  (* stmts *)
  | Set of (Token_types.TokenIndex.t * NodeIndex.t)
  | Change of (NodeIndex.t * NodeIndex.t)

  (* expressions *)
  | Add of bin
  | Sub of bin
  | Mul of bin
  | Gt of bin
  | Lt of bin
  | Equ of bin
  | Lte of bin
  | Gte of bin
  | Ne of bin
  | And of bin
  | ArrayAccess of bin

  | Number of Token_types.TokenIndex.t
  | Iden of Token_types.TokenIndex.t
  | String of Token_types.TokenIndex.t

  | Deref of NodeIndex.t
  | Ref of NodeIndex.t
[@@deriving show]

type ast_error =
  | InvalidInTl of Token_types.TokenIndex.t
  | InvalidStartOfExpr of Token_types.TokenIndex.t
  | ExpectedFound of Token_types.token * Token_types.TokenIndex.t
  | ExpectedFoundStr of string * Token_types.TokenIndex.t
  | SetWithNonIdentIsInvalid of NodeIndex.t
  | InvalidTODOSPECIFIC
[@@deriving show]
