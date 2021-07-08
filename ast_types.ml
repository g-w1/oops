module NodeIndex : Util.Index = struct
  type t = int [@@deriving show]
  let of_int x = x
  let to_int x = x
end

type bin = (NodeIndex.t * NodeIndex.t) [@@deriving show]
type fnparams = (NodeIndex.t * NodeIndex.t) list [@@deriving show]
type fnproto = { name: NodeIndex.t; params: fnparams; ret: NodeIndex.t}[@@deriving show]
type body = NodeIndex.t list [@@deriving show]
type node_tag =
  | Set of bin
  | Change of bin
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
  | Ident of Token_types.TokenIndex.t
  | String of Token_types.TokenIndex.t

  | Deref of NodeIndex.t
  | Ref of NodeIndex.t

  | Body of body

  | Function of fnproto * body
[@@deriving show]

type ast_error =
  | InvalidInTl of Token_types.token
  | ExpectedFound of Token_types.token * Token_types.token
  | InvalidTODOSPECIFIC
[@@deriving show]
