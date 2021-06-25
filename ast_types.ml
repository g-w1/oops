type binop =
  | Add
  | Sub
  | Mul
  | Gt
  | Lt
  | Equ
  | Lte
  | Gte
  | Ne
  | And

type expr =
  | Number of int
  | Ident of string
  | BinOp of { lhs: expr; op: binop; rhs: expr }
  | FuncCall of { func_name: string; args: expr list }
  | Deref of expr
  | Ref of expr
  | ArrayAccess of { arr: expr; acc: expr }


type stmt =
  | SetOrChange of { set: bool; lhs: expr; rhs: expr } (** sema validates if lhs can be assigned to or not*)
  | If of { guard: expr; body: body }
  | Loop of body
  | Return of expr
  | Break
and
  body = stmt list

type ty =
  | U32
  | U64
  | Array of ty

type tlnode =
  | Func of func
  | Extern of { name: string; params: fnparam list}
and func = { name: string; body: body; export: bool; params: fnparam list }
and fnparam = { n: string; t: ty }
