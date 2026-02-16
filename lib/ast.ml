module T = Linnet.Lexer.Token

type binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Equal
  | NotEqual
  | Less
  | LessEqual
  | Greater
  | GreaterEqual

module Ty = struct
  (* side-effect tracking *)
  type effect_ty = IO | FS | Network | Random | UserEff of string

  type t = 
    | Int
    | Float
    | String
    | Bool

    (* complex types *)
    | TyUser of string
    | TyCons of string * t list   (* parameterized type constructor *)
    | Fn of t list * t            (* function type *)
    | Eff of t * effect_ty list
end

module Expr = struct
  type t = 
    | Literal of T.t
    | Ident of string
    | Unary of T.t * t
    | Binary of binary_op * t * t
    | Let of string * Ty.t option * t (* optional type annotation *)
    | If of t * t * t option          (* condition, then, optional else *)
    | Loop of t option * t            (* loop *)
    | MBlock of t list                (* monadic block: { .. } *)
    | MBind of string * t             (* monadic bind: x <- m  *)
end