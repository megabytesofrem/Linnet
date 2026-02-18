module T = Lexer.Token

module BinOp = struct
  type t = 
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Eq
    | NotEq
    | Less
    | LessEq
    | Greater
    | GreaterEq
    | Apply   (* application: f x or f $ x *)
    | Compose (* composition: f . g *)

    (* Monadic/applicative/alternative *)
    | Bind  (* >>= *)
    | Pipe  (* >>  *)
    | UFO   (* <*> *)
    | Map   (* <$> *)
    | Alt   (* <|> *)

    [@@ocamlformat "disable"]
end

module Ty = struct
  (* side-effect tracking, later on *)
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

    [@@ocamlformat "disable"]
end

module Expr = struct
  type t =
    | Unit (* () *)
    | Literal of T.t
    | Ident of string
    | Unary of T.t * t
    | Binary of BinOp.t * t * t
    | List of t list (* [e1, e2, e3] *)
    | Tuple of t list (* (e1, e2, e3) *)
    | App of t * t (* f x y *)
    | Lam of string list * t (* \x y -> ... *)
    | Let of string * Ty.t option * t (* optional type annotation *)
    | If of t * t * t option (* condition, then, optional else *)
    | Loop of t option * t (* loop (condition) body *)
    | Block of t list (* { e1; e2; e3 } *)
    | Bind of string * t (* let x <- m *)
end
