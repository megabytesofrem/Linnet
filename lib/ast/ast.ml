(* Unprocessed AST *)
(* Human readable AST format that will be lowered to Core *)

module T = Lexer.Token

module UnaryOp = struct
  type t =
    | Negate (* -x *)
    | Not    (* !x *)

    [@@ocamlformat "disable"]
end

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
  type t = 
    | Int | Float | String | Bool | Unit
    | Var of string
    | Forall of string * t      (* forall a. ... *)
    | Fn of t * t 
    | TyCons of string * t list (* Maybe Int, Either String Int *)

    [@@ocamlformat "disable"]
end

module rec Expr : sig
  type t =
    | Unit (* () *)
    | Lit of T.t
    | Ident of string
    | Unary of UnaryOp.t * t
    | Binary of BinOp.t * t * t
    | List of t list  (* [e1, e2, e3] *)
    | Tuple of t list (* (e1, e2, e3) *)
    | App of t * t    (* f x y *)
    | Lam of string list * t (* \x y -> ... *)
    | Let of string * Ty.t option * t (* optional type annotation *)
    | LetIn of string * Ty.t option * t * t (* let x = expr in body *)
    | LetWhere of string * Ty.t option * t * Clause.t list (* let x = expr where ... *)
    | If of t * t * t        (* if cond then expr else expr  *)
    | Loop of t option * t   (* loop (condition) body *)
    | Block of t list        (* { e1; e2; e3 } *)
    | Bind of string * t     (* let x <- m *)

    [@@ocamlformat "disable"]
end =
  Expr

and Decl : sig
  type t =
    (* Top level expression *)
    | Expr of Expr.t
    (* Typeclasses *)
    | Class of {
        class_name : string;
        type_params : string list;
        methods : (string * string list * Ty.t) list;
            (* method name and type signature *)
      }
    | Instance of {
        class_name : string;
        type_args : Ty.t;
        methods : (string * string list * Ty.t * Expr.t) list;
            (* method name and implementation *)
      }
end =
  Decl

and Clause : sig
  type t = Where of string * Ty.t option * Expr.t
end =
  Clause

let binop_to_string bop =
  match bop with
  | BinOp.Add -> "+"
  | BinOp.Sub -> "-"
  | BinOp.Mul -> "*"
  | BinOp.Div -> "/"
  | BinOp.Mod -> "%"
  | BinOp.Eq -> "=="
  | BinOp.NotEq -> "!="
  | BinOp.Less -> "<"
  | BinOp.LessEq -> "<="
  | BinOp.Greater -> ">"
  | BinOp.GreaterEq -> ">="
  | BinOp.Apply -> "$"
  | BinOp.Compose -> "."

  (* Monadic/applicative/alternative *)
  | BinOp.Bind -> ">>="
  | BinOp.Pipe -> ">>"
  | BinOp.UFO -> "<*>"
  | BinOp.Map -> "<$>"
  | BinOp.Alt -> "<|>"
  [@@ocamlformat "disable"]

let unaryop_to_string uop =
  match uop with
  | UnaryOp.Negate -> "-"
  | UnaryOp.Not -> "!"
  [@@ocamlformat "disable"]
