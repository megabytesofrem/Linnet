(* Core System-F AST *)

type pattern =
  | PVar of string      (* x - matches anything *)
  | PLit of Ast.Expr.t  (* 42, "hello", True *)
  | PCon of string * pattern list (* Just x, Nothing *)
  | PWildcard
  [@@ocamlformat "disable"]

type ty =
  | Int
  | Float
  | String
  | Bool
  | Unit
  | Var of int
  | Forall of ty (* forall a. ... *)
  | Fn of ty * ty
  | TyCons of string * ty list (* Maybe Int, Either String Int *)

type expr =
  | Var of int
  | Lit of Ast.Expr.t
  | App of expr * expr  (* f x y *)
  | Lam of ty * expr    (* \x y -> ... *)
  | TyAbs of expr       (* ∀a. ... *)
  | TyApp of expr * ty  (* e [Int] *)
  | Let of expr * expr  (* let x = expr in body *)
  | Match of expr * (pattern * expr) list
  [@@ocamlformat "disable"]

type program = {
  (* Top level names *)
  globals : (string * expr) list;
  (* Entry point of the program *)
  main : expr option;
}

module Pattern = struct
  type t = pattern
end

module Ty = struct
  type t = ty
end

module Expr = struct
  type t = expr
end

(* Debugging *)

let rec ty_to_string = function
  | Int -> "Int"
  | Float -> "Float"
  | String -> "String"
  | Bool -> "Bool"
  | Unit -> "Unit"
  | Var i -> "'" ^ string_of_int i
  | Forall t -> "∀." ^ ty_to_string t
  | Fn (t1, t2) -> "(" ^ ty_to_string t1 ^ " -> " ^ ty_to_string t2 ^ ")"
  | TyCons (name, args) ->
      name ^ " " ^ String.concat " " (List.map ty_to_string args)

let rec to_string = function
  | Var i -> "v" ^ string_of_int i
  | Lit _ -> "lit" (* You can expand this to match Ast.Expr.t if you want *)
  | App (f, x) -> "(" ^ to_string f ^ " " ^ to_string x ^ ")"
  | Lam (ty, body) -> "λ(" ^ ty_to_string ty ^ ")." ^ to_string body
  | TyAbs e -> "Λ." ^ to_string e
  | TyApp (e, t) -> to_string e ^ "[" ^ ty_to_string t ^ "]"
  | Let (v, b) -> "let " ^ to_string v ^ " in " ^ to_string b
  | Match (e, _) -> "match " ^ to_string e ^ " with ..."
