open Ast

(* Core AST -- System F *)
(* https://en.wikipedia.org/wiki/System_F *)

module C = Core

type context = {
  term_env : string list; (* term variable context *)
  type_env : string list; (* type variable context *)
}

let empty_ctx = { term_env = []; type_env = [] }

let lookup_term name (ctx : context) : int =
  match List.find_index (fun n -> n = name) ctx.term_env with
  | Some idx -> idx
  | None -> failwith ("Unbound variable: " ^ name)

let lookup_type name (ctx : context) : int =
  match List.find_index (fun n -> n = name) ctx.type_env with
  | Some idx -> idx
  | None -> failwith ("Unbound type variable: " ^ name)

(* Desugar human readable AST to Core System-F AST *)
let rec desugar_ty (ctx : context) (t : Ty.t) : C.ty =
  match t with
  | Ty.Int -> C.Int
  | Ty.Float -> C.Float
  | Ty.String -> C.String
  | Ty.Bool -> C.Bool
  | Ty.Unit -> C.Unit
  | Ty.Var v -> C.Var (lookup_type v ctx)
  | Ty.Forall (v, t) ->
      let new_ctx = { ctx with type_env = v :: ctx.type_env } in
      C.Forall (desugar_ty new_ctx t)
  | Ty.Fn (t1, t2) -> C.Fn (desugar_ty ctx t1, desugar_ty ctx t2)
  | Ty.TyCons (name, args) ->
      let desugared_args = List.map (desugar_ty ctx) args in
      C.TyCons (name, desugared_args)

let rec desugar_expr (ctx : context) (e : Expr.t) : C.expr =
  match e with
  | Expr.Unit -> C.Lit Expr.Unit
  | Expr.Lit lit -> C.Lit (Expr.Lit lit)
  | Expr.Ident name -> C.Var (lookup_term name ctx)
  | Expr.Binary (op, l, r) ->
      let op_name = Ast.binop_to_string op in

      (* Lookup the De-Bruijn index for the operator *)
      let op_idx = lookup_term op_name ctx in
      C.App (C.App (C.Var op_idx, desugar_expr ctx l), desugar_expr ctx r)
  | Expr.Unary (op, e) ->
      let op_name = Ast.unaryop_to_string op in

      (* Lookup the De-Bruijn index for the operator *)
      let op_idx = lookup_term op_name ctx in
      C.App (C.Var op_idx, desugar_expr ctx e)
  | Expr.List xs -> desugar_list ctx xs
  | Expr.Tuple ts -> desugar_tuple ctx ts
  
  (* Function application *)
  | Expr.App (f, x) -> C.App (desugar_expr ctx f, desugar_expr ctx x)

  (* Catamorphism for lambda expressions, include arguments *)
  | Expr.Lam (args, body) ->
      (* Include the lambda arguments in the context *)
      let body_ctx = { ctx with term_env = args @ ctx.term_env } in
      let body_core = desugar_expr body_ctx body in
      List.fold_right (fun _ acc -> C.Lam (C.Unit, acc)) args body_core
  | Expr.LetIn (id, _ty_opt, expr, body) ->
      let val_core = desugar_expr ctx expr in
      let body_ctx = { ctx with term_env = id :: ctx.term_env } in
      C.Let (val_core, desugar_expr body_ctx body)
  | Expr.LetWhere (id, _ty_opt, expr, clauses) ->
      (* 1. Extract the names of the where-clauses *)
      let clause_names =
        List.map
          (function
            | Clause.Where (cid, _, _) -> cid)
          clauses
      in

      (* 2. REVERSE them because fold_right puts the last clause on the inside! *)
      let reversed_names = List.rev clause_names in
      let body_ctx = { ctx with term_env = reversed_names @ ctx.term_env } in

      (* 3. Desugar the main expression using the NEW context so it can see the clauses *)
      let expr_core = desugar_expr body_ctx expr in

      (* 4. Wrap each where clause into a let binding *)
      let wrapped_chain =
        List.fold_right
          (fun (Clause.Where (cid, _, cexp)) acc_body ->
            let cexp_core = desugar_expr ctx cexp in
            C.Let (cexp_core, acc_body))
          clauses expr_core
      in
      wrapped_chain
  | Expr.If (cond, then_b, else_b) ->
      let cond_core = desugar_expr ctx cond in
      let then_core = desugar_expr ctx then_b in
      let else_core = desugar_expr ctx else_b in

      (* Desugar to match expression on the condition *)
      C.Match
        ( cond_core,
          [
            (C.PCon ("True", []), then_core); (C.PCon ("False", []), else_core);
          ] )
  | Expr.Loop (cond_opt, body) ->
      (* Nameless binder to introduce new state *)
      let loop_ctx = { ctx with term_env = "" :: ctx.term_env } in
      let body_core = desugar_expr loop_ctx body in

      (* Desugar to match expression on the condition *)
      let recur_call = C.App (C.Var 0, C.Lit Expr.Unit) in

      let loop_expr = 
        match cond_opt with
        | None ->
          (* Recurse *)
          C.Let (body_core, recur_call)
        | Some cond ->
          let cond_core = desugar_expr loop_ctx cond in 
          C.Match 
            ( cond_core, 
              [
                (C.PCon ("True", []), C.Let (body_core, recur_call));
                (C.PCon ("False", []), C.Lit Expr.Unit);
              ] )
        in
          (* Recursion using fixpoint combinator *)
          let fix_idx = lookup_term "fix" ctx in
          C.App (C.Var fix_idx, C.Lam (C.Unit, loop_expr))

| Expr.Block exprs ->
    let rec expand curr_ctx = function
      | [] -> C.Lit Expr.Unit
      | [last] -> 
          (match last with
           | Expr.Bind _ -> failwith "Syntax Error: A block cannot end with a bind"
           | e -> desugar_expr curr_ctx e)
      | e :: rest ->
          let bind_idx = lookup_term "bind" curr_ctx in
          match e with
          | Expr.Bind (id, action) ->
              let action_core = desugar_expr curr_ctx action in
              let next_ctx = { curr_ctx with term_env = id :: curr_ctx.term_env } in
              C.App (C.App (C.Var bind_idx, action_core), 
                    C.Lam (C.Unit, expand next_ctx rest))
          | plain_expr ->
              let expr_core = desugar_expr curr_ctx plain_expr in
              let next_ctx = { curr_ctx with term_env = "_" :: curr_ctx.term_env } in
              C.App (C.App (C.Var bind_idx, expr_core), 
                    C.Lam (C.Unit, expand next_ctx rest))
    in
    expand ctx exprs

  | Expr.Bind _ -> failwith "Syntax Error: bind must be used inside a { } block"
    
  | _ -> failwith "Syntax Error: unsupported expression"
  [@@ocamlformat "disable"]

and desugar_program (initial_ctx : context) (decls : Decl.t list) : C.expr =
  let rec stitch ctx = function
    | [] -> C.Lit Expr.Unit
    (* If it's the very last thing in the file, just desugar it *)
    | [ Decl.Expr e ] -> desugar_expr ctx e
    | Decl.Expr e :: rest -> (
        match e with
        | Expr.LetIn (id, _, val_expr, _) ->
            let val_core = desugar_expr ctx val_expr in
            let next_ctx = { ctx with term_env = id :: ctx.term_env } in
            C.Let (val_core, stitch next_ctx rest)
        (* Top level expression, evaluate it, discard the result, and move on *)
        | other_e ->
            let e_core = desugar_expr ctx other_e in
            let next_ctx = { ctx with term_env = "_" :: ctx.term_env } in
            C.Let (e_core, stitch next_ctx rest))
    | _ :: rest -> stitch ctx rest (* Skip Class/Instance for now *)
  in
  stitch initial_ctx decls

and desugar_list (ctx : context) (xs : Expr.t list) : C.expr =
  let rec expand = function
    | [] -> C.App (C.Var (lookup_term "Nil" ctx), C.Lit Expr.Unit)
    | x :: xs ->
        let head = desugar_expr ctx x in
        let tail = expand xs in

        (* Apply cons to head, then recursively to the rest of the list *)
        C.App (C.App (C.Var (lookup_term "Cons" ctx), head), tail)
  in
  expand xs

and desugar_tuple (ctx : context) (ts : Expr.t list) : C.expr =
  (* same logic for desugaring tuples as for lists *)
  desugar_list ctx ts
