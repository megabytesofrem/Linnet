module T = Lexer.Token

(* Wrap out parser state in a monad *)
type parse_state = { tokens : Lexer.Token.t list; pos : int }

module Parser = Monad.State (struct
  type t = parse_state
end)

type 'a parser = 'a Parser.m

let create_parse_state tokens = { tokens; pos = 0 }

(* Bring Parser operators into scope *)
let ( let* ) = Parser.( let* )
let return = Parser.return
let get = Parser.get
let put = Parser.put
let modify = Parser.modify

(* Parser primitives *)
let peek : Lexer.Token.t option parser =
  let* st = get in
  return (List.nth_opt st.tokens st.pos)

let get_pos : int parser =
  let* st = get in
  return st.pos

let trace name : unit parser =
  let* st = get in
  let token_desc =
    match List.nth_opt st.tokens st.pos with
    | Some tok -> Lexer.string_of_token tok
    | None -> "EOF"
  in
  (* eprintf bypasses standard buffering so you see it INSTANTLY *)
  let () =
    Printf.eprintf "[TRACE] %s | Pos: %d | Token: %s\n%!" name st.pos token_desc
  in
  return ()

let fail msg : 'a parser = failwith msg

let advance : Lexer.Token.t option parser =
  let* st = get in
  match List.nth_opt st.tokens st.pos with
  | Some token ->
      let* () = put { st with pos = st.pos + 1 } in
      return (Some token)
  | None -> return None

let expect (expected : Lexer.Token.t) : Lexer.Token.t parser =
  let* token_opt = peek in
  match token_opt with
  | Some token when token = expected ->
      let* _ = advance in
      return token
  | Some token ->
      let* pos = get_pos in
      fail
        (Printf.sprintf "%d: Expected token %s but found %s" pos
           (Lexer.string_of_token expected)
           (Lexer.string_of_token token))
  | None ->
      let* pos = get_pos in
      fail
        (Printf.sprintf "%d: Expected token %s but found end of input" pos
           (Lexer.string_of_token expected))

let expect_ident : string parser =
  let* token_opt = peek in
  match token_opt with
  | Some (T.Ident id) ->
      let* _ = advance in
      return id
  | Some token ->
      let* pos = get_pos in
      fail
        (Printf.sprintf "%d: Expected identifier but found %s" pos
           (Lexer.string_of_token token))
  | None ->
      let* pos = get_pos in
      fail (Printf.sprintf "%d: Expected identifier but found end of input" pos)

(* Parsing *)

(* Pratt parser: precedence climber *)
(*
    The binding power of a token determines which direction it binds.
    In the case of 1 + 2, the '+' operator binds more strongly to the right.
*)

(* Prefix binding power: negation or not *)
(* Default binding power for prefix operators is 10 *)
let prefix_bp = 10

(* Function application has highest precedence *)
let app_bp = 20
let compose_bp = 18
let dollar_bp = 10

(* (left_bp, right_bp) *)
let infix_bp = function
  | T.Plus | T.Minus -> (1, 2)
  | T.Star | T.Slash | T.Percent -> (3, 4)

  (* Non-associative *)
  | T.Eq | T.NotEq | T.Less | T.LessEq | T.Greater | T.GreaterEq -> (5, 5)

  (* Monadic operators, right assoc, lower precedence *)
  | T.Alt -> (6, 5)   (* <|> - alternative, lowest *)
  | T.Bind -> (8, 7)  (* >>= - monadic bind *)
  | T.Pipe -> (7, 6)  (* >>  - monadic sequencing *)
  | T.UFO -> (9, 8)   (* <*> - applicative functor *)
  | T.Map -> (9, 8)   (* <$> - functor map *)

  (* function application: f $ x y *)
  | T.Dollar -> (dollar_bp, dollar_bp - 1) (* 10, 9 - right assoc *)

  (* function composition: f . g *)
  | T.Dot -> (compose_bp, compose_bp - 1) (* 18, 17 - right assoc *)

  | _ -> (0, 0)
  [@@ocamlformat "disable"]

let token_to_binop = function
  | T.Plus -> Some Ast.BinOp.Add
  | T.Minus -> Some Ast.BinOp.Sub
  | T.Star -> Some Ast.BinOp.Mul
  | T.Slash -> Some Ast.BinOp.Div
  | T.Percent -> Some Ast.BinOp.Mod
  | T.Eq -> Some Ast.BinOp.Eq
  | T.NotEq -> Some Ast.BinOp.NotEq
  | T.Less -> Some Ast.BinOp.Less
  | T.LessEq -> Some Ast.BinOp.LessEq
  | T.Greater -> Some Ast.BinOp.Greater
  | T.GreaterEq -> Some Ast.BinOp.GreaterEq
  | T.Dollar -> Some Ast.BinOp.Apply
  | T.Dot -> Some Ast.BinOp.Compose

  (* Monadic/applicative/alternative *)
  | T.Bind -> Some Ast.BinOp.Bind
  | T.Pipe -> Some Ast.BinOp.Pipe
  | T.UFO -> Some Ast.BinOp.UFO
  | T.Map -> Some Ast.BinOp.Map
  | T.Alt -> Some Ast.BinOp.Alt
  | _ -> None
  [@@ocamlformat "disable"]

let can_start_arg = function
  | T.Number _
  | T.String _
  | T.Ident _
  | T.LParen ->
      true
  | _ -> false

(* Expression parsing *)
(* ---------------------------------- *)

(* Parse a single atomic expression *)
let rec parse_atom () : Ast.Expr.t parser =
  let* _ = trace "parse_atom" in
  let parse_group_like () : Ast.Expr.t parser =
    (* lookahead to check for unit (), tuple, or grouped expr *)
    let* _ = advance in
    let* token_opt = peek in
    match token_opt with
    | Some T.RParen ->
        (* unit: () *)
        let* _ = advance in
        return Ast.Expr.Unit
    | Some _ -> (
        (* parse first element *)
        let* first = parse_expr () in
        let* token_opt = peek in
        match token_opt with
        | Some T.RParen ->
            (* grouped expression: (e) *)
            let* _ = advance in
            return first
        | Some T.Comma ->
            (* tuple: (e1, e2, ...) *)
            let rec parse_rest acc =
              let* _ = advance in
              (* consume comma *)
              let* next = parse_expr () in
              let* t_opt = peek in
              match t_opt with
              | Some T.Comma -> parse_rest (next :: acc)
              | Some T.RParen ->
                  let* _ = advance in
                  return (Ast.Expr.Tuple (List.rev (next :: acc)))
              | Some t ->
                  fail
                    (Printf.sprintf "Expected ',' or ')' but found %s"
                       (Lexer.string_of_token t))
              | None -> fail "Expected ',' or ')' but found EOF"
            in
            parse_rest [ first ]
        | Some token ->
            let* pos = get_pos in
            fail
              (Printf.sprintf "%d: Expected ',' or ')' but found %s" pos
                 (Lexer.string_of_token token))
        | None ->
            let* pos = get_pos in
            fail (Printf.sprintf "%d: Expected ')' but found end of input" pos))
    | None ->
        let* pos = get_pos in
        fail
          (Printf.sprintf
             "%d: Expected expression or ')' but found end of input" pos)
  in

  let* token_opt = peek in
  match token_opt with
  | Some (T.Number _ as token) ->
      let* _ = advance in
      return (Ast.Expr.Literal token)
  | Some (T.String _ as token) ->
      let* _ = advance in
      return (Ast.Expr.Literal token)
  | Some (T.Ident id) ->
      let* _ = advance in
      return (Ast.Expr.Ident id)
  | Some T.LParen -> parse_group_like ()
  | Some T.LBrack -> parse_list ()
  | Some T.Backslash -> parse_lambda ()
  | Some T.Let -> parse_let ()
  | Some T.If -> parse_if ()
  | Some T.Loop -> parse_loop ()
  (* *)
  (* Handle prefix operators *)
  | Some token ->
      let* pos = get_pos in
      fail
        (Printf.sprintf "%d: Expected expression but found %s" pos
           (Lexer.string_of_token token))
  | None ->
      let* pos = get_pos in
      fail (Printf.sprintf "%d: Expected expression but found end of input" pos)

and parse_expr_bp min_bp : Ast.Expr.t parser =
  let* _ = trace "parse_expr_bp" in

  let* lhs = parse_atom () in
  let rec loop lhs =
    let* _ = trace "parse_expr_bp loop" in
    let* token_opt = peek in
    match token_opt with
    | Some op -> (
        if can_start_arg op && app_bp >= min_bp then
          (* function application *)
          let* pre_pos = get_pos in
          let* arg = parse_expr_bp (app_bp + 1) in
          let* post_pos = get_pos in

          if post_pos = pre_pos then
            (* no progress, avoid infinite loop *)
            return lhs
          else loop (Ast.Expr.App (lhs, arg))
        else
          let l_bp, r_bp = infix_bp op in
          if l_bp < min_bp then return lhs
          else
            (* otherwise, consume and recursively parse rhs *)
            match token_to_binop op with
            | Some binop ->
                let* _ = advance in
                let* rhs = parse_expr_bp r_bp in
                loop (Ast.Expr.Binary (binop, lhs, rhs))
            | None -> return lhs)
    | None -> return lhs
  in
  loop lhs

and parse_expr () : Ast.Expr.t parser = parse_expr_bp 0

and parse_enclosed o s c f ctor : Ast.Expr.t parser =
  let* _ = expect o in
  let rec parse_elems acc =
    let* token_opt = peek in
    match token_opt with
    | Some t when t = c ->
        let* _ = advance in
        return (ctor (List.rev acc))
    | Some _ -> (
        let* elem = f in
        let* token_opt = peek in
        match token_opt with
        | Some t when t = s ->
            let* _ = advance in
            parse_elems (elem :: acc)
        | Some token when token = c ->
            let* _ = advance in
            return (ctor (List.rev (elem :: acc)))
        | Some token ->
            let* pos = get_pos in
            fail
              (Printf.sprintf "%d: Expected '%s' or '%s' but found %s" pos
                 (Lexer.string_of_token s) (Lexer.string_of_token c)
                 (Lexer.string_of_token token))
        | None ->
            let* pos = get_pos in
            fail
              (Printf.sprintf "%d: Expected '%s' or '%s' but found end of input"
                 pos (Lexer.string_of_token s) (Lexer.string_of_token c)))
    | None ->
        let* pos = get_pos in
        fail
          (Printf.sprintf
             "%d: Expected '%s' or expression but found end of input" pos
             (Lexer.string_of_token c))
  in
  parse_elems []

and parse_enclosed_generic (type a) o s c (f : unit -> a parser) : a list parser
    =
  let* _ = expect o in
  let rec parse_elems acc =
    let* token_opt = peek in
    match token_opt with
    | Some t when t = c ->
        let* _ = advance in
        return (List.rev acc)
    | Some _ -> (
        let* elem = f () in
        let* token_opt = peek in
        match token_opt with
        | Some t when t = s ->
            let* _ = advance in
            parse_elems (elem :: acc)
        | Some token when token = c ->
            let* _ = advance in
            return (List.rev (elem :: acc))
        | Some token ->
            let* pos = get_pos in
            fail
              (Printf.sprintf "%d: Expected '%s' or '%s' but found %s" pos
                 (Lexer.string_of_token s) (Lexer.string_of_token c)
                 (Lexer.string_of_token token))
        | None ->
            let* pos = get_pos in
            fail
              (Printf.sprintf "%d: Expected '%s' or '%s' but found end of input"
                 pos (Lexer.string_of_token s) (Lexer.string_of_token c)))
    | None ->
        let* pos = get_pos in
        fail
          (Printf.sprintf "%d: Expected '%s' or element but found end of input"
             pos (Lexer.string_of_token c))
  in
  parse_elems []

and parse_list () : Ast.Expr.t parser =
  (* [e1, e2, e3] *)
  parse_enclosed T.LBrack T.Comma T.RBrack (parse_expr ()) (fun elems ->
      Ast.Expr.List elems)

and parse_tuple () : Ast.Expr.t parser =
  (* (e1, e2, e3) *)
  parse_enclosed T.LParen T.Comma T.RParen (parse_expr ()) (fun elems ->
      match elems with
      | [ single ] -> single
      | _ -> Ast.Expr.Tuple elems)

and parse_arg_pair s =
  let* name = expect_ident in
  let* _ = expect s in
  let* value = parse_expr () in
  return (name, value)

(* ---------------------------------- *)
(* Type parsing *)
(* ---------------------------------- *)

and parse_fn_types (first_arg : Ast.Ty.t) =
  (* We've already consumed the '->', so parse the next full type *)
  let* next_ty = parse_type () in
  return (Ast.Ty.Fn (first_arg, next_ty))

and parse_type () : Ast.Ty.t parser =
  let* base_ty = parse_type_atom () in

  (* try to parse type application first (greedily) *)
  let* ty_after_app = parse_type_app base_ty in

  (* check for function type -> *)
  let* token_opt = peek in
  match token_opt with
  | Some T.Arrow ->
      let* _ = advance in
      parse_fn_types ty_after_app
  | _ -> return ty_after_app

and parse_type_params () : string list parser =
  (* [a, b, c] *)
  let* token_opt = peek in
  match token_opt with
  | Some T.LBrack ->
      parse_enclosed_generic T.LBrack T.Comma T.RBrack (fun () ->
          let* param = expect_ident in
          return param)
  | _ -> return [] (* no type parameters *)

and parse_type_atom () : Ast.Ty.t parser =
  let* token_opt = peek in
  match token_opt with
  | Some T.Unit ->
      let* _ = advance in
      return Ast.Ty.Unit
  | Some (T.Ident name) -> (
      let* _ = advance in
      (* Check if it's a built-in type *)
      match name with
      | "Int" -> return Ast.Ty.Int
      | "Float" -> return Ast.Ty.Float
      | "String" -> return Ast.Ty.String
      | "Bool" -> return Ast.Ty.Bool
      | _ ->
          (* Check for type constructor (capitalized) vs type variable (lowercase) *)
          if String.length name > 0 && Char.uppercase_ascii name.[0] = name.[0]
          then return (Ast.Ty.TyCons (name, []))
          else return (Ast.Ty.Var name))
  | Some T.LParen ->
      (* parenthesized type *)
      let* _ = advance in
      let* ty = parse_type () in
      let* _ = expect T.RParen in
      return ty
  | Some token ->
      let* pos = get_pos in
      fail
        (Printf.sprintf "%d: Expected type but found %s" pos
           (Lexer.string_of_token token))
  | None ->
      let* pos = get_pos in
      fail (Printf.sprintf "%d: Expected type but found end of input" pos)

and parse_type_app (base_ty : Ast.Ty.t) : Ast.Ty.t parser =
  let* _ = trace "parse_type_app" in

  let rec loop args =
    let* _ = trace "parse_type_app loop" in
    let* token_opt = peek in
    match token_opt with
    (* valid type atoms that can follow a constructor *)
    | Some (T.Ident _)
    | Some T.Unit
    | Some T.LParen ->
        let* arg = parse_type_atom () in
        loop (arg :: args)
    | _ -> return (List.rev args)
  in
  let* args = loop [] in
  match args with
  | [] -> return base_ty
  | _ -> (
      (* extract the base identifier *)
      match base_ty with
      | Ast.Ty.TyCons (n, []) -> return (Ast.Ty.TyCons (n, args))
      | Ast.Ty.Int -> return (Ast.Ty.TyCons ("Int", args))
      | Ast.Ty.String -> return (Ast.Ty.TyCons ("String", args))
      | Ast.Ty.Bool -> return (Ast.Ty.TyCons ("Bool", args))
      | Ast.Ty.Unit -> return (Ast.Ty.TyCons ("Unit", args))
      | _ ->
          let* pos = get_pos in
          fail
            (Printf.sprintf
               "%d: Expected type constructor but found complex type" pos))

(* ---------------------------------- *)

and parse_let () : Ast.Expr.t parser =
  let* _ = advance in
  let* name = expect_ident in
  let* ty_opt =
    let* token_opt = peek in
    match token_opt with
    | Some T.Colon ->
        let* _ = advance in
        let* ty = parse_type () in
        return (Some ty)
    | _ -> return None
  in
  let* _ = expect T.Eq in
  let* expr = parse_expr () in
  return (Ast.Expr.Let (name, ty_opt, expr))

and parse_if () : Ast.Expr.t parser =
  (* if cond then <then_branch> else <else_branch> *)
  let* _ = advance in
  let* cond = parse_expr () in
  let* _ = expect T.Then in
  let* then_branch = parse_expr () in
  let* _ = expect T.Else in
  let* else_branch = parse_expr () in
  return (Ast.Expr.If (cond, then_branch, else_branch))

and parse_lambda () : Ast.Expr.t parser =
  (* \x y -> body *)
  let* _ = advance in

  (* recursive function to parse parameters *)
  let rec parse_params acc =
    let* token_opt = peek in
    match token_opt with
    | Some (T.Ident param) ->
        let* _ = advance in
        parse_params (param :: acc)
    | Some T.Arrow ->
        (* consume -> *)
        let* _ = advance in
        return (List.rev acc)
    | Some token ->
        let* pos = get_pos in
        fail
          (Printf.sprintf "%d: Expected parameter name or '->' but found %s" pos
             (Lexer.string_of_token token))
    | None ->
        let* pos = get_pos in
        fail
          (Printf.sprintf
             "%d: Expected parameter name or '->' but found end of input" pos)
  in
  let* params = parse_params [] in
  let* body = parse_expr () in
  return (Ast.Expr.Lam (params, body))

and parse_loop () : Ast.Expr.t parser =
  (* loop (cond) body *)
  let* _ = advance in
  let* cond_opt =
    let* token_opt = peek in
    match token_opt with
    | Some T.LParen ->
        let* _ = advance in
        let* cond = parse_expr () in
        let* _ = expect T.RParen in
        return (Some cond)
    | _ -> return None
  in
  let* body = parse_expr () in
  return (Ast.Expr.Loop (cond_opt, body))

and parse_bind () : Ast.Expr.t parser =
  (* x <- m *)
  let* name = expect_ident in
  let* _ = expect T.Bind in
  let* expr = parse_expr () in
  return (Ast.Expr.Bind (name, expr))

(* Typeclasses *)
(* ---------------------------------- *)

and parse_typeclass_decl () : Ast.Decl.t parser =
  (* 
    class Functor[f] {
       fn fmap[a,b] : (a -> b) -> f a -> f b
    }
  *)
  let parse_class_rule () =
    let* _ = expect T.Fn in
    let* name = expect_ident in
    let* type_params = parse_type_params () in
    let* _ = expect T.Colon in
    let* ty = parse_type () in
    return (name, type_params, ty)
  in

  (* parse class rules *)
  let rec parse_rules acc =
    let* token_opt = peek in
    match token_opt with
    | Some T.RCurly ->
        let* _ = advance in
        return (List.rev acc)
    | Some T.Fn ->
        let* rule = parse_class_rule () in
        parse_rules (rule :: acc)
    | Some token ->
        let* pos = get_pos in
        fail
          (Printf.sprintf "%d: Expected 'fn' or '}' but found %s" pos
             (Lexer.string_of_token token))
    | None ->
        let* pos = get_pos in
        fail
          (Printf.sprintf "%d: Expected 'fn' or '}' but found end of input" pos)
  in

  let* _ = expect T.Class in
  let* class_name = expect_ident in
  let* type_params = parse_type_params () in
  let* _ = expect T.LCurly in
  let* members = parse_rules [] in

  return (Ast.Decl.Class { class_name; type_params; methods = members })

(* ---------------------------------- *)

(* Parse a single top-level declaration *)
let parse_decl () : Ast.Decl.t parser =
  let* token_opt = peek in
  match token_opt with
  | Some T.Class -> parse_typeclass_decl ()
  | Some _ ->
      (* Parse top level expression - don't check for remaining tokens here *)
      let* expr = parse_expr () in
      return (Ast.Decl.Expr expr)
  | None ->
      let* pos = get_pos in
      fail
        (Printf.sprintf "%d: Expected declaration but found end of input" pos)

(* Parse the whole program *)
let parse_program () : Ast.Decl.t list parser =
  let* _ = trace "parse_program" in
  let rec loop acc =
    let* _ = trace "parse_program loop" in
    let* token_opt = peek in
    match token_opt with
    | None -> return (List.rev acc)
    | Some _ ->
        let* decl = parse_decl () in
        loop (decl :: acc)
  in
  loop []

(* Run the parser *)
let parse parser tokens =
  try
    let result, _ = Parser.run parser (create_parse_state tokens) in
    Ok result
  with
  | Failure msg -> Error msg
  | exn -> Error (Printexc.to_string exn)
