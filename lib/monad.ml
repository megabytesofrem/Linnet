module type Monad = sig
  type 'a m

  (* return : a -> 'a m *)
  val return : 'a -> 'a m

  (* bind : 'a m -> ('a -> 'b m) -> 'b m *)
  val bind : 'a m -> ('a -> 'b m) -> 'b m

  (* map : ('a -> 'b) -> 'a m -> 'b m *)
  val map : ('a -> 'b) -> 'a m -> 'b m

  (* >>= and <$> *)
  val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
  val ( <$> ) : ('a -> 'b) -> 'a m -> 'b m
end

(* State monad *)
module State (S : sig
  type t
end) : sig
  include Monad

  (* State operations *)
  val get : S.t m
  val put : S.t -> unit m
  val modify : (S.t -> S.t) -> unit m
  val run : 'a m -> S.t -> 'a * S.t
  val eval : 'a m -> S.t -> 'a
  val exec : 'a m -> S.t -> S.t

  (* >>= and <$> *)
  val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
  val ( <$> ) : ('a -> 'b) -> 'a m -> 'b m
  val ( let* ) : 'a m -> ('a -> 'b m) -> 'b m
end = struct
  type 'a m = S.t -> 'a * S.t

  let return x = fun s -> (x, s)

  let map f ma =
   fun s ->
    let a, s' = ma s in
    (f a, s')

  let bind ma f =
   fun s ->
    let a, s' = ma s in
    f a s'

  let ( >>= ) = bind
  let ( <$> ) = map
  let ( let* ) = bind
  let get = fun s -> (s, s)
  let put new_s = fun _ -> ((), new_s)
  let modify f = fun s -> ((), f s)
  let run m init = m init
  let eval m init = fst (m init)
  let exec m init = snd (m init)
end
