open Term

(* Since the set of all Mazeppa integers is finite, we _could_ treat each integer as a
   unique constructor; however, that would result in supercompilation never terminating in
   practice. See issues #12, #32, and #35. *)
let decide_ints (x, y) =
    match Checked_oint.pair (x, y) with
    | Some (module Pair) ->
      let x, y = Pair.value in
      Pair.compare x y <= 0
    | None -> false
;;

(* Decides whether strings [s1] and [s2] contain the same characters, possibly in
   different number and/or order. *)
let same_character_set (s1, s2) =
    let present = Array.make 256 0 in
    s1
    |> String.iter (fun c ->
      let i = Char.code c in
      present.(i) <- present.(i) lor 1);
    s2
    |> String.iter (fun c ->
      let i = Char.code c in
      present.(i) <- present.(i) lor 2);
    Array.(for_all (unsafe_get [| true; false; false; true |]) present)
;;

(* This relation is borrowed from [1]. For the proof that this is a valid WQO, see theorem
   3.2 from the same paper.

   [1] Bolingbroke, Maximilian, Simon Peyton Jones, and Dimitrios Vytiniotis. "Termination
   combinators forever." Proceedings of the 4th ACM symposium on Haskell. 2011. *)
let decide_strings (s1, s2) =
    same_character_set (s1, s2) && String.(length s1 <= length s2)
;;

let decide_const =
    let open Const in
    function
    | Int x, Int y -> decide_ints (x, y)
    | String s1, String s2 -> decide_strings (s1, s2)
    | (Int _ | String _), _ -> false
;;

module Make (_ : sig end) = struct
  external address_of_value : 'a -> int = "address_of_value"

  module type Cache = sig
    type key

    type !'a t

    val create : int -> 'a t

    val add : 'a t -> key -> 'a -> unit

    val find : 'a t -> key -> 'a
  end

  module Make_cache
      (Maker : functor (H : Hashtbl.HashedType) -> Cache with type key = H.t)
      (H : Hashtbl.HashedType) : sig
    include Cache with type key = H.t

    val bind : 'a t * key -> (unit -> 'a) -> 'a
  end = struct
    include Maker (H)

    let bind (cache, key) k =
        try find cache key with
        | Not_found ->
          let result = k () in
          add cache key result;
          result
    ;;
  end

  [@@@coverage off]

  module Size_cache =
    Make_cache
      (Ephemeron.K1.Make)
      (struct
        type t = Term.t

        let equal, hash = ( == ), address_of_value
      end)

  module Result_cache =
    Make_cache
      (Hashtbl.Make)
      (struct
        type t = Term.t * Term.t

        let equal (t1, t2) (s1, s2) = t1 == s1 && t2 == s2

        let hash (t1, t2) = Hashtbl.hash (address_of_value t1, address_of_value t2)
      end)

  [@@@coverage on]

  let ( let$ ), ( let& ) = Size_cache.bind, Result_cache.bind

  (* The same size as that of the global term pool used for driving. *)
  let size_table = Size_cache.create 16384

  let rec memoize_size = function
    | Const (Const.String s) -> String.length s
    | Var _ | Const _ -> 1
    | Call (_op, args) as t ->
      let$ () = size_table, t in
      1 + List.fold_left (fun acc t -> acc + memoize_size t) 0 args
  ;;

  let rec decide ~cache = function
    | Var _, Var _ -> true
    | Const const, Const const' -> decide_const (const, const')
    | t1, (Call (_op, args) as t2) ->
      let& () = cache, (t1, t2) in
      let t1_size, t2_size = memoize_size t1, memoize_size t2 in
      if t1_size = t2_size
      then decide_by_coupling ~cache (t1, t2)
      else if t1_size < t2_size
      then decide_by_diving ~cache (t1, args) || decide_by_coupling ~cache (t1, t2)
      else false
    | (Var _ | Const _ | Call _), _ -> false

  and decide_by_diving ~cache (t1, args) =
      List.exists (fun t2 -> decide ~cache (t1, t2)) args

  and decide_by_coupling ~cache = function
    | Call (op, args), Call (op', args') when op = op' ->
      List.for_all2 (fun t1 t2 -> decide ~cache (t1, t2)) args args'
    | _, _ -> false
  ;;

  let decide ((t1, t2) : Term.t * Term.t) : bool =
      decide ~cache:(Result_cache.create 128) (t1, t2)
  ;;
end
