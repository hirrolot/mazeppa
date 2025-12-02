[@@@coverage off]

(* To avoid confusion with variables, integers are named [m] or [n] in this file.
   (Normally, we name both [x], [y], or [z].) *)

open Term
open Checked_oint

let symbol = Symbol.of_string

let to_int (n : Checked_oint.generic) : int =
    let (module Singleton) = Checked_oint.singleton n in
    let repr : string = Singleton.(to_string value) in
    try int_of_string repr with
    | Failure _ ->
      (* This is possible but should not happen normally. *)
      Util.panic "Out of bounds: %s" repr
;;

let do_int_op1 ~op (module S : Checked_oint.Singleton) =
    let of_int n = int (S.to_generic n) in
    let cast ty =
        let (module Target) = Checked_oint.ops ty in
        let x = S.(to_generic value) in
        int Target.(to_generic (of_generic_exn x))
    in
    try
      match Symbol.to_string op with
      | "~" -> S.(bit_not value) |> of_int
      | "string" -> S.(show value) |> string
      | "u8" -> cast Int_ty.u8
      | "u16" -> cast Int_ty.u16
      | "u32" -> cast Int_ty.u32
      | "u64" -> cast Int_ty.u64
      | "u128" -> cast Int_ty.u128
      | "i8" -> cast Int_ty.i8
      | "i16" -> cast Int_ty.i16
      | "i32" -> cast Int_ty.i32
      | "i64" -> cast Int_ty.i64
      | "i128" -> cast Int_ty.i128
      | _ -> panic "unexpected integer unary operator: %s" (Symbol.verbatim op)
    with
    | Checked_oint.Out_of_range ->
      panic
        "out of range: `%s(%s)`"
        (Symbol.to_string op)
        Const.(to_string (Int S.(to_generic value)))
;;

let do_int_op2 ~op (module S : Checked_oint.Pair) =
    let of_int n = int (S.to_generic n) in
    let m, n = S.value in
    try
      match Symbol.to_string op with
      | "+" -> S.add_exn m n |> of_int
      | "-" -> S.sub_exn m n |> of_int
      | "*" -> S.mul_exn m n |> of_int
      | "/" -> S.div_exn m n |> of_int
      | "%" -> S.rem_exn m n |> of_int
      | "|" -> S.bit_or m n |> of_int
      | "&" -> S.bit_and m n |> of_int
      | "^" -> S.bit_xor m n |> of_int
      | "<<" -> S.shift_left_exn m n |> of_int
      | ">>" -> S.shift_right_exn m n |> of_int
      | "=" -> S.equal m n |> of_bool
      | "!=" -> (not (S.equal m n)) |> of_bool
      | ">" -> S.compare m n > 0 |> of_bool
      | ">=" -> S.compare m n >= 0 |> of_bool
      | "<" -> S.compare m n < 0 |> of_bool
      | "<=" -> S.compare m n <= 0 |> of_bool
      | _ -> panic "unexpected integer binary operator: %s" (Symbol.verbatim op)
    with
    | Checked_oint.Out_of_range ->
      panic
        "out of range: `%s(%s, %s)`"
        (Symbol.to_string op)
        Const.(to_string (Int (S.to_generic m)))
        Const.(to_string (Int (S.to_generic n)))
;;

let do_string_op1 ~op s =
    match Symbol.to_string op with
    | "length" ->
      let n = U64.of_int_exn (String.length s) in
      int (U64 n)
    | "string" -> string s
    | _ -> panic "unexpected string unary operator: %s" (Symbol.verbatim op)
;;

let do_string_op2 ~op (s1, s2) =
    match Symbol.to_string op with
    | "=" -> s1 = s2 |> of_bool
    | "!=" -> s1 <> s2 |> of_bool
    | ">" -> s1 > s2 |> of_bool
    | ">=" -> s1 >= s2 |> of_bool
    | "<" -> s1 < s2 |> of_bool
    | "<=" -> s1 <= s2 |> of_bool
    | "++" -> string (s1 ^ s2)
    | _ -> panic "unexpected string binary operator: %s" (Symbol.verbatim op)
;;

(* In the simplification rules below, we restrict neutral operands to variables only. The
   reason is that sometimes it is possible to accidentally remove a panicking condition;
   for example, rewriting [*(+(x, 100u8), 0u8)] to [0u8] would remove an overflow panic if
   [x] is instantiated to [200u8] at run-time. Since Mazeppa must preserve the semantics
   of input programs, the easiest solution is to simply reject compound neutral operands.
   Even if an operand can be an arbitrary neutral value in principle, such as when
   rewriting [+(t, 0u8)] to [t], we nonetheless follow the same approach for consistency
   and to reduce the chance of messing up! *)

let handle_op1 ~(op : Symbol.t) : t -> t =
    let involutory_ops, idempotent_ops =
        ( [ "~" ] |> Symbol.list
        , [ "u8"; "u16"; "u32"; "u64"; "u128" ]
          @ [ "i8"; "i16"; "i32"; "i64"; "i128" ]
          @ [ "string" ]
          |> Symbol.list )
    in
    function
    | Const (Const.Int (U8 value)) when op = symbol "#" ->
      let c = char_of_int (to_int (U8 value)) in
      string (String.make 1 c)
    | Const (Const.Int n) ->
      let (module Singleton) = Checked_oint.singleton n in
      do_int_op1 ~op (module Singleton)
    | Const (Const.String s) -> do_string_op1 ~op s
    (* op(op(x)) -> x *)
    | Call (op', [ (Var _x as t) ]) when List.mem op involutory_ops && op = op' -> t
    (* op(op(x)) -> op(x) *)
    | Call (op', [ Var _x ]) as t when List.mem op idempotent_ops && op = op' -> t
    (* The catch-call rule. *)
    | t -> Call (op, [ t ])
;;

let handle_op2 ~(op : Symbol.t) : t * t -> t = function
  | (Const (Const.Int m) as t1), (Const (Const.Int n) as t2) ->
    (match Checked_oint.pair (m, n) with
     | Some (module Pair) -> do_int_op2 ~op (module Pair)
     | None ->
       panic
         "unexpected argument list for %s: %s, %s"
         (Symbol.verbatim op)
         (Term.verbatim t1)
         (Term.verbatim t2))
  | Const (Const.String s1), Const (Const.String s2) -> do_string_op2 ~op (s1, s2)
  | Const (Const.String s), Const (Const.Int (U64 idx)) when op = symbol "get" ->
    let idx = to_int (U64 idx) in
    (match s.[idx] with
     | c -> int (U8 (U8.of_int_exn (int_of_char c)))
     | exception Invalid_argument _ -> panic "out of bounds: `%du64`" idx)
  (* +(x, 0), +(0, x) -> x *)
  (* |(x, 0), |(0, x) -> x *)
  | ((Var _x as t), Const (Const.Int n) | Const (Const.Int n), (Var _x as t))
    when (op = symbol "+" || op = symbol "|") && is_zero n -> t
  (* -(x, 0) -> x *)
  | (Var _x as t), Const (Const.Int n) when op = symbol "-" && is_zero n -> t
  (* *(x, 1), *(1, x) -> x *)
  | ((Var _x as t), Const (Const.Int n) | Const (Const.Int n), (Var _x as t))
    when op = symbol "*" && is_one n -> t
  (* *(x, 0), *(0, x) -> 0 *)
  (* &(x, 0), &(0, x) -> 0 *)
  | (Var _x, Const (Const.Int n) | Const (Const.Int n), Var _x)
    when (op = symbol "*" || op = symbol "&") && is_zero n -> int n
  (* |(x, all ones), |(all ones, x) -> all ones *)
  | (Var _x, Const (Const.Int n) | Const (Const.Int n), Var _x)
    when op = symbol "|" && is_all_ones n -> int n
  (* &(x, all ones), &(all ones, x) -> x *)
  | ((Var _x as t), Const (Const.Int n) | Const (Const.Int n), (Var _x as t))
    when op = symbol "&" && is_all_ones n -> t
  (* /(x, 1) -> x *)
  | (Var _x as t), Const (Const.Int n) when op = symbol "/" && is_one n -> t
  (* /(0, x) -> 0 *)
  | Const (Const.Int n), Var _x when op = symbol "/" && is_zero n -> int n
  (* %(x, 1) -> 0 *)
  | Var _x, Const (Const.Int n) when op = symbol "%" && is_one n ->
    let (module Singleton) = Checked_oint.singleton n in
    int Singleton.(to_generic zero)
  (* |(x, x), &(x, x) -> x *)
  | (Var x as t), Var y when (op = symbol "|" || op = symbol "&") && x = y -> t
  (* =(x, x), >=(x, x), <=(x, x) -> T() *)
  | Var x, Var y when (op = symbol "=" || op = symbol ">=" || op = symbol "<=") && x = y
    -> Call (symbol "T", [])
  (* !=(x, x), >(x, x), <(x, x) -> F() *)
  | Var x, Var y when (op = symbol "!=" || op = symbol ">" || op = symbol "<") && x = y ->
    Call (symbol "F", [])
  (* The catch-call rule. *)
  | t1, t2 -> Call (op, [ t1; t2 ])
;;
