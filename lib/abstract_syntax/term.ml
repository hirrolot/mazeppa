[@@@coverage off]

type t =
  | Var of Symbol.t
  | Const of Const.t
  | Call of Symbol.t * t list
[@@deriving eq, show]

type category =
  | Global
  | Local
  | Trivial
[@@deriving eq, show]

type value_category =
  | VConst
  | VCCall of Symbol.t
  | VNeutral
[@@deriving eq, show]

type redex_sig = (Symbol.t * value_category list) option [@@deriving eq, show]

let var x = Var (Symbol.of_string x)

let int x = Const (Const.Int x)

let string s = Const (Const.String s)

let call (op, args) = Call (Symbol.of_string op, args)

let var_list symbols = List.map (fun x -> Var x) symbols

let of_bool = function
  | true -> Call (Symbol.of_string "T", [])
  | false -> Call (Symbol.of_string "F", [])
;;

let panic fmt =
    Printf.ksprintf (fun s -> Call (Symbol.of_string "Panic", [ string s ])) fmt
;;

let is_var = function
  | Var _ -> true
  | _ -> false
;;

let rec is_neutral = function
  | Var _ -> true
  | Const _ -> false
  | Call (op, args) -> is_neutral_call ~op args

and is_neutral_call ~op = function
  | [ t ] when Symbol.is_op1 op -> is_neutral t
  | [ t1; t2 ] when Symbol.is_op2 op ->
    (is_neutral t1 && is_value t2) || (is_value t1 && is_neutral t2)
  | _args -> Symbol.is_foreign_function op

and is_value = function
  | Var _ | Const _ -> true
  | Call (op, args) ->
    (match Symbol.op_kind op with
     | `CCall -> op <> Symbol.of_string "Panic"
     | `FCall -> is_neutral_call ~op args
     | `GCall -> false)
;;

let rec to_string = function
  | Var x -> Symbol.to_string x
  | Const const -> Const.to_string const
  | Call (op, args) ->
    Printf.sprintf
      "%s(%s)"
      (Symbol.to_string op)
      (String.concat ", " (List.map to_string args))
;;

let verbatim t = "`" ^ to_string t ^ "`"

[@@@coverage on]

let classify : t -> category =
    let rec go = function
      | Var _ | Const _ -> Trivial
      | Call (op, args) -> go_call (Symbol.op_kind op, args)
    and go_call = function
      | `CCall, _args -> Trivial
      | `GCall, Var _ :: _args -> Global
      | (`FCall | `GCall), args -> go_args args
    and go_args = function
      | [] -> Local
      | t :: rest ->
        (match go t with
         | Global -> Global
         | Local | Trivial -> go_args rest)
    in
    go
;;

let redex_sig : t -> redex_sig =
    let rec go = function
      | Var _ | Const _ -> None
      | Call (op, _args) when Symbol.is_lazy_op op -> None
      | Call (op, args) -> go_args ~op ~acc:Fun.id args
    and go_args ~op ~acc = function
      | [] -> Some (op, acc [])
      | t :: rest ->
        let$ category = t in
        go_args ~op ~acc:(fun xs -> acc (category :: xs)) rest
    and ( let$ ) t k =
        match t with
        | Call (c, [ _ ]) when c = Symbol.of_string "Panic" -> None
        | Const _ -> k VConst
        | Call (c, _args) when Symbol.op_kind c = `CCall -> k (VCCall c)
        | t when is_neutral t -> k VNeutral
        | t -> go t
    in
    go
;;

let subst ~x ~value : t -> t =
    let exception Rebuild of t in
    let rebuild t = raise_notrace (Rebuild t) in
    let rec go = function
      | Var y when x = y -> rebuild value
      | Var _ | Const _ -> ()
      | Call (op, args) -> go_args ~op ~acc:Fun.id args
    and go_args ~op ~acc = function
      | [] -> ()
      | t :: rest ->
        (try go t with
         | Rebuild t ->
           (rebuild [@coverage off]) (Call (op, acc (t :: List.map try_go rest))));
        go_args ~op ~acc:(fun xs -> acc (t :: xs)) rest
    and try_go t =
        match go t with
        | exception Rebuild t -> t
        | () -> t
    in
    try_go
;;

let bind ~subst (x, t) = subst := Symbol_map.add x t !subst

let match_against (t1, t2) : t Symbol_map.t option =
    let exception Fail in
    let subst = ref Symbol_map.empty in
    let rec go (t1, t2) =
        match t1 with
        | Var x ->
          (match Symbol_map.find_opt x !subst with
           | Some x_subst -> if not (equal x_subst t2) then raise_notrace Fail
           | None -> bind ~subst (x, t2))
        | Const _ -> if not (equal t1 t2) then raise_notrace Fail
        | Call (op, args) ->
          (match t2 with
           | Call (op', args') when op = op' ->
             List.iter2 (fun t1 t2 -> go (t1, t2)) args args'
           | _ -> raise_notrace Fail)
    in
    try
      go (t1, t2);
      Some !subst
    with
    | Fail -> None
;;

let rename_against (t1, t2) : t Symbol_map.t option =
    let exception Fail in
    let subst = ref Symbol_map.empty in
    let rec go = function
      | Var x, (Var y as t2) ->
        (match Symbol_map.find_opt x !subst with
         | Some (Var y') -> if y <> y' then raise_notrace Fail
         | _ -> bind ~subst (x, t2))
      | Const const, Const const' when Const.equal const const' -> ()
      | Call (op, args), Call (op', args') when op = op' ->
        List.iter2 (fun t1 t2 -> go (t1, t2)) args args'
      | (Var _ | Const _ | Call _), _ -> raise_notrace Fail
    in
    try
      go (t1, t2);
      Some !subst
    with
    | Fail -> None
;;
