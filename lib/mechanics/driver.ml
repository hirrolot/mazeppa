[@@@coverage off]

type 'a step =
  | Var of Symbol.t
  | Const of Const.t
  | Decompose of Symbol.t * 'a list
  | Unfold of 'a
  | Analyze of Symbol.t * 'a * (contraction * 'a case_body) list
  | Extract of (Symbol.t * 'a) * 'a

and contraction =
  { c : Symbol.t
  ; fresh_vars : Symbol.t list
  ; source_vars : Symbol.t list
  }

and 'a case_body = (Symbol.t * 'a) option * 'a

let step_of_term : Term.t -> Term.t step = function
  | Term.Var x -> Var x
  | Term.Const const -> Const const
  | Term.Call (op, args) -> Decompose (op, args)
;;

let map_case_body ~(f : 'a -> 'b) : 'a case_body -> 'b case_body = function
  | Some (x, t), u ->
    let binding = Some (x, f t) in
    binding, f u
  | None, u -> None, f u
;;

let map ~(f : 'a -> 'b) : 'a step -> 'b step = function
  | Var x -> Var x
  | Const _ as step -> step
  | Decompose (op, args) -> Decompose (op, List.map f args)
  | Unfold t -> Unfold (f t)
  | Analyze (x, t, variants) ->
    let t = f t in
    let variants =
        List.map (fun (contraction, body) -> contraction, map_case_body ~f body) variants
    in
    Analyze (x, t, variants)
  | Extract ((x, t), u) ->
    let binding = x, f t in
    Extract (binding, f u)
;;

let unify ~x ~contraction:{ c; fresh_vars; source_vars = _ } list =
    match x with
    | None -> list
    | Some x ->
      let unifier = Term.(Call (c, var_list fresh_vars)) in
      List.map (Term.subst ~x ~value:unifier) list
;;

let symbol = Symbol.of_string

let panic fmt =
    Format.ksprintf (fun msg -> Decompose (symbol "Panic", [ Term.string msg ])) fmt
;;

let view_g_rules ~program g =
    let rules, productive_rules =
        ( Program.(G_rules_by_name.bindings (find_g_rule_list ~program g))
        , Program.find_productive_g_rule_list ~program g )
    in
    List.combine rules productive_rules
;;

(* The "body builder" implements hash consing for function bodies. Its purpose is twofold:
   first, it drastically improves memory usage of the supercompiler by sharing
   structurally equal terms; second, it allows homeomorphic embedding to avoid doing
   duplicate work (by looking into its own caches). We avoid memory leaks by using a weak
   hash set provided by the standard library.

   We have also tried using the "ocaml-hashcons" library from [1], but we did not observe
   any noticeable performance improvements.

   [1] Jean-Christophe Filliâtre and Sylvain Conchon. 2006. Type-safe modular
   hash-consing. In Proceedings of the 2006 workshop on ML (ML '06). Association for
   Computing Machinery, New York, NY, USA, 12–19.
   https://doi.org/10.1145/1159876.1159880 *)
module Make_body_builder (_ : sig end) : sig
  val build : env:Subst.t -> Term.t -> Term.t
end = struct
  module Pool = Weak.Make (struct
      type t = Term.t

      let equal, hash = Term.equal, Hashtbl.hash
    end)

  (* The same size as that of the global homeomorphic embedding cache. *)
  let pool = Pool.create 16384

  let build_call ~op =
      let open Simplifier in
      function
      | `FCall, [ t ] when Symbol.is_op1 op -> handle_op1 ~op t
      | `FCall, [ t1; t2 ] when Symbol.is_op2 op -> handle_op2 ~op (t1, t2)
      | (`CCall | `FCall | `GCall), args -> Call (op, args)
  ;;

  let rec build ~env t = Pool.merge pool (build_term ~env t)

  and build_term ~env = function
    | Term.Var x as default -> Option.value ~default (Symbol_map.find_opt x env)
    | Term.Const _ as t -> t
    | Term.Call (op, args) ->
      build_call ~op (Symbol.op_kind op, List.map (build ~env) args)
  ;;
end

(* [argument_status t] is [`Drive] when [t] can be further reduced, [`Extract] when [t]
   must be extracted into a new let-binding for safety, and [`Pass] when [t] is ready to
   be substituted for a respective parameter. *)
let argument_status =
    let open Term in
    function
    | Var _ | Const _ -> `Pass
    (* We treat variable comparisons as values because they do not panic and facilitate
       further information propagation. *)
    | Call (op, ([ Var _; Var _ ] | [ Var _; Const _ ] | [ Const _; Var _ ]))
      when match Symbol.to_string op with
           | "=" | "!=" | ">" | ">=" | "<" | "<=" -> true
           | _ -> false -> `Pass
    (* Some compound neutral values can panic at run-time; for example, [+(x, 100u8)]
       panics if [x] is instantiated to [200u8]. We therefore extract them into new
       let-bindings to preserve the original order of panics. Another reason for this is
       to avoid potential duplication of computation, because (as usual) arguments are
       duplicated when they are substituted. *)
    | t when Term.is_neutral t -> `Extract
    (* We extract foreign function calls for the same reason. *)
    | Call (op, _args) when Symbol.is_foreign_function op -> `Extract
    (* Only regular constructor calls can be passed as arguments. *)
    | Call (op, _args) ->
      (match Symbol.op_kind op with
       | `CCall when op <> symbol "Panic" -> `Pass
       | `CCall | `FCall | `GCall -> `Drive)
;;

module Make (S : sig
    val program : Program.t

    val inspect : bool

    val gensym : Gensym.t
  end) =
struct
  open S

  module Body_builder = Make_body_builder (struct end)

  let simplify_body ~params ~args t =
      Body_builder.build ~env:(Symbol_map.setup2 (params, args)) t
  ;;

  let maybe_extract_body ~depth ~is_productive body =
      if is_productive || depth = 0 || Term.is_var body
      then None, body
      else (
        let x = Gensym.emit gensym in
        Some (x, body), Term.Var x)
  ;;

  exception Uncontractable

  let analyze_g_rules ~depth ~f g =
      view_g_rules ~program g
      |> List.map (fun ((c, (c_params, params, body)), is_productive) ->
        let fresh_vars = Gensym.emit_list ~length_list:c_params gensym in
        let contraction = { c; fresh_vars; source_vars = c_params } in
        let args =
            try f contraction with
            | Uncontractable ->
              Util.panic
                "Unexpected pattern %s"
                Term.(verbatim (Call (c, var_list c_params)))
        in
        let params, args = c_params @ params, Term.var_list fresh_vars @ args in
        let body =
            body
            |> simplify_body ~params ~args
            |> maybe_extract_body ~depth ~is_productive
        in
        contraction, body)
  ;;

  let unfold_g_rules ~depth ~x ~args g =
      analyze_g_rules ~depth ~f:(fun contraction -> unify ~x ~contraction args) g
  ;;

  let unfold_g_rules_t_f ~depth ~test:(x, op', unifier) ~args g =
      let f { c; fresh_vars; source_vars = _ } =
          match Symbol.(to_string c, to_string op'), fresh_vars with
          | ("T", "="), [] | ("F", "!="), [] ->
            List.map (Term.subst ~x ~value:unifier) args
          (* We do not propagate negative information during driving (yet?). *)
          | (("T" | "F"), _), [] -> args
          | _ -> raise Uncontractable
      in
      analyze_g_rules ~depth ~f g
  ;;

  let rec reduce ~depth : Term.t -> Term.t step = function
    | Term.Var x -> Var x
    | Term.Const const -> Const const
    | Term.Call (op, args) when Symbol.is_lazy_op op ->
      reduce_call ~depth ~op (Symbol.op_kind op, args)
    | Term.Call (op, args) -> reduce_args ~depth (op, args)

  and reduce_args ~depth (op, args) =
      let rec go ~acc = function
        | [] -> reduce_call ~depth ~op (Symbol.op_kind op, acc [])
        | t :: rest ->
          (match argument_status t with
           | `Drive -> reduce_amidst ~depth ~op (acc [], t, rest)
           (* Primitive operations cannot remove/reorder panics in their operands. *)
           | `Extract when not (Symbol.is_primitive_op op) ->
             let x = Gensym.emit gensym in
             Extract ((x, t), Term.(Call (op, acc (Var x :: rest))))
           | `Extract | `Pass -> go ~acc:(fun xs -> acc (t :: xs)) rest)
      in
      go ~acc:Fun.id args

  and reduce_amidst ~depth ~op (before, t, after) =
      match reduce ~depth:(depth + 1) t with
      | Var x -> Unfold Term.(Call (op, before @ [ Var x ] @ after))
      | Const const -> Unfold Term.(Call (op, before @ [ Const const ] @ after))
      | Decompose (c, [ _ ]) as step when c = symbol "Panic" -> step
      | Decompose (c, args) ->
        Unfold Term.(Call (op, before @ [ Call (c, args) ] @ after))
      | Unfold t -> Unfold (Term.Call (op, before @ [ t ] @ after))
      | Analyze (x, t, variants) ->
        variants
        |> List.map (fun (contraction, (binding, u)) ->
          let unify list = unify ~x:(Some x) ~contraction list in
          let u = Term.Call (op, unify before @ [ u ] @ unify after) in
          contraction, (binding, u))
        |> fun variants -> Analyze (x, t, variants)
      | Extract ((x, t), u) -> Extract ((x, t), Term.Call (op, before @ [ u ] @ after))

  and reduce_call ~depth ~op = function
    | `CCall, args -> Decompose (op, args)
    | `FCall, [ t ] when Symbol.is_op1 op -> step_of_term (Simplifier.handle_op1 ~op t)
    | `FCall, [ t1; t2 ] when Symbol.is_op2 op ->
      step_of_term (Simplifier.handle_op2 ~op (t1, t2))
    | `FCall, args when Symbol.is_primitive_op op ->
      let comma_sep_args = args |> List.map Term.verbatim |> String.concat "," in
      panic "unexpected argument list for %s: %s" (Symbol.verbatim op) comma_sep_args
    | `FCall, args when Symbol.is_foreign_function op -> Decompose (op, args)
    | `FCall, args ->
      let params, body = Program.find_f_rule ~program op in
      Unfold (simplify_body ~params ~args body)
    | `GCall, [] -> panic "no scrutinee available"
    | `GCall, (Term.Const _ as scrutinee) :: _args ->
      panic "ill-formed scrutinee %s" (Term.verbatim scrutinee)
    | `GCall, Term.Var x :: args ->
      Analyze (x, Term.Var x, unfold_g_rules ~depth ~x:(Some x) ~args op)
    | `GCall, Term.Call (op', args') :: args ->
      reduce_g_call ~depth ~op (Symbol.op_kind op', (op', args'), args)

  (* Reduces a g-function call where the first argument is also a call. *)
  and reduce_g_call ~depth ~op = function
    | `CCall, (c, c_args), args ->
      (match Program.find_g_rule ~program (op, c) with
       | Some (c_params, params, body) ->
         Unfold (simplify_body ~params:(c_params @ params) ~args:(c_args @ args) body)
       | None -> panic "no such pattern %s" (Symbol.verbatim c))
    | _, (op', Term.(([ Var x; t ] | [ t; Var x ]) as args')), args
      when op' = symbol "=" || op' = symbol "!=" ->
      let scrutinee = Term.Call (op', args') in
      let variants = unfold_g_rules_t_f ~depth ~test:(x, op', t) ~args op in
      Analyze (Gensym.emit gensym, scrutinee, variants)
    | _, (op', args'), args ->
      let scrutinee = Term.Call (op', args') in
      let variants = unfold_g_rules ~depth ~x:None ~args op in
      Analyze (Gensym.emit gensym, scrutinee, variants)
  ;;

  let run ~f t = map ~f (reduce ~depth:0 t)

  let try_run ~f t =
      try run ~f t with
      | Util.Panic { msg; reduction_path } ->
        Util.panic ~reduction_path:(Term.verbatim t :: reduction_path) "%s" msg
  ;;

  (* Notice that this function is not recursive! To enable continuous driving, [f] should
     call [run] inside itself. *)
  let run : f:(Term.t -> 'a) -> Term.t -> 'a step =
      (* Do not install the exception handler if inspection is disabled. *)
      if inspect then try_run else run
  ;;
end
