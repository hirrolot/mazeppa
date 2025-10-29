[@@@coverage off]

open Raw_program

let map_of_hashtbl table = Hashtbl.fold Symbol_map.add table Symbol_map.empty

let set_of_hashtbl table =
    Hashtbl.fold (fun g () acc -> Symbol_set.add g acc) table Symbol_set.empty
;;

(* Launches the productivity analysis of all program rules. A function is productive iff
   all exit points produce a constant or constructor. *)
let compute_productive_g_rules program =
    (* The temporary cache makes our algorithm linear in the program size. *)
    let temp_cache, g_rules_cache, unproductive_g_rules_cache =
        Hashtbl.(create 64, create 64, create 64)
    in
    let ( let$ ) op k =
        match Hashtbl.find_opt temp_cache op with
        | Some result -> result
        | None ->
          Hashtbl.add temp_cache op false;
          let result = k () in
          Hashtbl.replace temp_cache op result;
          result
    in
    let rec go_term = function
      | Term.Var _ -> false
      | Term.Const _ -> true
      | Term.Call (op, _args) -> go_call ~op (Symbol.op_kind op)
    and go_call ~op = function
      | `CCall -> op <> Symbol.of_string "Panic"
      | `FCall when Symbol.is_primitive_op op || Symbol.is_foreign_function op -> false
      | `FCall ->
        let$ () = op in
        let _params, body = Program.find_f_rule ~program op in
        go_term body
      | `GCall ->
        let$ () = op in
        let for_all, for_any = ref true, ref false in
        let descriptions =
            Program.(G_rules_by_name.bindings (find_g_rule_list ~program op))
            |> List.map (fun (_c, (_c_params, _params, body)) ->
              let result = go_term body in
              for_all := !for_all && result;
              for_any := !for_any || result;
              result)
        in
        Hashtbl.add g_rules_cache op descriptions;
        if not !for_any
        then (* All rules are unproductive. *)
          Hashtbl.add unproductive_g_rules_cache op ();
        !for_all
    in
    program.g_rules
    |> Program.G_rules_by_name.iter (fun g _rules ->
      (* Arguments are ignored. *)
      ignore (go_term (Term.Call (g, []))));
    { program with
      productive_g_rules = map_of_hashtbl g_rules_cache
    ; unproductive_g_rules = set_of_hashtbl unproductive_g_rules_cache
    }
;;

(* We record all function signatures and non-primitive operator calls in the so-called
   "arity table". If some operator (= function/constructor) is used inconsistently within
   the source program, we abort with a proper panic. Otherwise, supercompilation might
   crash or produce an incorrect residual program. *)
module Arity_table (_ : sig end) : sig
  val record : scrutinee:Raw_term.t -> Symbol.t * int -> unit
end = struct
  let table = Hashtbl.create 64

  let record ~scrutinee (op, arity) : unit =
      match Hashtbl.find_opt table op with
      | Some (prev_scrutinee, prev_arity) when arity <> prev_arity ->
        Util.panic
          "%s has an ambiguous arity: first %s, then %s"
          (Symbol.verbatim op)
          (Raw_term.verbatim prev_scrutinee)
          (Raw_term.verbatim scrutinee)
      | None when op = Symbol.of_string "Panic" && arity <> 1 ->
        Util.panic
          "%s accepts only one argument: %s"
          (Symbol.verbatim op)
          (Raw_term.verbatim scrutinee)
      | None when Symbol.(op = of_string "T" || op = of_string "F") && arity <> 0 ->
        Util.panic
          "%s accepts no arguments: %s"
          (Symbol.verbatim op)
          (Raw_term.verbatim scrutinee)
      | None when Symbol.is_op1 op && arity <> 1 ->
        Util.panic
          "%s accepts only one argument: %s"
          (Symbol.verbatim op)
          (Raw_term.verbatim scrutinee)
      | None when Symbol.is_op2 op && arity <> 2 ->
        Util.panic
          "%s accepts exactly two arguments: %s"
          (Symbol.verbatim op)
          (Raw_term.verbatim scrutinee)
      | None when not (Symbol.is_primitive_op op) ->
        Hashtbl.replace table op (scrutinee, arity)
      | _ -> ()
  ;;
end

let check_duplicate_symbols ~op list : unit =
    let rec go = function
      | [] -> ()
      | x :: xs when List.exists (( = ) x) xs ->
        Util.panic
          "A duplicate symbol %s occurs inside `%s(%s)`"
          (Symbol.verbatim x)
          (Symbol.to_string op)
          (Symbol.comma_sep list)
      | _x :: xs -> go xs
    in
    go list
;;

let print_pattern_list list =
    list
    |> List.map (fun (pattern, _t_raw) -> Raw_term.pattern_verbatim pattern)
    |> String.concat ", "
;;

let check_duplicate_patterns list : unit =
    let rec go = function
      | [] -> ()
      | ((c, _c_params), _t_raw) :: xs
        when List.exists (fun ((c', _c_params'), _t_raw') -> c = c') xs ->
        Util.panic
          "A duplicate pattern %s occurs among %s"
          (Symbol.verbatim c)
          (print_pattern_list list)
      | _x :: xs -> go xs
    in
    go list
;;

let to_program (input : t) : Program.t =
    let module Arity_table = Arity_table (struct end) in
    let f_gensym, g_gensym = Gensym.(create ~prefix:".f" (), create ~prefix:".g" ()) in
    let f_rules, g_rules, extract_f_rules =
        Program.(ref F_rules.empty, ref G_rules_by_name.empty, ref Symbol_set.empty)
    in
    let rec to_term : Raw_term.t -> Term.t * Symbol_set.t = function
      | Raw_term.Var x -> Term.Var x, Symbol_set.singleton x
      | Raw_term.Const const -> Term.Const const, Symbol_set.empty
      | Raw_term.Call (op, args_raw) as scrutinee ->
        Arity_table.record ~scrutinee (op, List.length args_raw);
        let args, args_fv = Symbol_set.decouple_map ~f:to_term args_raw in
        Term.Call (op, args), args_fv
      | Raw_term.Match (t_raw, cases_raw) ->
        check_duplicate_patterns cases_raw;
        let t, t_fv = to_term t_raw in
        let op, cases_fv_list, cases_fv = go_match_cases cases_raw in
        Term.(Call (op, t :: var_list cases_fv_list)), Symbol_set.union t_fv cases_fv
      | Raw_term.Let (x, t_raw, u_raw) ->
        if Symbol.is_primitive_op x
        then Util.panic "Cannot bind the primitive operator %s" (Symbol.verbatim x);
        let t, t_fv = to_term t_raw in
        let op, u_fv_list, u_fv = go_let_body (x, u_raw) in
        Term.(Call (op, t :: var_list u_fv_list)), Symbol_set.union t_fv u_fv
    (* Generate a new g-function that describes the [match] cases. *)
    and go_match_cases cases_raw =
        let cases, cases_fv =
            cases_raw
            |> Symbol_set.decouple_map ~f:(fun (((c, c_params) as pattern), t_raw) ->
              check_duplicate_symbols ~op:c c_params;
              Arity_table.record
                ~scrutinee:Raw_term.(Call (c, var_list c_params))
                (c, List.length c_params);
              let t, t_fv = to_term t_raw in
              (pattern, t), Symbol_set.(diff t_fv (of_list c_params)))
        in
        let cases_fv_list = Symbol_set.elements cases_fv in
        let rules =
            cases
            |> List.map (fun ((c, c_params), t) -> c, (c_params, cases_fv_list, t))
            |> List.to_seq
            |> Program.G_rules_by_pattern.of_seq
        in
        let fresh_g = Gensym.emit g_gensym in
        g_rules := Program.G_rules_by_name.add fresh_g rules !g_rules;
        fresh_g, cases_fv_list, cases_fv
    (* Generate a new f-function that describes the [let] body. *)
    and go_let_body (x, u_raw) =
        let u, u_fv = to_term u_raw in
        let u_fv = Symbol_set.(diff u_fv (singleton x)) in
        let params_tail = Symbol_set.elements u_fv in
        let fresh_f = Gensym.emit f_gensym in
        f_rules := Program.F_rules.add fresh_f (x :: params_tail, u) !f_rules;
        fresh_f, params_tail, u_fv
    in
    let seen_functions = Hashtbl.create 64 in
    input
    (* First, only check function signatures and record information about them. *)
    |> List.map (fun (attrs, f, params, body_raw) ->
      if Symbol.is_primitive_op f
      then Util.panic "Cannot redefine the primitive operator %s" (Symbol.verbatim f);
      if Symbol.op_kind f <> `FCall
      then
        Util.panic
          "Functions must follow the `camelCase` convention: %s"
          (Symbol.verbatim f);
      if Hashtbl.mem seen_functions f
      then Util.panic "%s is defined more than once" (Symbol.verbatim f);
      check_duplicate_symbols ~op:f params;
      Arity_table.record
        ~scrutinee:Raw_term.(Call (f, var_list params))
        (f, List.length params);
      Hashtbl.add seen_functions f ();
      if List.mem `Extract attrs then extract_f_rules := Symbol_set.add f !extract_f_rules;
      f, params, body_raw)
    (* Given that all function signatures are correct and their arities have been
       recorded, proceed with checking the bodies. *)
    |> List.iter (fun (f, params, body_raw) ->
      let body, body_fv = to_term body_raw in
      body_fv
      |> Symbol_set.find_first_opt (fun x -> not (List.mem x params))
      |> Option.iter (fun x ->
        Util.panic
          "The variable %s is unbound in the function %s"
          (Symbol.verbatim x)
          (Symbol.verbatim f));
      f_rules := Program.F_rules.add f (params, body) !f_rules);
    Program.
      { f_rules = !f_rules
      ; g_rules = !g_rules
      ; extract_f_rules = !extract_f_rules
      ; productive_g_rules = Symbol_map.empty
      ; unproductive_g_rules = Symbol_set.empty
      }
    |> compute_productive_g_rules
;;
