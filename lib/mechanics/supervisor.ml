[@@@coverage off]

module MakeMutable (S : sig
    val program : Program.t

    val inspect : bool

    val observe_node : Symbol.t * Term.t -> unit

    val unobserve_node : Symbol.t -> unit
  end) : sig
  val run : Term.t -> Process_graph.t
end = struct
  open Process_graph

  let var_gensym = Gensym.create ~prefix:".v" ()

  module Driver_inst = Driver.Make (struct
      include S

      let gensym = var_gensym
    end)

  module History_inst = History.Make (struct end)

  module State = struct
    let node_gensym : Gensym.t = Gensym.create ~prefix:"n" ()

    let processed_globals : (Term.t * Symbol.t) list ref = ref []

    let backup () =
        let gensym, globals = Gensym.clone node_gensym, !processed_globals in
        gensym, globals
    ;;

    let update (gensym, globals) =
        Gensym.assign ~other:gensym node_gensym;
        processed_globals := globals
    ;;
  end

  (* We do not store terms in the resulting process graph. Logging is used to recover them
     for inspection purposes. *)
  let report_node (n : Term.t) : Symbol.t =
      let n_id = Gensym.emit State.node_gensym in
      S.observe_node (n_id, n);
      n_id
  ;;

  (* Unreporting is used to "forget" nodes when generalization is performed. *)
  let unreport_nodes (gensym_backup : Gensym.t) : unit =
      let iter = Gensym.clone gensym_backup in
      let until = Gensym.latest State.node_gensym in
      let rec go () =
          let node_id = Gensym.emit iter in
          S.unobserve_node node_id;
          if node_id <> until then go ()
      in
      go ()
  ;;

  (* When node [m] (the ancestor) is homeomorphically embedded into [n] (the child), we
     issue a generalization of [m]. Alternatively, we could generalize [n], but doing so
     increases the size of residual code.

     When the generalization of [m] is not safe (i.e., the substitution contains potential
     loops and panics), we resort to splitting [m]. *)
  exception Failback of Symbol.t * [ `Generalize of Subst.t * Term.t | `Split ]

  (* When there is a _nested_ redex call of a function marked [@extract], we replace this
     call with a fresh variable and proceed supercompiling the extracted call and the
     enclosing shell separately. This mechanism can be seen as giving the supercompiler
     information about "dangerous" functions. *)
  exception Extract of ((Symbol.t * Term.t) * Term.t)

  let is_extractable f = Symbol_set.mem f S.program.extract_f_rules

  let is_unproductive g = Symbol_set.mem g S.program.unproductive_g_rules

  let is_extracted_call = function
    | Term.Call (f, _args) when is_extractable f -> true
    | _ -> false
  ;;

  let failback ~m_id action = raise_notrace (Failback (m_id, action))

  let extract t =
      let x = Gensym.emit var_gensym in
      raise_notrace (Extract ((x, t), Term.Var x))
  ;;

  let extract_call : Term.t -> unit =
      let open Term in
      let rec go ~depth = function
        | Call (f, _args) as t when is_extractable f && depth > 0 -> extract t
        | Call (op, args) when not (Symbol.is_lazy_op op) ->
          go_args ~depth ~op ~acc:Fun.id args
        | Var _ | Const _ | Call _ -> ()
      and go_args ~depth ~op ~acc = function
        | [] -> go_call ~depth ~op (Symbol.op_kind op, acc [])
        | t :: rest when is_value t ->
          go_args ~depth ~op ~acc:(fun xs -> acc (t :: xs)) rest
        | t :: rest ->
          (try go ~depth:(depth + 1) t with
           | Extract ((x, call), shell) ->
             let shell = Call (op, acc [] @ [ shell ] @ rest) in
             raise_notrace (Extract ((x, call), shell)))
      and go_call ~depth ~op = function
        | `GCall, Call (c, _c_args) :: _args when Symbol.op_kind c = `CCall -> ()
        | `GCall, (_ :: _ as args) when is_unproductive op && depth > 0 ->
          extract (Call (op, args))
        | (`CCall | `FCall | `GCall), _args -> ()
      in
      fun t -> go ~depth:0 t
  ;;

  let rec run ~(history : History_inst.t) (n : Term.t) : Process_graph.t =
      let n_id = report_node n in
      let gensym_backup, globals_backup = State.backup () in
      try check_extract ~history (n_id, n) with
      | Failback (m_id, action) as exn ->
        if m_id = n_id
        then (
          unreport_nodes gensym_backup;
          State.update (gensym_backup, globals_backup);
          match action with
          | `Generalize (subst, g) -> generalize ~history ~bindings:subst g
          | `Split -> split ~history n)
        else (* Rethrow the exception until [m_id] is found. *)
          raise_notrace exn

  and check_extract ~history (n_id, n) =
      match extract_call n with
      | exception Extract extraction -> extract ~history extraction
      | () -> check_globals ~history (n_id, n)

  (* If a node is global, try to fold it against existing global nodes. If the node is not
     global or folding failed, continue with [check_whistle].

     This technique helps us eliminate many equivalent function definitions in residual
     programs. Since driving blindly duplicates the whole context when analyzing a
     g-function call, we essentially eschew re-supervising a potentially very large space
     of program states. Since there are not many global nodes (in comparison with locals
     and trivials), checking them all is doable.

     We also treat calls to extractable f-functions as globals. *)
  and check_globals ~history (n_id, n) =
      let rec go = function
        | [] ->
          let graph = check_whistle ~history (n_id, n) in
          State.processed_globals := (n, n_id) :: !State.processed_globals;
          graph
        | (m, m_id) :: rest ->
          (match Term.rename_against (m, n) with
           | Some subst -> fold ~history ~bindings:subst m_id
           | None -> go rest)
      in
      match Term.classify n with
      | Term.Global -> go !State.processed_globals
      | _ when is_extracted_call n -> go !State.processed_globals
      | _ -> check_whistle ~history (n_id, n)

  (* If some ancestor node [m] is homeomorphically embedded into [n], do something with
     either [m] or [n] to ensure eventual termination of the algorithm. Otherwise, drive
     [n]. Before folding or generalizing any node, we must check that the computed
     substitution is "safe", so as to preserve the call-by-value semantics of code:
     preserving the existence (and order) of panics, foreign function calls, and
     non-termination. *)
  and check_whistle ~history (n_id, n) =
      match History_inst.memoize ~suspect:(n_id, n) history with
      | Some (m_id, m), history ->
        (match Term.match_against (m, n) with
         (* Fold: [n] is an instance of [m] through [subst], meaning that [subst] applied
            to [m] yields us [n]. This is the only case we fold, thereby issuing an
            upwards-directed pointer in the process graph, which will become a function
            call during residualization. The substitution must be safe, because in the
            residualized code, it will appear as an argument vector, which would make
            unsafe arguments evaluated too eagerly. *)
         | Some subst when Subst.is_safe subst -> fold ~history ~bindings:subst m_id
         | _ ->
           let g, subst_1, _subst_2 = Msg.compute ~gensym:var_gensym (m, n) in
           (match g with
            (* Split: there is no meaningful generalization of [m] and [n]. Since there is
               no common outer constructor in [m] and [n], [m] must be embedded into [n]
               by diving, making it impossible that [n] is a call with all arguments
               variables. Therefore, splitting [n] will necessarily make it smaller,
               ensuring that we make progress. *)
            | Term.Var _ -> split ~history n
            (* Multiple choices: [g] renames to [m]. *)
            | _ when Subst.is_renaming subst_1 ->
              (match n with
               | Term.Call (_op, args) when List.for_all Term.is_var args ->
                 (match m with
                  (* We are in a peculiar situation: both [m] and [n] are calls with all
                     arguments variables. A concrete example of this situation would be
                     [m] defined as [f(s, s)] and [n] defined as [f(x, y)], whose MSG [g]
                     would be [f(v1, v2)]. Notice that [n] is not an instance of [m], so
                     folding is not possible. What should we do? Our decision is to
                     generalize [m], so as to prohibit this type of situation in the
                     future: if we ever encounter a call to [f] with all arguments
                     variables, it will constitute a renaming of [g], thereby allowing
                     ourselves to fold as usual. *)
                  | Term.Call (_op', args') when List.for_all Term.is_var args' ->
                    failback ~m_id (`Generalize (subst_1, g))
                  | _ -> failback ~m_id `Split)
               | _ -> split ~history n)
            (* Split: [subst_1] is not safe, making generalization of [m] problematic. We
               proceed by splitting [m] because, since [subst_1] is not a safe
               substitution, it must contain at least one function call; and since [m] can
               be obtained through [g] by [subst_1], then this function call must be
               somewhere in [m] in argument position, ruling out the possibility that [m]
               is a call with all arguments values. *)
            | _ when not (Subst.is_safe subst_1) -> failback ~m_id `Split
            (* Perform upwards generalization otherwise. *)
            | _ -> failback ~m_id (`Generalize (subst_1, g))))
      | None, history -> Step (Driver_inst.run ~f:(run ~history) n)

  and supercompile_bindings ~history subst =
      subst |> Symbol_map.bindings |> List.map (fun (x, t) -> x, run ~history t)

  and fold ~history ~bindings m_id =
      let bindings_sup = supercompile_bindings ~history bindings in
      Bind (bindings_sup, Fold m_id)

  and generalize ~history ~bindings g =
      let bindings_sup = supercompile_bindings ~history bindings in
      let g_sup = run ~history g in
      Bind (bindings_sup, Generalize g_sup)

  and split ~history = function
    | Term.Call (op, args) ->
      let fresh_vars = Gensym.emit_list ~length_list:args var_gensym in
      let bindings_sup = List.map2 (fun x t -> x, run ~history t) fresh_vars args in
      let g_sup = run ~history Term.(Call (op, var_list fresh_vars)) in
      Bind (bindings_sup, Split g_sup)
    | _ -> failwith "Impossible"

  and extract ~history ((x, call), shell) =
      let call_sup = run ~history call in
      let shell_sup = run ~history shell in
      Process_graph.Step (Driver.Extract ((x, call_sup), shell_sup))
  ;;

  let run (t : Term.t) : Process_graph.t = run ~history:History_inst.empty t
end

module Make (S : sig
    val program : Program.t

    val inspect : bool

    val observe_node : Symbol.t * Term.t -> unit

    val unobserve_node : Symbol.t -> unit
  end) =
struct
  let run (t : Term.t) : Process_graph.t =
      let module Runner = MakeMutable (S) in
      Runner.run t
  ;;
end

module MakeSimple (S : sig
    val program : Program.t
  end) =
struct
  let run (t : Term.t) : Process_graph.t =
      let module Runner =
        MakeMutable (struct
          include S

          let inspect = false

          let observe_node (_id, _node) = ()

          let unobserve_node _id = ()
        end)
      in
      Runner.run t
  ;;
end
