[@@@coverage off]

type t =
  | Step of t Driver.step
  | Bind of (Symbol.t * t) list * binder

and binder =
  | Fold of node_id
  | Generalize of t
  | Split of t

and node_id = Symbol.t

type metadata =
  { symbol_table : (Symbol.t * Program.param_list) Symbol_map.t
  ; fresh_to_source_vars : Renaming.t
  }

let compute_metadata (graph : t) : metadata =
    let f_gensym = Gensym.create ~prefix:"f" () in
    let symbol_table = ref Symbol_map.empty in
    let fresh_to_source_vars = ref Symbol_map.empty in
    let rec go = function
      | Step step -> go_step step
      | Bind (bindings, binder) -> go_binder ~bindings binder
    and go_step = function
      | Driver.(Var _ | Const _) -> ()
      | Driver.Decompose (_op, params) -> List.iter go params
      | Driver.Unfold graph -> go graph
      | Driver.Analyze (_x, graph, variants) ->
        go graph;
        variants
        |> List.iter (fun (Driver.{ c = _; fresh_vars; source_vars }, (binding, graph)) ->
          fresh_vars
          |> List.iter2
               (fun x y ->
                  fresh_to_source_vars := Symbol_map.add x y !fresh_to_source_vars)
               source_vars;
          match binding with
          | Some binding -> go_extract (binding, graph)
          | None -> go graph)
      | Driver.Extract (binding, graph) -> go_extract (binding, graph)
    and go_extract ((x, call), graph) = go_binder ~bindings:[ x, call ] (Split graph)
    and go_binder ~bindings binder =
        List.iter (fun (_x, graph) -> go graph) bindings;
        match binder with
        | Fold node_id when not (Symbol_map.mem node_id !symbol_table) ->
          let f, params =
              Gensym.emit f_gensym, List.map (fun (x, _graph) -> x) bindings
          in
          symbol_table := Symbol_map.add node_id (f, params) !symbol_table
        (* We have already generated a symbol for this function. *)
        | Fold _ -> ()
        | Generalize graph | Split graph -> go graph
    in
    go graph;
    { symbol_table = !symbol_table; fresh_to_source_vars = !fresh_to_source_vars }
;;
