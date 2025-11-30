open Document

let print_symbol x = atom (Symbol.to_string x)

let print_symbol_list list = comma_sep (List.map print_symbol list)

let rec print = function
  | Raw_term.Var x -> print_symbol x
  | Raw_term.Const const -> atom (Const.to_string const)
  | Raw_term.Call (op, args) ->
    (* Since residual code may contain some deeply nested function calls, [~nest:4] would
       not be a good idea. *)
    (if Symbol.is_foreign_function op
     then [ atom "call"; space; atom "\""; print_symbol op; atom "\""; space ]
     else [ print_symbol op ])
    @ [ parens [ comma_sep (List.map print args) ] ]
    |> combine
  | Raw_term.Match (t, [ (pattern, u) ]) -> print_let (print_pattern pattern, t, u)
  | Raw_term.Match (t, cases) ->
    let cases_doc =
        [ combine ~sep:[ atom "," ] ~nest:4 (List.map print_case cases); hardline ]
        |> braces ~group:true
    in
    [ atom "match"; space; print t; space; cases_doc ] |> combine
  | Raw_term.Let (x, t, u) -> print_let (print_symbol x, t, u)

and print_flat = function
  | Raw_term.Let _ as t -> combine ~group:true [ break 0; print t ]
  | t -> print t

and print_let (init_doc, t, u) =
    [ combine [ atom "let"; space; init_doc; space; atom ":="; space; print t ]
    ; combine [ atom ";"; break 1; print u ]
    ]
    |> combine ~group:true

and print_case (pattern, t) =
    [ hardline; print_pattern pattern; space; atom "->"; space; print_flat t ] |> combine

and print_pattern (c, c_params) =
    [ print_symbol c; parens [ print_symbol_list c_params ] ] |> combine
;;

(* There are no attributes in residual code, so do not try to print them. *)
let print_def (_attrs, op, params, body) =
    let go_body = function
      | Raw_term.Match _ as body -> print body
      | Raw_term.(Var _ | Const _ | Call _ | Let _) as body ->
        combine ~nest:4 [ print_flat body ]
    in
    [ [ print_symbol op; parens [ print_symbol_list params ] ] |> combine
    ; space
    ; atom ":="
    ; space
    ; go_body body
    ; atom ";"
    ]
    |> combine
;;

let print_program ~oc ?width program =
    let program_doc =
        program |> List.map print_def |> combine ~sep:[ hardline; hardline ]
    in
    Document.(output ~oc:(Lazy.force oc) (build ?width program_doc))
;;
