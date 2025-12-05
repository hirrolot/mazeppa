open Document

let print_symbol x = atom (Symbol.to_string x)

let print_symbol_list list = comma_sep (List.map print_symbol list)

let rec print = function
  | Raw_term.Var x -> print_symbol x
  | Raw_term.Const const -> atom (Const.to_string const)
  | Raw_term.Call (op, args) when Symbol.is_foreign_function op ->
    [ atom "call"; space; atom "\""; print_symbol op; atom "\""; space; print_args args ]
    |> combine
  | Raw_term.Call (op, args) -> [ print_symbol op; print_args args ] |> combine
  | Raw_term.Match (t, [ (pattern, u) ]) -> print_let (print_pattern pattern, t, u)
  | Raw_term.Match (t, cases) ->
    let scrutinee_doc = [ atom "match"; space; print t ] |> combine in
    let cases_doc =
        [ combine ~sep:[ atom "," ] (List.map print_case cases); hardline ]
        |> braces ~group:true
    in
    [ scrutinee_doc; space; cases_doc ] |> combine
  | Raw_term.Let (x, t, u) -> print_let (print_symbol x, t, u)

and print' ?nest = function
  | Raw_term.(Match _ | Let _) as t -> combine ?nest [ hardline; print t ]
  | t -> print t

and print_args args =
    [ break 0; args |> List.map print |> combine ~sep:[ atom ","; break 1 ] ]
    |> parens ~group:true ~nest:4

and print_let (init_doc, t, u) =
    [ atom "let"
    ; space
    ; init_doc
    ; space
    ; atom ":="
    ; space
    ; print' ~nest:4 t
    ; atom ";"
    ; hardline
    ; combine [ print u ]
    ]
    |> combine

and print_case (pattern, t) =
    [ hardline; print_pattern pattern; space; atom "->"; space; print' t ] |> combine

and print_pattern (c, c_params) =
    [ print_symbol c; parens [ print_symbol_list c_params ] ] |> combine
;;

let print_def (attrs, op, params, body) =
    (* TODO: change to [List.is_empty] for newer versions of OCaml. *)
    assert (List.length attrs = 0);
    [ [ print_symbol op; parens [ print_symbol_list params ] ] |> combine
    ; space
    ; atom ":="
    ; space
    ; print' ~nest:4 body
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
