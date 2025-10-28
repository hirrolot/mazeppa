[@@@coverage off]

open C_syntax
module C = Const

module Shortcuts = struct
  let id x = c_identifier (Symbol.of_string x)

  let index i = Checked_oint.U64.(to_generic (of_int_exn i))
end

open Shortcuts

let gen_symbol ~prefix x =
    let x = Symbol.to_string x in
    (* Unlike Mazeppa, GNU C does not accept apostrophes in identifiers, but accepts
       dollar signs. *)
    let x_without_apostrophes = Str.(global_replace (regexp "'") "$" x) in
    (if String.contains x '?'
     then prefix ^ "q_" ^ Str.(replace_first (regexp "?") "" x_without_apostrophes)
     else prefix ^ "_" ^ x_without_apostrophes)
    |> id
;;

let gen_var, gen_op = gen_symbol ~prefix:"var", gen_symbol ~prefix:"op"

let int_ty (signedness, bitness) =
    [ id (C.signedness_to_string_uppercase signedness); id (C.bitness_to_string bitness) ]
    |> List.map c_identifier_expression
;;

let gen_int =
    let open Checked_oint in
    let ready (op, x) = c_function_call (id op, [ c_integer_constant_expression x ]) in
    let gen_u64 x = ready ("UINT64_C", U64 x) in
    function
    | U8 _ as x -> ready ("UINT8_C", x)
    | U16 _ as x -> ready ("UINT16_C", x)
    | U32 _ as x -> ready ("UINT32_C", x)
    | U64 _ as x -> ready ("UINT64_C", x)
    | U128 x ->
      let high, low = U128.split x in
      c_function_call (id "mz_U128_of_parts", [ gen_u64 high; gen_u64 low ])
    | I8 _ as x -> ready ("INT8_C", x)
    | I16 _ as x -> ready ("INT16_C", x)
    | I32 _ as x -> ready ("INT32_C", x)
    | I64 _ as x -> ready ("INT64_C", x)
    | I128 x ->
      let high, low = I128.split x in
      c_function_call (id "mz_I128_of_parts", [ gen_u64 high; gen_u64 low ])
;;

let gen_const = function
  | C.Int x ->
    c_function_call
      (id "MZ_INT", int_ty (Checked_oint.Int_ty.of_generic x) @ [ gen_int x ])
  | C.String s -> c_function_call (id "MZ_STRING", [ c_string_literal s ])
;;

let op1_impl op =
    match Symbol.to_string op with
    | "~" -> id "mz_bit_not"
    | "string" -> id "mz_string_of"
    | "#" -> id "mz_string_of_char"
    | "length" -> id "mz_length_of"
    | ("u8" | "u16" | "u32" | "u64" | "u128" | "i8" | "i16" | "i32" | "i64" | "i128") as
      ty -> id ("mz_" ^ String.capitalize_ascii ty ^ "_cast")
    | op -> Util.panic "Unexpected unary operator: `%s`" op
;;

let op2_impl op =
    match Symbol.to_string op with
    | "+" -> "add"
    | "-" -> "sub"
    | "*" -> "mul"
    | "/" -> "div"
    | "%" -> "rem"
    | "|" -> "bit_or"
    | "&" -> "bit_and"
    | "^" -> "bit_xor"
    | "<<" -> "shift_left"
    | ">>" -> "shift_right"
    | "=" -> "equal"
    | "!=" -> "not_equal"
    | ">" -> "greater_than"
    | ">=" -> "greater_than_equal"
    | "<" -> "less_than"
    | "<=" -> "less_than_equal"
    | "++" -> "plus_plus"
    | "get" -> "get"
    | op -> Util.panic "Unexpected binary operator: `%s`" op
;;

module Make (S : sig
    val rules : Raw_program.t

    val entry : Symbol.t
  end) : sig
  val external_declarations : c_external_declaration list
end = struct
  let thunks, thunks_gensym = ref Fun.id, Gensym.create ~prefix:"thunk_" ()

  let constructor_symbols = ref Symbol_set.empty

  let remember_constructor c =
      if Symbol.(c <> of_string "T" && c <> of_string "F")
      then constructor_symbols := Symbol_set.add c !constructor_symbols
  ;;

  (* A variable is lazy if it is extracted from a constructor value by pattern matching.
     Otherwise, it is eager. Before using the value of a lazy variable, we must first call
     [mz_force] on it to force the computation. *)
  type binding_type =
    | VarEager
    | VarLazy

  type context =
    { renaming : Renaming.t
    ; types : binding_type Symbol_map.t
    }

  (* We freshen all bindings other than function parameters. For example:

     [let x := 1u8; let x := +(x, 2u8); x]

     will be renamed to:

     [let x := 1u8; let x' := +(x, 2u8); x']

     This is needed because in C, an expression that initializes a variable has access to
     the (undetermined) value of this variable:

     [int x = x + 1;]

     With freshening, we avoid this situation that would otherwise result in undefined
     behaviour (by reading uninitialized memory). In generated code, we prepend [var_] to
     variables and replace apostrophes (') with dollar signs ($), so the above Mazeppa
     code would first define [var_x] and then [var_x$]. *)
  let rec freshen ~ctx x =
      if Symbol_map.exists (fun _x' y -> x = y) ctx.renaming
      then freshen ~ctx Symbol.(of_string (to_string x ^ "'"))
      else x
  ;;

  let remember' ~ctx (x, ty) =
      let x' = freshen ~ctx x in
      ( x'
      , { renaming = Symbol_map.add x x' ctx.renaming
        ; types = Symbol_map.add x' ty ctx.types
        } )
  ;;

  let remember ~ctx (x, ty) =
      let _x', ctx = remember' ~ctx (x, ty) in
      ctx
  ;;

  let assoc ~map x =
      try Symbol_map.find x map with
      | Not_found -> Util.panic "Unbound variable %s" (Symbol.verbatim x)
  ;;

  let rec gen_term ~(ctx : context) = function
    | Raw_term.Var x ->
      let x = assoc ~map:ctx.renaming x in
      ( (match assoc ~map:ctx.types x with
         | VarEager -> c_identifier_expression (gen_var x)
         | VarLazy ->
           c_function_call (id "mz_force", [ c_identifier_expression (gen_var x) ]))
      , Symbol_set.singleton x )
    | Raw_term.Const const -> gen_const const, Symbol_set.empty
    | Raw_term.Let _ as t ->
      let t_gen, t_fv = flatten_let_sequence ~ctx ~seen:Symbol_set.empty t in
      c_statement_expression t_gen, t_fv
    | Raw_term.(Call (c, [ t ])) when c = Symbol.of_string "Panic" ->
      let t_gen, t_fv = gen_term ~ctx t in
      (* The panic term is computed eagerly because we need to print it. *)
      c_function_call (id "mz_panic", [ t_gen ]), t_fv
    | Raw_term.Call (c, args) when Symbol.op_kind c = `CCall ->
      gen_data_call ~ctx (c, args)
    | Raw_term.Call (op, [ t ]) when Symbol.is_op1 op ->
      let t_gen, t_fv = gen_term ~ctx t in
      c_function_call (op1_impl op, [ t_gen ]), t_fv
    | Raw_term.Call (op, [ t1; t2 ]) when Symbol.is_op2 op ->
      let t1_gen, t1_fv = gen_term ~ctx t1 in
      let t2_gen, t2_fv = gen_term ~ctx t2 in
      ( c_function_call
          (id "MZ_OP2", [ t1_gen; c_identifier_expression (id (op2_impl op)); t2_gen ])
      , Symbol_set.union t1_fv t2_fv )
    | Raw_term.Call (f, args) -> gen_call ~ctx (f, args)
    | Raw_term.Match (t, cases) -> gen_match ~ctx (t, cases)

  and flatten_let_sequence ~ctx ~seen = function
    | Raw_term.(Let (x, t, u)) ->
      let t_gen, t_fv = gen_term ~ctx t in
      let x', ctx = remember' ~ctx (x, VarEager) in
      (* See <https://github.com/hirrolot/mazeppa/issues/29>. *)
      let seen = Symbol_set.(if mem x' seen then empty else add x' seen) in
      let u_gen, u_fv = flatten_let_sequence ~ctx ~seen u in
      let scope =
          if Symbol_set.is_empty seen
          then
            fun block_item_list ->
              [ c_expression_statement (c_statement_expression block_item_list) ]
          else Fun.id
      in
      ( scope
          (c_initialization_declaration
             ( [ c_typedef_name (id "mz_Value") ]
             , c_identifier_declarator (gen_var x')
             , Some t_gen )
           :: u_gen)
      , Symbol_set.(union t_fv (diff u_fv (singleton x))) )
    | t ->
      let t_gen, t_fv = gen_term ~ctx t in
      [ c_expression_statement t_gen ], t_fv

  and gen_data_call ~ctx (c, args) =
      remember_constructor c;
      match args with
      | [] ->
        ( c_function_call (id "MZ_EMPTY_DATA", [ c_identifier_expression (gen_op c) ])
        , Symbol_set.empty )
      | _ ->
        let args_gen = List.map (gen_thunk_function ~ctx) args in
        let args_fv =
            Symbol_set.(
              List.fold_left union empty (List.map (fun (_, fv) -> fv) args_gen))
        in
        ( c_function_call
            ( id "MZ_DATA"
            , [ c_identifier_expression (gen_op c)
              ; c_integer_constant_expression (index (List.length args))
              ]
              @ List.map (gen_thunk_call ~ctx) args_gen )
        , args_fv )

  and gen_thunk_call ~ctx (thunk, env) =
      match thunk with
      | `SimpleThunk x -> gen_simple_thunk_call ~ctx x
      | `Thunk identifier when Symbol_set.is_empty env ->
        c_function_call (id "MZ_EMPTY_THUNK", [ c_identifier_expression identifier ])
      | `Thunk identifier ->
        c_function_call
          ( id "MZ_THUNK"
          , [ c_identifier_expression identifier
            ; c_integer_constant_expression (index (Symbol_set.cardinal env))
            ]
            @ (Symbol_set.elements env
               |> List.map (fun x ->
                 (* [x] is a computed free variable: no need to rename. *)
                 c_identifier_expression (gen_var x))) )

  (* If a variable stands as a constructor argument, it needs a thunk. Generating
     identical thunk definitions for each variable affects compilation times and code
     size, so instead, we reuse our predefined thunks. *)
  and gen_simple_thunk_call ~ctx x =
      match assoc ~map:ctx.types x with
      | VarEager ->
        c_function_call (id "MZ_SIMPLE_THUNK", [ c_identifier_expression (gen_var x) ])
      | VarLazy ->
        c_function_call
          (id "MZ_SIMPLE_THUNK_LAZY", [ c_identifier_expression (gen_var x) ])

  and gen_thunk_function ~ctx t =
      let t_gen, t_fv = gen_term ~ctx t in
      ( (match t with
         | Raw_term.Var x -> `SimpleThunk (assoc ~map:ctx.renaming x)
         | _ ->
           let declaration_specifiers = [ c_static; c_typedef_name (id "mz_Value") ] in
           let identifier = c_identifier (Gensym.emit thunks_gensym) in
           let parameter_list =
               [ c_parameter_declaration
                   ( [ c_typedef_name (id "mz_EnvPtr") ]
                   , Some (c_identifier_declarator (id "env")) )
               ]
           in
           let block_item_list = gen_thunk_function_body (t_gen, t_fv) in
           let f = !thunks in
           (thunks
            := fun xs ->
                 f
                   (c_function_definition
                      (declaration_specifiers, identifier, parameter_list, block_item_list)
                    :: xs));
           `Thunk identifier)
      , t_fv )

  and gen_thunk_function_body (t_gen, t_fv) =
      (Symbol_set.elements t_fv
       |> List.mapi (fun i x ->
         c_initialization_declaration
           ( [ c_typedef_name (id "mz_Value") ]
           , c_identifier_declarator (gen_var x)
           , Some
               (c_array_subscript
                  ( c_identifier_expression (id "env")
                  , c_integer_constant_expression (index i) )) )))
      @ [ c_return_statement t_gen ]

  and gen_call ~ctx (f, args) =
      match args with
      | [] -> c_function_call (gen_op f, []), Symbol_set.empty
      | args ->
        let args_gen, args_fv = Symbol_set.decouple_map ~f:(gen_term ~ctx) args in
        ( [ c_initialization_declaration
              ( [ c_struct_specifier (id "mz_value", None) ]
              , c_array_declarator (id "args", List.length args)
              , None )
          ]
          @ (args_gen
             |> List.mapi (fun i t_gen ->
               c_expression_statement
                 (c_assignment_expression
                    ( c_array_subscript
                        ( c_identifier_expression (id "args")
                        , c_integer_constant_expression (index i) )
                    , t_gen ))))
          @ [ c_expression_statement
                (c_function_call (gen_op f, [ c_identifier_expression (id "args") ]))
            ]
          |> c_statement_expression
        , args_fv )

  and gen_match ~ctx (t, cases) =
      let t_gen, t_fv = gen_term ~ctx t in
      let cases_gen, cases_fv = gen_cases ~ctx cases in
      let block_item_list =
          cases_gen
          @ [ c_default_statement
                (c_expression_statement
                   (c_function_call
                      ( id "MZ_UNEXPECTED_TAG"
                      , [ c_struct_member (c_identifier_expression (id "tmp"), id "tag") ]
                      )))
            ]
      in
      ( c_statement_expression
          [ c_initialization_declaration
              ( [ c_struct_specifier (id "mz_value", None) ]
              , c_identifier_declarator (id "tmp")
              , Some t_gen )
          ; c_switch_statement
              ( c_struct_member (c_identifier_expression (id "tmp"), id "tag")
              , c_compound_statement block_item_list )
          ; c_expression_statement (c_identifier_expression (id "tmp"))
          ]
      , Symbol_set.union t_fv cases_fv )

  and gen_cases ~ctx cases =
      cases
      |> Symbol_set.decouple_map ~f:(fun ((c, c_params), t) ->
        remember_constructor c;
        let ctx = List.fold_left (fun ctx x -> remember ~ctx (x, VarLazy)) ctx c_params in
        let t_gen, t_fv = gen_term ~ctx t in
        ( c_case_statement
            ( c_identifier_expression (gen_op c)
            , c_compound_statement
                ((c_params
                  |> List.mapi (fun i x ->
                    let initializer' =
                        c_array_subscript
                          ( c_struct_member
                              (c_identifier_expression (id "tmp"), id "payload")
                          , c_integer_constant_expression (index i) )
                    in
                    c_initialization_declaration
                      ( [ c_typedef_name (id "mz_Value") ]
                      , c_identifier_declarator (gen_var (assoc ~map:ctx.renaming x))
                      , Some initializer' )))
                 @ [ c_expression_statement
                       (c_assignment_expression
                          (c_identifier_expression (id "tmp"), t_gen))
                   ; c_break_statement
                   ]) )
        , Symbol_set.(diff t_fv (of_list c_params)) ))
  ;;

  let gen_function_body ~params body =
      let ctx =
          List.fold_left
            (fun ctx x -> remember ~ctx (x, VarEager))
            { renaming = Symbol_map.empty; types = Symbol_map.empty }
            params
      in
      let body_gen, _body_fv = gen_term ~ctx body in
      (params
       |> List.mapi (fun i x ->
         c_initialization_declaration
           ( [ c_typedef_name (id "mz_Value") ]
           , c_identifier_declarator (gen_var x)
           , Some
               (c_array_subscript
                  ( c_identifier_expression (id "args")
                  , c_integer_constant_expression (index i) )) )))
      @ [ c_return_statement body_gen ]
  ;;

  let function_prototypes, function_definitions =
      S.rules
      |> List.map (fun (_attrs, f, params, body) ->
        let declaration_specifiers = [ c_static; c_typedef_name (id "mz_Value") ] in
        let identifier = gen_op f in
        let parameter_list =
            match params with
            | [] -> []
            | _ ->
              [ c_parameter_declaration
                  ( [ c_typedef_name (id "mz_ArgsPtr") ]
                  , Some (c_identifier_declarator (id "args")) )
              ]
        in
        ( c_function_prototype (declaration_specifiers, identifier, parameter_list)
        , c_function_definition
            ( declaration_specifiers
            , identifier
            , parameter_list
            , gen_function_body ~params body ) ))
      |> List.split
  ;;

  let main_symbol = Symbol.of_string "main"

  let main_params =
      match
        List.find_map
          (fun (_attrs, f, params, _body) ->
             if f = main_symbol then Some params else None)
          S.rules
      with
      | Some main_params -> main_params
      | None -> Util.panic "No main function found"
  ;;

  let main_wrapper_body =
      match main_params with
      | [] -> [ c_return_statement (c_function_call (gen_op main_symbol, [])) ]
      | _ ->
        [ c_return_statement
            (c_function_call
               ( id "MZ_CALL_MAIN"
               , List.map (fun x -> c_identifier_expression (gen_var x)) main_params ))
        ]
  ;;

  let main_wrapper =
      c_function_definition
        ( [ c_extern; c_typedef_name (id "mz_Value") ]
        , c_identifier S.entry
        , main_params
          |> List.map (fun x ->
            c_parameter_declaration
              ( [ c_typedef_name (id "mz_Value") ]
              , Some (c_identifier_declarator (gen_var x)) ))
        , main_wrapper_body )
  ;;

  let user_enumeration_definition =
      c_expand
        ( id "MZ_ENUM_USER_TAGS"
        , Symbol_set.elements !constructor_symbols
          |> List.map (fun c -> c_identifier_expression (gen_op c)) )
  ;;

  let external_declarations : c_external_declaration list =
      [ c_include (`Quotes "mazeppa.h") ]
      @ [ user_enumeration_definition ]
      @ function_prototypes
      @ !thunks []
      @ function_definitions
      @ [ main_wrapper ]
  ;;
end

let run ~(oc : out_channel) ~(entry : Symbol.t) (rules : Raw_program.t) : unit =
    let module M =
      Make (struct
        let rules, entry = rules, entry
      end)
    in
    c_translation_unit ~oc M.external_declarations
;;
