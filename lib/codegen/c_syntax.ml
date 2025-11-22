[@@@coverage off]

(* The simplified syntax of C99 from [1] with some GNU extensions.
 *
 * [1] <https://www.open-std.org/JTC1/SC22/WG14/www/docs/n1256.pdf>
 *)

open Document

let c_block ?(sep = [ hardline ]) list =
    let list_doc = [ hardline; combine ~sep list ] |> combine ~nest:4 in
    braces [ list_doc; hardline ]
;;

(* 6.4.2 Identifiers *)
type c_identifier = document

let c_identifier x = atom (Symbol.to_string x)

(* 6.7.1 Storage-class specifiers *)
type c_storage_class_specifier = document

let c_static = `CStorageClassSpecifier (atom "static")

let c_extern = `CStorageClassSpecifier (atom "extern")

(* 6.7.2 Type specifiers *)
type c_type_specifier = document

let c_void = `CTypeSpecifier (atom "void")

let c_char = `CTypeSpecifier (atom "char")

let c_int = `CTypeSpecifier (atom "int")

let c_typedef_name identifier = `CTypeSpecifier identifier

(* 6.7.3 Type qualifiers *)
type c_type_qualifier = document

let c_const = `CTypeQualifier (atom "const")

(* 6.7.2.1 Structure and union specifiers *)
type c_specifier_qualifier =
  [ `CTypeSpecifier of c_type_specifier
  | `CTypeQualifier of c_type_qualifier
  ]

let combine_specifier_qualifier_list list =
    list
    |> List.map (function `CTypeSpecifier x | `CTypeQualifier x -> x)
    |> combine ~sep:[ space ]
;;

(* 6.7 Declarations *)
type c_declaration_specifier =
  [ `CStorageClassSpecifier of c_storage_class_specifier
  | `CTypeSpecifier of c_type_specifier
  | `CTypeQualifier of c_type_qualifier
  ]

let combine_declaration_specifiers list =
    list
    |> List.map (function
        | `CStorageClassSpecifier x | `CTypeSpecifier x | `CTypeQualifier x -> x)
    |> combine ~sep:[ space ]
;;

(* 6.7.5 Declarators *)
type c_declarator = document

let c_identifier_declarator identifier = identifier

(* 6.7.5 Declarators *)
type c_parameter_declaration = document

let c_parameter_declaration (declaration_specifiers, declarator) =
    [ combine_declaration_specifiers declaration_specifiers
    ; declarator |> optional ~f:(fun declarator -> combine [ space; declarator ])
    ]
    |> combine
;;

(* 6.7.5.2 Array declarators *)
let c_array_declarator (identifier, (assignment_expression : int)) =
    assert (assignment_expression > 0);
    [ identifier; brackets [ atom (string_of_int assignment_expression) ] ] |> combine
;;

(* 6.7.5.3 Function declarators (including prototypes) *)
let c_function_declarator (identifier, parameter_list) =
    [ identifier
    ; [ (match parameter_list with
         | [] -> atom "void"
         | _ -> comma_sep parameter_list)
      ]
      |> parens
    ]
    |> combine ~nest:4
;;

(* 6.7.2.1 Structure and union specifiers *)
type c_struct_declaration = document

let c_struct_declaration (specifier_qualifier_list, struct_declarator) =
    [ combine_specifier_qualifier_list specifier_qualifier_list
    ; space
    ; struct_declarator
    ; atom ";"
    ]
    |> combine
;;

let c_struct_specifier (identifier, struct_declaration_list) =
    `CTypeSpecifier
      ([ atom "struct"
       ; space
       ; identifier
       ; struct_declaration_list
         |> optional ~f:(fun struct_declaration_list ->
           combine [ space; c_block struct_declaration_list ])
       ]
       |> combine)
;;

(* 6.7.2.2 Enumeration specifiers *)
type c_enumerator = document

let c_enumerator (enumeration_constant, constant_expression) =
    match constant_expression with
    | None -> enumeration_constant
    | Some constant_expression ->
      [ enumeration_constant
      ; space
      ; atom "="
      ; space
      ; atom (string_of_int constant_expression)
      ]
      |> combine
;;

let c_enum_specifier (identifier, enumerator_list) =
    `CTypeSpecifier
      ([ atom "enum"
       ; space
       ; identifier
       ; space
       ; c_block ~sep:[ atom ","; space ] enumerator_list
       ]
       |> combine)
;;

(* 6.5 Expressions *)
type c_expression = document

let c_integer_constant_expression x =
    let (module S) = Checked_oint.singleton x in
    atom S.(to_string value)
;;

let c_identifier_expression identifier = identifier

(* 6.4.5 String literals *)
let c_string_literal s = atom (Const.escape_string s)

(* 6.5.2.1 Array subscripting *)
let c_array_subscript (expression, expression') =
    combine [ parens [ expression ]; brackets [ expression' ] ]
;;

(* 6.5.2.2 Function calls *)
let c_function_call (identifier, argument_expression_list) =
    [ identifier; parens [ comma_sep argument_expression_list ] ] |> combine
;;

(* 6.5.2.3 Structure and union members *)
let c_struct_member (expression, identifier) =
    combine [ parens [ expression ]; atom "."; identifier ]
;;

(* 6.5.4 Cast operators *)
let c_cast_expression (specifier_qualifier_list, cast_expression) =
    [ parens [ combine_specifier_qualifier_list specifier_qualifier_list ]
    ; cast_expression
    ]
    |> combine
;;

(* 6.5.16 Assignment operators *)
let c_assignment_expression (unary_expression, assignment_expression) =
    combine [ unary_expression; space; atom "="; space; assignment_expression ]
;;

(* 6.8 Statements and blocks *)
type c_statement = document

(* 6.7.8 Initialization *)
let c_initialization_declaration (declaration_specifiers, declarator, initializer') =
    [ combine_declaration_specifiers declaration_specifiers
    ; space
    ; declarator
    ; initializer'
      |> optional ~f:(fun initializer' ->
        combine [ space; atom "="; space; initializer' ])
    ; atom ";"
    ]
    |> combine
;;

(* 6.8.1 Labeled statements *)
let c_case_statement (constant_expression, statement) =
    [ atom "case"; space; constant_expression; atom ":"; space; statement ] |> combine
;;

(* 6.8.1 Labeled statements *)
let c_default_statement statement =
    [ atom "default"; atom ":"; space; statement ] |> combine
;;

(* 6.8.2 Compound statement *)
let c_compound_statement block_item_list =
    braces [ hardline; combine ~sep:[ hardline ] block_item_list; hardline ]
;;

(* 6.8.3 Expression and null statements *)
let c_expression_statement expression = combine [ expression; atom ";" ]

(* 6.8.4.2 The switch statement *)
let c_switch_statement (expression, statement) =
    [ atom "switch"; space; parens [ expression ]; space; statement ] |> combine
;;

(* 6.8.6.3 The break statement *)
let c_break_statement = combine [ atom "break"; atom ";" ]

(* 6.8.6.4 The return statement *)
let c_return_statement expression = combine [ atom "return"; space; expression; atom ";" ]

(* GNU C statement expressions [1].
 *
 * [1] <https://gcc.gnu.org/onlinedocs/gcc/Statement-Exprs.html>
 *)
let c_statement_expression block_item_list =
    parens [ c_compound_statement block_item_list ]
;;

(* 6.9 External definitions *)
type c_external_declaration = document

(* 6.7 Declarations *)
let c_declaration declaration_specifiers =
    [ combine_declaration_specifiers declaration_specifiers; atom ";"; hardline ]
    |> combine
;;

(* 6.7.5.3 Function declarators (including prototypes) *)
let c_function_prototype (declaration_specifiers, identifier, parameter_list) =
    [ combine_declaration_specifiers declaration_specifiers
    ; space
    ; c_function_declarator (identifier, parameter_list)
    ; atom ";"
    ; hardline
    ]
    |> combine
;;

(* 6.9.1 Function definitions *)
let c_function_definition
      (declaration_specifiers, identifier, parameter_list, block_item_list)
  =
    [ combine_declaration_specifiers declaration_specifiers
    ; space
    ; c_function_declarator (identifier, parameter_list)
    ; space
    ; c_block block_item_list
    ; hardline
    ]
    |> combine
;;

let c_include = function
  | `Angles s -> combine [ atom "#include"; space; atom "<"; atom s; atom ">"; hardline ]
  | `Quotes s ->
    combine [ atom "#include"; space; atom "\""; atom s; atom "\""; hardline ]
;;

let c_expand (identifier, argument_expression_list) =
    [ c_function_call (identifier, argument_expression_list); atom ";"; hardline ]
    |> combine ~nest:4
;;

let c_translation_unit ~(oc : out_channel) external_declarations =
    let doc = combine ~sep:[ hardline ] external_declarations in
    doc |> build ~width:100 |> output ~oc
;;
