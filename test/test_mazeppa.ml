(* Test only utilitary functions and specific scenarios here. The code in [mechanics/] is
   covered by end-to-end tests in [examples/]. *)

open Mazeppa
open Mazeppa.Internals
module T = Term
module R = Raw_term

let u8, u16, u32, u64, i32, i64, i128 =
    let open Checked_oint in
    ( (fun x -> U8 (U8.of_int_exn x))
    , (fun x -> U16 (U16.of_int_exn x))
    , (fun x -> U32 (U32.of_int_exn x))
    , (fun x -> U64 (U64.of_int_exn x))
    , (fun x -> I32 (I32.of_int_exn x))
    , (fun x -> I64 (I64.of_int_exn x))
    , fun x -> I128 (I128.of_int_exn x) )
;;

let symbol = Symbol.of_string

let list xs =
    List.fold_right
      (fun x xs -> Raw_term.(call ("Cons", [ x; xs ])))
      xs
      (Raw_term.call ("Nil", []))
;;

let make_subst list =
    list |> List.map (fun (x, t) -> symbol x, t) |> List.to_seq |> Symbol_map.of_seq
;;

let print_constants () =
    let check ~expected const =
        Alcotest.(check' string)
          ~msg:"Print constants"
          ~actual:(Const.to_string const)
          ~expected
    in
    (* Includes the set of all printable characters in the default C locale (from
       <https://en.cppreference.com/w/c/string/byte/isprint>). *)
    let characters =
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~ \
         \012\n\
         \r\t\011\x00\xFF"
    in
    let escaped_characters =
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\\\"#$%&'()*+,-./:;<=>?@[\\\\]^_`{|}~ \
         \\f\\n\\r\\t\\v\\x00\\xFF"
    in
    check ~expected:("\"" ^ escaped_characters ^ "\"") (Const.String characters);
    check ~expected:"42u8" (Const.Int (u8 42));
    check ~expected:"42u16" (Const.Int (u16 42));
    check ~expected:"42i32" (Const.Int (i32 42));
    check ~expected:"42i64" (Const.Int (i64 42));
    check ~expected:"42i128" (Const.Int (i128 42))
;;

let print_constants_cases = [ "Tests", print_constants ]

let classify () =
    let module Category = struct
      type t = T.category

      let pp = T.pp_category

      let equal = T.equal_category
    end
    in
    let check ~expected t =
        Alcotest.(check' (module Category))
          ~msg:"Classify"
          ~actual:(T.classify t)
          ~expected
    in
    check ~expected:T.Trivial (T.var "x");
    check ~expected:T.Trivial (T.string "hello world");
    check ~expected:T.Trivial T.(call ("Foo", [ call ("f", []); call ("g", []) ]));
    check ~expected:T.Global T.(call (".g", [ var "x"; call ("f", []) ]));
    check
      ~expected:T.Global
      T.(call (".g", [ call ("+", [ var "x"; var "y" ]); var "z" ]));
    check
      ~expected:T.Global
      T.(call (".g", [ call ("mz_ffi_f", [ int (u8 42) ]); var "y" ]));
    [ ".g1"; "f" ]
    |> List.iter (fun op ->
      let args = T.[ call (".g2", [ var "x" ]); var "y"; call ("f", []) ] in
      check ~expected:T.Global (T.call (op, args));
      check ~expected:T.Global (T.call (op, List.rev args)));
    check
      ~expected:T.Local
      T.(
        call
          ( "f"
          , [ call (".g", [ call ("f'", []) ])
            ; var "x"
            ; int (u8 42)
            ; string "hello world"
            ] ))
;;

let term_classification_cases = [ "Tests", classify ]

let redex_sig () =
    let module Redex_sig = struct
      type t = T.redex_sig

      let pp = T.pp_redex_sig

      let equal = T.equal_redex_sig
    end
    in
    let check ~expected t =
        Alcotest.(check' (module Redex_sig))
          ~msg:"Redex signature"
          ~actual:(T.redex_sig t)
          ~expected
    in
    check ~expected:None (T.var "x");
    check ~expected:None (T.string "hello world");
    check ~expected:None T.(call ("Foo", [ call ("f", []); call ("g", []) ]));
    check ~expected:None T.(call ("f", [ call ("Panic", [ var "x" ]) ]));
    let redex, redex_sig =
        ( T.(call ("f", [ var "x"; string "hello world"; call ("Foo", []) ]))
        , Some (symbol "f", T.[ VNeutral; VConst; VCCall (symbol "Foo") ]) )
    in
    let redex = ref redex in
    for _i = 0 to 9 do
      check ~expected:redex_sig !redex;
      redex := T.call ("f", [ !redex ])
    done
;;

let term_redex_signature_cases = [ "Tests", redex_sig ]

let subst () =
    let check ~expected ~x ~value t =
        Alcotest.(check' (module Term))
          ~msg:"Substitute"
          ~actual:(T.subst ~x ~value t)
          ~expected
    in
    check ~expected:(T.var "y") ~x:(symbol "x") ~value:(T.var "y") (T.var "x");
    check ~expected:(T.var "x") ~x:(symbol "z") ~value:(T.var "y") (T.var "x");
    check
      ~expected:(T.string "hello world")
      ~x:(symbol "x")
      ~value:(T.var "y")
      (T.string "hello world");
    let t x = T.(call ("f", [ var "a"; var "b"; x; var "c"; var "d" ])) in
    check ~expected:(t (T.var "y")) ~x:(symbol "x") ~value:(T.var "y") (t (T.var "x"));
    check
      ~expected:(t T.(call ("f", [ var "y" ])))
      ~x:(symbol "x")
      ~value:(T.var "y")
      (t T.(call ("f", [ var "x" ])));
    (* Test physical equalities (implicit sharing). *)
    let subst t = T.subst ~x:(symbol "x") ~value:(T.var "y") t in
    let t = T.(call ("f", [ var "a"; var "b"; var "c" ])) in
    assert (t == subst t);
    let t' = T.(call ("f", [ var "x"; t ])) in
    let[@warning "-8"] (T.Call (_op, [ _; t'' ])) = subst t' in
    assert (t == t'')
;;

let substitution_cases = [ "Tests", subst ]

let match_against () =
    let check ~expected (t1, t2) =
        Alcotest.(check' (option (module Subst)))
          ~msg:"Match against"
          ~actual:(T.match_against (t1, t2))
          ~expected
    in
    check
      ~expected:(Some (make_subst [ "x", T.call ("Foo", []) ]))
      T.(var "x", call ("Foo", []));
    check
      ~expected:(Some (make_subst [ "x", T.var "y" ]))
      T.(call ("Foo", [ var "x"; var "x" ]), call ("Foo", [ var "y"; var "y" ]));
    check
      ~expected:None
      T.(call ("Foo", [ var "x"; var "x" ]), call ("Foo", [ var "y"; var "z" ]));
    check ~expected:None T.(call ("Foo", []), var "x");
    check ~expected:None T.(call ("Foo", []), call ("Bar", []));
    check ~expected:(Some (make_subst [])) T.(string "hello world", string "hello world");
    check ~expected:None T.(string "hello world", string "hello world 123")
;;

let term_instances_cases = [ "Tests", match_against ]

let rename_against () =
    let check ~expected (t1, t2) =
        Alcotest.(check' (option (module Subst)))
          ~msg:"Rename against"
          ~actual:(T.rename_against (t1, t2))
          ~expected
    in
    check ~expected:None T.(var "x", call ("Foo", []));
    check
      ~expected:(Some (make_subst [ "x", T.var "y" ]))
      T.(call ("Foo", [ var "x"; var "x" ]), call ("Foo", [ var "y"; var "y" ]));
    check
      ~expected:None
      T.(call ("Foo", [ var "x"; var "x" ]), call ("Foo", [ var "y"; var "z" ]));
    check ~expected:None T.(call ("Foo", []), var "x");
    check ~expected:None T.(call ("Foo", []), call ("Bar", []));
    check ~expected:(Some (make_subst [])) T.(string "hello world", string "hello world");
    check ~expected:None T.(string "hello world", string "hello world 123")
;;

let term_renamings_cases = [ "Tests", rename_against ]

let msg () =
    let check ~expected (t1, t2) =
        let gensym = Gensym.create ~prefix:".v" () in
        Alcotest.(check' (module Msg))
          ~msg:"MSG"
          ~actual:(Msg.compute ~gensym (t1, t2))
          ~expected
    in
    (* The following two samples were taken from [1].

       [1] Glück, R., Klimov, A.V., & Nepeivoda, A. (2016). Nonlinear Configurations for
       Superlinear Speedup by Supercompilation. *)
    check
      ~expected:
        ( T.(call ("f", [ var ".v2"; var ".v2" ]))
        , make_subst T.[ ".v2", var "x" ]
        , make_subst T.[ ".v2", call ("f", [ var "v0"; var "v0" ]) ] )
      T.(
        ( call ("f", [ var "x"; var "x" ])
        , call
            ( "f"
            , [ call ("f", [ var "v0"; var "v0" ]); call ("f", [ var "v0"; var "v0" ]) ]
            ) ));
    check
      ~expected:
        ( T.(call ("f", [ var ".v1"; var ".v2" ]))
        , make_subst T.[ ".v1", var "x"; ".v2", var "y" ]
        , make_subst
            T.
              [ ".v1", call ("f", [ var "x"; var "x" ])
              ; ".v2", call ("f", [ var "x"; var "x" ])
              ] )
      T.(
        ( call ("f", [ var "x"; var "y" ])
        , call
            ("f", [ call ("f", [ var "x"; var "x" ]); call ("f", [ var "x"; var "x" ]) ])
        ));
    (* Neither common functor nor common substitution applies. *)
    check
      ~expected:
        ( T.var ".v0"
        , make_subst [ (".v0", T.(call ("f", [ var "x" ]))) ]
        , make_subst [ (".v0", T.(call ("g", [ var "y" ]))) ] )
      T.(call ("f", [ var "x" ]), call ("g", [ var "y" ]))
;;

let msg_cases = [ "Tests", msg ]

let he () =
    let module Emb = Homeomorphic_emb.Make (struct end) in
    let check ~expected (t1, t2) =
        Alcotest.(check' bool)
          ~msg:"Homeomorphic embedding"
          ~actual:(Emb.decide (t1, t2))
          ~expected
    in
    (* All variables are collapsed into a single unique constructor. *)
    check ~expected:true T.(var "x", var "x");
    check ~expected:true T.(var "x", var "y");
    (* Coupling of respective arguments. *)
    check ~expected:true T.(call ("f", []), call ("f", []));
    check
      ~expected:true
      T.(call ("f", [ call ("g", []) ]), call ("f", [ call ("g", []) ]));
    check
      ~expected:false
      T.(call ("f", [ call ("g", []) ]), call ("f", [ call ("h", []) ]));
    (* Diving into some argument. *)
    check ~expected:true T.(var "x", call ("f", [ var "y"; call ("g", []) ]));
    check ~expected:false T.(var "x", call ("f", [ call ("g", []) ]));
    check ~expected:false T.(call ("f", []), var "x");
    (* A bigger term cannot be embedded into a smaller one. *)
    check
      ~expected:false
      T.(call ("f", [ var "x"; int (u8 10); string "hello world" ]), call ("f", []));
    (* Integers of the same type are embedded in each other structurally. *)
    for i = -100 to 100 do
      for j = -100 to 100 do
        check ~expected:(i <= j) T.(int (i32 i), int (i32 j))
      done
    done;
    (* Integers of distinct types cannot embed into each other -- even if they have the
       same numeric value. *)
    check ~expected:false T.(int (u8 10), int (u16 10));
    (* Checks that a given sequence of strings is well-formed. *)
    let check_strings (sequence : string array) =
        for i = 0 to Array.length sequence - 1 do
          for j = 0 to i - 1 do
            let s1, s2 = sequence.(j), sequence.(i) in
            check ~expected:false T.(string s1, string s2)
          done
        done
    in
    (* [set(s1) = set(s2) /\ length(s1) <= length(s2)] *)
    check_strings [| "abc"; "ac"; "a" |];
    check_strings [| "a"; "b"; "c" |];
    check_strings [| "c"; "b"; "a" |];
    check_strings [| "aa"; "ccc"; "bbbbaa"; "ca" |];
    check ~expected:true T.(string "abc", string "abc");
    check ~expected:true T.(string "abc", string "aaabc");
    (* Incomparable terms. *)
    let incomparable_terms = T.[ int (u8 10); string "hello world"; var "x" ] in
    incomparable_terms
    |> List.iteri (fun i const ->
      incomparable_terms
      |> List.iteri (fun j const' ->
        if i <> j
        then
          (* Incomparable terms have different outer constructors. *)
          check ~expected:false (const, const')))
;;

let he_cases = [ "Tests", he ]

(* Only test errors. Happy paths are covered in [examples/]. *)
let raw_program_errors () =
    let check ~expected input =
        Alcotest.check_raises "Raw program errors" (Mazeppa.Panic expected) (fun () ->
          Mazeppa.check input)
    in
    (* Just for the test coverage. *)
    Mazeppa.check [ [], symbol "main", [ symbol "x" ], R.var "x" ];
    check
      ~expected:"Cannot redefine the primitive operator `+`"
      [ [], symbol "+", [ symbol "x" ], R.var "x" ];
    check
      ~expected:"Cannot bind the primitive operator `+`"
      [ ([], symbol "f", [], R.(Let (symbol "+", call ("Foo", []), call ("Bar", [])))) ];
    check
      ~expected:"Functions must follow the `camelCase` convention: `Foo`"
      [ [], symbol "Foo", [ symbol "x" ], R.var "x" ];
    check
      ~expected:"`f` is defined more than once"
      [ [], symbol "f", [ symbol "x" ], R.var "x"
      ; [], symbol "f", [], R.string "hello world"
      ];
    check
      ~expected:"The variable `z` is unbound in the function `f`"
      [ [], symbol "f", [ symbol "x"; symbol "y" ], R.var "z" ];
    check
      ~expected:"A duplicate symbol `x` occurs inside `f(x, x)`"
      [ [], symbol "f", [ symbol "x"; symbol "x" ], R.var "x" ];
    check
      ~expected:"A duplicate symbol `y` occurs inside `Foo(y, y)`"
      [ ( []
        , symbol "f"
        , [ symbol "x" ]
        , R.(Match (var "x", [ (symbol "Foo", [ symbol "y"; symbol "y" ]), var "y" ])) )
      ];
    check
      ~expected:"A duplicate pattern `A` occurs among `A()`, `B(x)`, `A(x, y)`"
      [ ( []
        , symbol "f"
        , [ symbol "x" ]
        , R.(
            Match
              ( var "x"
              , [ (symbol "A", []), call ("Foo", [])
                ; (symbol "B", [ symbol "x" ]), call ("Bar", [ var "x" ])
                ; ( (symbol "A", [ symbol "x"; symbol "y" ])
                  , call ("Baz", [ var "x"; var "y" ]) )
                ] )) )
      ];
    check
      ~expected:"`Panic` accepts only one argument: `Panic(x, x)`"
      [ ([], symbol "f", [ symbol "x" ], R.(call ("Panic", [ var "x"; var "x" ]))) ];
    check
      ~expected:"`T` accepts no arguments: `T(x)`"
      [ ([], symbol "f", [ symbol "x" ], R.(call ("T", [ var "x" ]))) ];
    check
      ~expected:"`F` accepts no arguments: `F(x)`"
      [ ([], symbol "f", [ symbol "x" ], R.(call ("F", [ var "x" ]))) ];
    check
      ~expected:"`f` has an ambiguous arity: first `f(x)`, then `f(x, x)`"
      [ [], symbol "f", [ symbol "x" ], R.var "x"
      ; ([], symbol "g", [ symbol "x" ], R.(call ("f", [ var "x"; var "x" ])))
      ];
    check
      ~expected:"`Foo` has an ambiguous arity: first `Foo(x)`, then `Foo(x, y)`"
      [ ([], symbol "f", [ symbol "x" ], R.(call ("Foo", [ var "x" ])))
      ; ( []
        , symbol "g"
        , [ symbol "x"; symbol "y" ]
        , R.(call ("Foo", [ var "x"; var "y" ])) )
      ];
    check
      ~expected:"`A` has an ambiguous arity: first `A()`, then `A(x)`"
      [ ( []
        , symbol "f"
        , []
        , R.(Match (call ("A", []), [ (symbol "A", [ symbol "x" ]), var "x" ])) )
      ];
    (* Test unary primitive operators with wrong arity. *)
    check
      ~expected:"`~` accepts only one argument: `~()`"
      [ ([], symbol "f", [], R.(call ("~", []))) ];
    check
      ~expected:"`~` accepts only one argument: `~(x, y)`"
      [ ([], symbol "f", [ symbol "x"; symbol "y" ], R.(call ("~", [ var "x"; var "y" ])))
      ];
    check
      ~expected:"`#` accepts only one argument: `#()`"
      [ ([], symbol "f", [], R.(call ("#", []))) ];
    check
      ~expected:"`length` accepts only one argument: `length(x, y, z)`"
      [ ( []
        , symbol "f"
        , [ symbol "x"; symbol "y"; symbol "z" ]
        , R.(call ("length", [ var "x"; var "y"; var "z" ])) )
      ];
    check
      ~expected:"`string` accepts only one argument: `string()`"
      [ ([], symbol "f", [], R.(call ("string", []))) ];
    check
      ~expected:"`u8` accepts only one argument: `u8(x, y)`"
      [ ([], symbol "f", [ symbol "x"; symbol "y" ], R.(call ("u8", [ var "x"; var "y" ])))
      ];
    check
      ~expected:"`i32` accepts only one argument: `i32(x, y, z, w)`"
      [ ( []
        , symbol "f"
        , [ symbol "x"; symbol "y"; symbol "z"; symbol "w" ]
        , R.(call ("i32", [ var "x"; var "y"; var "z"; var "w" ])) )
      ];
    (* Test binary primitive operators with wrong arity. *)
    check
      ~expected:"`+` accepts exactly two arguments: `+()`"
      [ ([], symbol "f", [], R.(call ("+", []))) ];
    check
      ~expected:"`+` accepts exactly two arguments: `+(x)`"
      [ ([], symbol "f", [ symbol "x" ], R.(call ("+", [ var "x" ]))) ];
    check
      ~expected:"`+` accepts exactly two arguments: `+(x, y, z)`"
      [ ( []
        , symbol "f"
        , [ symbol "x"; symbol "y"; symbol "z" ]
        , R.(call ("+", [ var "x"; var "y"; var "z" ])) )
      ];
    check
      ~expected:"`-` accepts exactly two arguments: `-(x)`"
      [ ([], symbol "f", [ symbol "x" ], R.(call ("-", [ var "x" ]))) ];
    check
      ~expected:"`*` accepts exactly two arguments: `*()`"
      [ ([], symbol "f", [], R.(call ("*", []))) ];
    check
      ~expected:"`/` accepts exactly two arguments: `/(x, y, z)`"
      [ ( []
        , symbol "f"
        , [ symbol "x"; symbol "y"; symbol "z" ]
        , R.(call ("/", [ var "x"; var "y"; var "z" ])) )
      ];
    check
      ~expected:"`=` accepts exactly two arguments: `=(x)`"
      [ ([], symbol "f", [ symbol "x" ], R.(call ("=", [ var "x" ]))) ];
    check
      ~expected:"`!=` accepts exactly two arguments: `!=()`"
      [ ([], symbol "f", [], R.(call ("!=", []))) ];
    check
      ~expected:"`<` accepts exactly two arguments: `<(x, y, z, w)`"
      [ ( []
        , symbol "f"
        , [ symbol "x"; symbol "y"; symbol "z"; symbol "w" ]
        , R.(call ("<", [ var "x"; var "y"; var "z"; var "w" ])) )
      ];
    check
      ~expected:"`get` accepts exactly two arguments: `get(x)`"
      [ ([], symbol "f", [ symbol "x" ], R.(call ("get", [ var "x" ]))) ]
;;

let raw_program_errors_cases = [ "Tests", raw_program_errors ]

let sum_squares_rules : Raw_program.t =
    let open Raw_term in
    [ ( []
      , symbol "sum"
      , [ symbol "xs" ]
      , Match
          ( var "xs"
          , [ (symbol "Nil", []), int (i32 0)
            ; ( (symbol "Cons", [ symbol "x"; symbol "xs" ])
              , call ("+", [ var "x"; call ("sum", [ var "xs" ]) ]) )
            ] ) )
    ; ( []
      , symbol "mapSq"
      , [ symbol "xs" ]
      , Match
          ( var "xs"
          , [ (symbol "Nil", []), call ("Nil", [])
            ; ( (symbol "Cons", [ symbol "x"; symbol "xs" ])
              , call
                  ( "Cons"
                  , [ call ("*", [ var "x"; var "x" ]); call ("mapSq", [ var "xs" ]) ] ) )
            ] ) )
    ]
;;

let supercompile () =
    let open Raw_term in
    let main_rule =
        [], symbol "main", [ symbol "xs" ], call ("sum", [ call ("mapSq", [ var "xs" ]) ])
    in
    let expected =
        [ [], symbol "main", [ symbol "xs" ], call ("f0", [ var "xs" ])
        ; ( []
          , symbol "f0"
          , [ symbol "xs" ]
          , Match
              ( var "xs"
              , [ ( (symbol "Cons", [ symbol "x"; symbol "xs'" ])
                  , call
                      ( "+"
                      , [ call ("*", [ var "x"; var "x" ]); call ("f0", [ var "xs'" ]) ]
                      ) )
                ; (symbol "Nil", []), int (i32 0)
                ] ) )
        ]
    in
    Alcotest.(check' (module Raw_program))
      ~msg:"Supercompilation"
      ~actual:(Mazeppa.supercompile (main_rule :: sum_squares_rules))
      ~expected
;;

let supercompilation_cases = [ "Tests", supercompile ]

(* See <https://github.com/hirrolot/mazeppa/issues/28>. *)
let freshen_variables () =
    let open Raw_term in
    let make_program y =
        [ ( []
          , symbol "main"
          , [ symbol "x"; symbol "v0" ]
          , let'
              ( y
              , call ("+", [ var "x"; var "x" ])
              , call ("+", [ call ("*", [ var y; var y ]); var "v0" ]) ) )
        ]
    in
    Alcotest.(check' (module Raw_program))
      ~msg:"Freshen variables"
      ~actual:(Mazeppa.supercompile (make_program "y"))
      ~expected:(make_program "v0'")
;;

let freshen_variables_cases = [ "Tests", freshen_variables ]

let freshen_variables_in_rules () =
    let open Raw_term in
    let make_program (f, y) =
        [ [], symbol "main", [ symbol "x"; symbol "v0" ], call (f, [ var "v0"; var "x" ])
        ; ( []
          , symbol f
          , [ symbol "v0"; symbol "x" ]
          , let'
              ( y
              , call ("+", [ var "x"; var "x" ])
              , call (f, [ var "v0"; call ("*", [ var y; var y ]) ]) ) )
        ]
    in
    Alcotest.(check' (module Raw_program))
      ~msg:"Freshen variables in rules"
      ~actual:(Mazeppa.supercompile (make_program ("f", "y")))
      ~expected:(make_program ("f0", "v0'"))
;;

let freshen_variables_in_rules_cases = [ "Tests", freshen_variables_in_rules ]

let eval () =
    let open Raw_term in
    let check msg ~expected input =
        Alcotest.(check' (module Raw_term)) ~msg ~actual:(Mazeppa.eval input) ~expected
    in
    let input_list =
        list [ int (i32 0); int (i32 1); int (i32 2); int (i32 3); int (i32 4) ]
    in
    let main_rule =
        [], symbol "main", [], call ("sum", [ call ("mapSq", [ input_list ]) ])
    in
    check "Evaluation" ~expected:(int (i32 30)) (main_rule :: sum_squares_rules);
    check
      "Built-in panics"
      ~expected:(call ("Panic", [ string "out of range: `/(5i32, 0i32)`" ]))
      [ [], symbol "main", [], call ("/", [ int (i32 5); int (i32 0) ]) ];
    check
      "Custom panics"
      ~expected:(call ("Panic", [ int (i32 5) ]))
      [ ( []
        , symbol "main"
        , []
        , Let (symbol "x", call ("Panic", [ int (i32 5) ]), int (i32 100)) )
      ]
;;

let evaluation_cases = [ "Tests", eval ]

let eval_errors () =
    let open Raw_term in
    let check_rules ~expected input =
        Alcotest.check_raises "Evaluation errors" (Mazeppa.Panic expected) (fun () ->
          ignore (Mazeppa.eval input))
    in
    let check ~expected ?(rules = []) body =
        let input = ([], symbol "main", [], body) :: rules in
        check_rules ~expected input
    in
    check_rules
      ~expected:"The main function cannot accept parameters"
      [ [], symbol "main", [ symbol "x" ], var "x" ];
    check ~expected:"No such function `f`" (call ("f", []));
    check
      ~expected:"No such case `C`"
      (Match (call ("C", []), [ (symbol "C'", []), call ("f", []) ]));
    check
      ~expected:"Expected a constructor call: `5i32`"
      ~rules:[ [], symbol "f", [], int (i32 5) ]
      (Match (call ("f", []), []));
    check ~expected:"Expected a constant: `C()`" (call ("length", [ call ("C", []) ]));
    check
      ~expected:"Cannot reduce: `+(5i32, \"hello world\")`"
      (call ("+", [ int (i32 5); string "hello world" ]));
    check ~expected:"Unbound variable `x`" (var "x");
    check
      ~expected:"Unexpected argument list for `+`: `0i32`,`1i32`,`2i32`"
      (call ("+", [ int (i32 0); int (i32 1); int (i32 2) ]))
;;

let evaluation_errors_cases = [ "Tests", eval_errors ]

(* Adopted from <https://wiki.haskell.org/Haskell/Lazy_evaluation>. *)
let lazy_eval () =
    let open Raw_term in
    let rules =
        [ ( []
          , symbol "main"
          , []
          , call ("getIt", [ call ("magic", [ int (u32 1); int (u32 1) ]); int (u64 3) ])
          )
        ; ( []
          , symbol "magic"
          , [ symbol "m"; symbol "n" ]
          , Match
              ( call ("=", [ var "m"; int (u32 0) ])
              , [ (symbol "T", []), call ("Nil", [])
                ; ( (symbol "F", [])
                  , call
                      ( "Cons"
                      , [ var "m"
                        ; call ("magic", [ var "n"; call ("+", [ var "m"; var "n" ]) ])
                        ] ) )
                ] ) )
        ; ( []
          , symbol "getIt"
          , [ symbol "xs"; symbol "n" ]
          , Match
              ( var "xs"
              , [ (symbol "Nil", []), call ("Panic", [ string "undefined" ])
                ; ( (symbol "Cons", [ symbol "x"; symbol "xs" ])
                  , Match
                      ( call ("=", [ var "n"; int (u64 1) ])
                      , [ (symbol "T", []), var "x"
                        ; ( (symbol "F", [])
                          , call
                              ("getIt", [ var "xs"; call ("-", [ var "n"; int (u64 1) ]) ])
                          )
                        ] ) )
                ] ) )
        ]
    in
    Alcotest.(check' (module Raw_term))
      ~msg:"Lazy evaluation"
      ~actual:(Mazeppa.eval rules)
      ~expected:(int (u32 2))
;;

let lazy_evaluation_cases = [ "Tests", lazy_eval ]

let make_test_case (name, f) = Alcotest.test_case name `Quick f

let make_test (name, test_cases) = name, List.map make_test_case test_cases

let () =
    Alcotest.run
      "Mazeppa"
      ([ "Print constants", print_constants_cases
       ; "Term classification", term_classification_cases
       ; "Term redex signatures", term_redex_signature_cases
       ; "Term substitution", substitution_cases
       ; "Term instances", term_instances_cases
       ; "Term renamings", term_renamings_cases
       ; "MSG", msg_cases
       ; "Homeomorphic embedding", he_cases
       ; "Raw program errors", raw_program_errors_cases
       ; "Supercompilation", supercompilation_cases
       ; "Freshen variables", freshen_variables_cases
       ; "Freshen variables in rules", freshen_variables_in_rules_cases
       ; "Evaluation", evaluation_cases
       ; "Evaluation errors", evaluation_errors_cases
       ; "Lazy evaluation", lazy_evaluation_cases
       ]
       |> List.map make_test)
;;
