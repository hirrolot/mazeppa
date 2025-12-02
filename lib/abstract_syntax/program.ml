[@@@coverage exclude_file]

open Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Symbol_map = Map.Make (Symbol)
module F_rules = Symbol_map
module G_rules_by_pattern = Symbol_map
module G_rules_by_name = Symbol_map

type param_list = Symbol.t list [@@deriving show]

type f_rule = param_list * Term.t [@@deriving show]

type f_rules_by_name = f_rule F_rules.t

type g_rule = param_list * param_list * Term.t [@@deriving show]

type g_rules_by_pattern = g_rule G_rules_by_pattern.t

type g_rules_by_name = g_rules_by_pattern G_rules_by_name.t

type t =
  { f_rules : f_rules_by_name
  ; g_rules : g_rules_by_name
  ; extract_f_rules : Symbol_set.t
  ; productive_g_rules : bool list Symbol_map.t
  ; unproductive_g_rules : Symbol_set.t
  }

let find_f_rule ~program op : f_rule =
    try Symbol_map.find op program.f_rules with
    | Not_found -> Util.panic "No such f-function %s" (Symbol.verbatim op)
;;

let find_g_rule_list ~program op : g_rules_by_pattern =
    try Symbol_map.find op program.g_rules with
    | Not_found -> Util.panic "No such g-function %s" (Symbol.verbatim op)
;;

let find_productive_g_rule_list ~program op : bool list =
    try Symbol_map.find op program.productive_g_rules with
    | Not_found -> Util.panic "No such g-function %s" (Symbol.verbatim op)
;;

let find_g_rule ~program (op, c) : g_rule option =
    Symbol_map.find_opt c (find_g_rule_list ~program op)
;;

let output ~oc ({ f_rules; g_rules; _ } : t) =
    let module Printable = struct
      type t = f_rules_list * g_rules_list

      and f_rules_list = (Symbol.t * Symbol.t list * string) list

      and g_rules_list =
        (Symbol.t * (Symbol.t * Symbol.t list * Symbol.t list * string) list) list
      [@@deriving yojson_of]
    end
    in
    let f_rules =
        f_rules
        |> F_rules.bindings
        |> List.map (fun (f, (params, body)) -> f, params, Term.to_string body)
    in
    let g_rules =
        g_rules
        |> G_rules_by_name.bindings
        |> List.map (fun (g, rules) ->
          ( g
          , rules
            |> G_rules_by_pattern.bindings
            |> List.map (fun (c, (c_params, params, body)) ->
              c, c_params, params, Term.to_string body) ))
    in
    let json : Yojson.Safe.t = Printable.yojson_of_t (f_rules, g_rules) in
    Yojson.Safe.pretty_to_channel (Lazy.force oc) json
;;
