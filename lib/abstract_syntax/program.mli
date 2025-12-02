(* An f-function (also called "indifferent") is a function that just unfolds to its body;
   a g-function (called "curious") is a function that pattern-matches on its first
   parameter and has several unfolding rules for each pattern alternative. This is a
   standard terminology in the supercompilation literature, which was originally
   introduced for SLL (Simply Lazy Language). *)

module Symbol_map : module type of Map.Make (Symbol)

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

val find_f_rule : program:t -> Symbol.t -> f_rule

val find_g_rule_list : program:t -> Symbol.t -> g_rules_by_pattern

val find_productive_g_rule_list : program:t -> Symbol.t -> bool list

val find_g_rule : program:t -> Symbol.t * Symbol.t -> g_rule option

val output : oc:out_channel Lazy.t -> t -> unit
