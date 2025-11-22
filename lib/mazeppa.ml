module Checked_oint = Checked_oint
module Symbol = Symbol
module Const = Const
module Raw_term = Raw_term
module Raw_program = Raw_program
module Gensym = Gensym

exception Panic of string

let wrap_panic f =
    try f () with
    | Util.Panic { msg; reduction_path = _ } -> raise (Panic msg)
;;

let supercompile (input : Raw_program.t) : Raw_program.t =
    wrap_panic (fun () ->
      let program = Converter.to_program input in
      let module Supervisor =
        Supervisor.MakeSimple (struct
          let program = program
        end)
      in
      let main_symbol = Symbol.of_string "main" in
      let main_params, _ = Program.find_f_rule ~program main_symbol in
      let t = Term.(Call (main_symbol, var_list main_params)) in
      let graph = Supervisor.run t in
      let t_res, program_res = Residualizer.run ~unknowns:main_params graph in
      ([], main_symbol, main_params, t_res) :: program_res)
;;

let check (input : Raw_program.t) : unit =
    wrap_panic (fun () -> ignore (Converter.to_program input))
;;

let eval (input : Raw_program.t) : Raw_term.t =
    wrap_panic (fun () -> Evaluator.run_exn input)
;;

let translate_to_c ~(oc : out_channel) ~(entry : Symbol.t) (input : Raw_program.t) : unit =
    wrap_panic (fun () -> C_codegen.run ~oc ~entry input)
[@@coverage off]
;;

let mazeppa_h (oc : out_channel) : unit =
    Out_channel.output_string oc [%blob "../c/mazeppa.h"]
[@@coverage off]
;;

(* The modules must be sorted in the alphabetic order. *)
module Internals = struct
  module Converter = Converter
  module Homeomorphic_emb = Homeomorphic_emb
  module Lexer = Lexer
  module Msg = Msg
  module Parser = Parser
  module Pretty = Pretty
  module Program = Program
  module Residualizer = Residualizer
  module Subst = Subst
  module Supervisor = Supervisor
  module Symbol_map = Symbol_map
  module Term = Term
  module Util = Util
  module Visualizer = Visualizer
end
