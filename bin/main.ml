open Mazeppa
open Mazeppa.Internals
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let github_repo = "https://github.com/hirrolot/mazeppa"

let version = "0.7.2"

let tab = "    "

let main_symbol = Symbol.of_string "main"

let position_to_string ({ pos_fname; pos_lnum; pos_bol; pos_cnum } : Lexing.position) =
    Spectrum.Simple.sprintf
      "@{<bold>%s:%d:%d@}"
      pos_fname
      pos_lnum
      (pos_cnum - pos_bol + 1)
;;

let parse lexbuf =
    try Parser.program Lexer.read lexbuf with
    | Lexer.SyntaxError msg ->
      Util.panic "%s: %s" (position_to_string lexbuf.lex_curr_p) msg
    | Parser.Error -> Util.panic "%s: syntax error" (position_to_string lexbuf.lex_curr_p)
;;

let read_channel ic = parse (Lexing.from_channel ic)

let read_file filename =
    In_channel.with_open_text filename (fun ic ->
      let lexbuf = Lexing.from_channel ic in
      Lexing.set_filename lexbuf filename;
      try Parser.program Lexer.read lexbuf with
      | Lexer.SyntaxError msg ->
        Util.panic "%s: %s" (position_to_string lexbuf.lex_curr_p) msg
      | Parser.Error ->
        Util.panic "%s: syntax error" (position_to_string lexbuf.lex_curr_p))
;;

module Make_nodes (S : sig end) : sig
  val memoize : Symbol.t * Term.t -> unit

  val unmemoize : Symbol.t -> unit

  val output : oc:out_channel Lazy.t -> unit -> unit
end = struct
  let history = Hashtbl.create 1024

  let memoize (id, node) =
      let label, contents = Symbol.to_string id, Term.to_string node in
      Hashtbl.add history label contents
  ;;

  let unmemoize id = Hashtbl.remove history (Symbol.to_string id)

  let output ~oc () =
      let module Printable = struct
        type t = (string * string) list [@@deriving yojson_of]
      end
      in
      List.of_seq (Hashtbl.to_seq history)
      |> List.sort (fun (l1, _) (l2, _) ->
        let i1, i2 = Scanf.(sscanf l1 "n%d" Fun.id, sscanf l2 "n%d" Fun.id) in
        Int.compare i1 i2)
      |> Printable.yojson_of_t
      |> Yojson.Safe.pretty_to_channel (Lazy.force oc)
  ;;
end

type channels =
  { (* For printing the core program representation. *)
    program_oc : out_channel Lazy.t option
  ; (* For printing the overall graph structure (without term contents). *)
    graph_oc : out_channel Lazy.t option
  ; (* For printing the term contents of corresponding graph nodes. *)
    nodes_oc : out_channel Lazy.t option
  ; (* For printing the residual program. *)
    output_oc : out_channel Lazy.t
  }

let supercompile ~(channels : channels) (input : Raw_program.t) : unit =
    let program = Converter.to_program input in
    (match channels.program_oc with
     | Some oc -> Program.output ~oc program
     | None -> ());
    let module Nodes = Make_nodes (struct end) in
    let module Supervisor_inst =
      Supervisor.Make (struct
        let program = program

        let inspect = Option.is_some channels.program_oc

        let observe_node =
            match channels.nodes_oc with
            | Some _oc -> Nodes.memoize
            | None -> ignore
        ;;

        let unobserve_node = Nodes.unmemoize
      end)
    in
    let main_params, _ = Program.find_f_rule ~program main_symbol in
    let t = Term.(Call (main_symbol, var_list main_params)) in
    let graph = Supervisor_inst.run t in
    (match channels.graph_oc with
     | Some oc -> Visualizer.run ~oc graph
     | None -> ());
    (match channels.nodes_oc with
     | Some oc -> Nodes.output ~oc ()
     | None -> ());
    let t_res, program_res = Residualizer.run ~unknowns:main_params graph in
    let residue = ([], main_symbol, main_params, t_res) :: program_res in
    Pretty.print_program ~oc:channels.output_oc residue
;;

type run_config =
  { target_dir : string
  ; inspect : bool
  }

let prepare_target_dir (conf : run_config) : unit =
    if Sys.file_exists conf.target_dir
    then
      if not (Sys.is_directory conf.target_dir)
      then Util.panic "`%s` is not a directory" conf.target_dir
      else ()
    else Sys.mkdir conf.target_dir 0o775
;;

let ( let$ ) filename k =
    let oc = lazy (Out_channel.open_text filename) in
    Fun.protect
      ~finally:(fun () ->
        if Lazy.is_val oc
        then (
          Out_channel.output_string (Lazy.force oc) "\n";
          Stdlib.close_out_noerr (Lazy.force oc)))
      (fun () -> k oc)
;;

let supercompile (conf : run_config) : unit =
    let input = read_file "main.mz" in
    prepare_target_dir conf;
    let$ output_oc = conf.target_dir ^ "/output.mz" in
    if conf.inspect
    then
      let$ program_oc = conf.target_dir ^ "/program.json" in
      let$ graph_oc = conf.target_dir ^ "/graph.dot" in
      let$ nodes_oc = conf.target_dir ^ "/nodes.json" in
      supercompile
        ~channels:
          { program_oc = Some program_oc
          ; graph_oc = Some graph_oc
          ; nodes_oc = Some nodes_oc
          ; output_oc
          }
        input
    else
      supercompile
        ~channels:{ program_oc = None; graph_oc = None; nodes_oc = None; output_oc }
        input
;;

type translate_config =
  { language : string
  ; entry : string
  ; dump_header_to : string option
  }

let c_identifier x =
    if
      Str.(
        string_match (regexp "[a-zA-Z_][a-zA-Z0-9_]*") x 0
        &&
        (* Ensure that the whole string has been matched. *)
        matched_string x = x)
    then Symbol.of_string x
    else Util.panic "Invalid entry name: `%s`" x
;;

let translate (conf : translate_config) : unit =
    let input = read_channel stdin in
    Mazeppa.check input;
    match String.lowercase_ascii conf.language with
    | "c" ->
      Mazeppa.translate_to_c ~oc:stdout ~entry:(c_identifier conf.entry) input;
      conf.dump_header_to
      |> Option.iter (fun dir ->
        Out_channel.with_open_text (dir ^ "/mazeppa.h") Mazeppa.mazeppa_h)
    | _ -> Util.panic "Unsupported target language: `%s`" conf.language
;;

module Options = struct
  let get_target_dir () =
      Clap.default_string
        ~long:"target-dir"
        ~description:"The target directory to use."
        "target"
  ;;

  let get_inspect () =
      Clap.flag
        ~set_long:"inspect"
        ~description:
          "Inspect the work of the supercompiler. The resulting files will be in the \
           target directory."
        false
  ;;

  let get_language () =
      Clap.mandatory_string
        ~long:"language"
        ~description:
          "The target language for translation (case-insensitive). The only supported \
           value is `C`, which corresponds to `-std=gnu11`. Both GCC and Clang should \
           accept the output without issues."
        ()
  ;;

  let get_entry () =
      Clap.mandatory_string
        ~long:"entry"
        ~description:
          "If `--language C` is specified, the name of an `extern` function that will \
           correspond to your main function."
        ()
  ;;

  let get_dump_header_to () =
      Clap.optional_string
        ~long:"dump-header-to"
        ~description:
          "The directory to dump `mazeppa.h` to. This header is required to compile the \
           generated C code."
        ()
  ;;
end

let get_command () =
    let open Options in
    [ (Clap.case "run" ~description:"Run the supercompiler."
       @@ fun () ->
       let target_dir = get_target_dir () in
       let inspect = get_inspect () in
       fun () -> supercompile { target_dir; inspect })
    ; (Clap.case "translate" ~description:"Translate a program to some target language."
       @@ fun () ->
       let language = get_language () in
       let entry = get_entry () in
       let dump_header_to = get_dump_header_to () in
       fun () -> translate { language; entry; dump_header_to })
    ; (Clap.case "check" ~description:"Check a program for errors."
       @@ fun () () ->
       let input = read_file "main.mz" in
       Mazeppa.check input)
    ; (Clap.case
         "eval"
         ~description:
           "Evaluate a program and print the result. The main function must not accept \
            parameters."
       @@ fun () ->
       fun () ->
       let input = read_file "main.mz" in
       Mazeppa.check input;
       print_endline (Raw_term.to_string (Mazeppa.eval input)))
    ]
    |> Clap.subcommand
;;

let cut_list list =
    let array = Array.of_list list in
    let length = Array.length array in
    if length <= 20
    then list
    else (
      let first_10, last_10 = Array.(make 10 "", make 10 "") in
      Array.blit array 0 first_10 0 10;
      Array.blit array (length - 10) last_10 0 10;
      let middle =
          Spectrum.Simple.sprintf "@{<italic,underline>(%d more...)@}" (length - 20)
      in
      Array.(to_list first_10 @ [ middle ] @ to_list last_10))
;;

let its_over ?(reduction_path = []) msg =
    Spectrum.Simple.eprintf "@{<bold,red>error:@} %s\n" msg;
    (match reduction_path with
     | [] -> ()
     | _ ->
       Spectrum.Simple.eprintf
         "%s@{<bold,aqua>note:@} While reducing %s\n"
         tab
         (String.concat " -> " (cut_list reduction_path)));
    exit 1
;;

let () =
    Printexc.record_backtrace true;
    Clap.description (Printf.sprintf "The Mazeppa supercompiler (v%s)." version);
    let command = get_command () in
    Clap.close ();
    try command () with
    | Util.Panic { msg; reduction_path } -> its_over ~reduction_path msg
    | Mazeppa.Panic msg | Sys_error msg -> its_over msg
    | e ->
      let description, backtrace = Printexc.(to_string_default e, get_backtrace ()) in
      Spectrum.Simple.eprintf
        "@{<bold,red>internal compiler error:@} %s\n\n\
         @{<bold>Please, file an issue in <%s/issues>. Include the compiler's version \
         and a Minimal Reproducible Example (MRE).@}\n\n\
         %s\n"
        description
        github_repo
        (* Since it is unspecified whether the backtrace contains leading/trailing
           whitespace, it is safer to trim it. *)
        (String.trim backtrace);
      exit 1
;;
