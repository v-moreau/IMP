(* File main.ml *)

open Imp

(* cf. https://ocaml.org/docs/cli-arguments *)
let filename = ref ""
let input = ref 0
let anon_fun arg = if !filename = "" then filename := arg else input := int_of_string arg
let verbose = ref false
let speclist = [
  ("--verbose", Arg.Set verbose, "Output debug information");
]
let usage_msg = "imp [--verbose] <imp file> [input]"
let () = Arg.parse speclist anon_fun usage_msg


let () =
  let com = open_in !filename |>
  Lexing.from_channel |>
  Parser.main Lexer.token in
  (* print_endline (if !verbose then "true" else "false"); *)
  if !verbose then (
    let paragraph s = print_endline ("--- " ^ s ^ " ---") in
    paragraph "Input program";
    print_endline (Syntax.string_of_com com);
    paragraph "Content of 'out'"
  ) else ();
  let result = Syntax.compute com !input in
  print_int result; print_newline ();
  flush stdout
