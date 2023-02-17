open! Core
open! Remy_type_inference.Parser
open! Remy_type_inference.Lexer
open Remy_type_inference.Type_inference
module LC = Remy_type_inference.Lambda_calculus
module IG = Remy_type_inference.Inefficient_generalization
module LBE = Remy_type_inference.Level_based_eager

let pretty_exp_with_typs inference_mods exp =
  let exp_pretty = LC.pretty_exp exp in
  let typ_descriptions =
    inference_mods
    |> List.map ~f:(fun m ->
           let module M = (val m : TYPE_INFERENCE with type exp = LC.exp) in
           let start_time = Time.now () in
           let typ_pretty =
             try M.pretty_typ (M.typeof M.empty_env exp) with
             | Failure msg -> "Type inference failed; " ^ msg
           in
           let end_time = Time.now () in
           let ellapsed_time_us = Time.diff end_time start_time |> Time.Span.to_us in
           String.concat
             [ M.inference_strategy
             ; ":\n"
             ; typ_pretty
             ; " ("
             ; Float.to_string ellapsed_time_us
             ; "us)"
             ])
  in
  String.concat ~sep:"\n" (exp_pretty :: typ_descriptions)
;;

let parse_exp_exn input =
  let open Lexing in
  let print_position outx lexbuf =
    let pos = lexbuf.lex_curr_p in
    fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
  in
  let buffer = Lexing.from_string input in
  try prog read buffer with
  | SyntaxError errmsg ->
    fprintf stderr "%a: %s\n" print_position buffer errmsg;
    Out_channel.flush stderr;
    failwith ""
  | Error ->
    fprintf stderr "%a: syntax error\n" print_position buffer;
    Out_channel.flush stderr;
    failwith ""
;;

let rec repl () =
  (try
     print_string "> ";
     Out_channel.flush stdout;
     let input = In_channel.(input_line_exn stdin) in
     let exp = parse_exp_exn input in
     pretty_exp_with_typs [ (module IG); (module LBE) ] exp |> print_endline;
     IG.reset_gensym ();
     LBE.reset_gensym ()
   with
  | _ -> ());
  repl ()
;;

let () = repl ()
