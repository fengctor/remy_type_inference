open! Core
open! Remy_type_inference.Parser
open! Remy_type_inference.Lexer
open Remy_type_inference.Type_inference
module LC = Remy_type_inference.Lambda_calculus
module IG = Remy_type_inference.Inefficient_generalization
module LBE = Remy_type_inference.Level_based_eager

(* Simple expressions *)
let id = LC.Lam ("x", Var "x")
let e1 = LC.Lam ("x", Lam ("y", App (Var "x", Var "y")))

(* Unsound without environment check *)
let e2 = LC.Lam ("x", Let ("y", Var "x", Var "y"))
let e3 = LC.Lam ("x", Let ("y", Lam ("z", Var "x"), Var "y"))
let e4 = LC.Lam ("x", Let ("y", Lam ("z", App (Var "x", Var "z")), Var "y"))
let e5 = LC.Lam ("x", Lam ("y", Let ("x", App (Var "x", Var "y"), App (Var "x", Var "y"))))
let e6 = LC.Lam ("x", Let ("y", Var "x", App (Var "y", Var "y")))
let e7 = LC.Lam ("x", Let ("y", Let ("z", App (Var "x", id), Var "z"), Var "y"))

(* Expressions to test *)
let test_exps = [ id; e1; e2; e3; e4; e5; e6; e7 ]

let pretty_exp_with_typ (module M : TYPE_INFERENCE with type exp = LC.exp) exp =
  let start_time = Time.now () in
  let typ_pretty =
    try M.pretty_typ (M.typeof M.empty_env exp) with
    | Failure msg -> "Type inference failed; " ^ msg
  in
  let end_time = Time.now () in
  let ellapsed_time_us = Time.diff end_time start_time |> Time.Span.to_us in
  String.concat
    [ LC.pretty_exp exp
    ; " : "
    ; typ_pretty
    ; " ("
    ; Float.to_string ellapsed_time_us
    ; "us)"
    ]
;;

(* TODO: move the above into tests module *)

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
     print_endline "Inefficient generalization:";
     pretty_exp_with_typ (module IG) exp |> print_endline;
     IG.reset_gensym ();
     print_endline "Level-based eager:";
     pretty_exp_with_typ (module LBE) exp |> print_endline;
     LBE.reset_gensym ()
   with
  | _ -> ());
  repl ()
;;

let () = repl ()
