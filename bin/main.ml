open! Core
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
  let typ_pretty =
    try M.pretty_typ (M.typeof M.empty_env exp) with
    | Failure msg -> "Type inference failed; " ^ msg
  in
  String.concat [ LC.pretty_exp exp; " : "; typ_pretty ]
;;

let () =
  print_endline "Inefficient generalization:";
  List.iter
    ~f:(fun exp ->
      pretty_exp_with_typ (module IG) exp |> print_endline;
      IG.reset_gensym ())
    test_exps;
  print_endline "Level-based eager:";
  List.iter
    ~f:(fun exp ->
      pretty_exp_with_typ (module LBE) exp |> print_endline;
      LBE.reset_gensym ())
    test_exps
;;
