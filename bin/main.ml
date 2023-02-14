open! Core
module IG = Remy_type_inference.Inefficient_generalization
module LC = Remy_type_inference.Lambda_calculus

(* Simple expressions *)
let id = LC.Lam ("x", Var "x")
let e0 = LC.Lam ("x", Lam ("y", App (Var "x", Var "y")))

(* Unsound without environment check *)
let e1 = LC.Lam ("x", Let ("y", Var "x", Var "y"))
let e2 = LC.Lam ("x", Let ("y", Var "x", Var "y"))
let e3 = LC.Lam ("x", Let ("y", Lam ("z", Var "x"), Var "y"))
let e4 = LC.Lam ("x", Let ("y", Lam ("z", App (Var "x", Var "z")), Var "y"))
let e5 = LC.Lam ("x", Lam ("y", Let ("x", App (Var "x", Var "y"), App (Var "x", Var "y"))))
let e6 = LC.Lam ("x", Let ("y", Var "x", App (Var "y", Var "y")))
let e7 = LC.Lam ("x", Let ("y", Let ("z", App (Var "x", id), Var "z"), Var "y"))

(* Expressions to test *)
let test_exps = [ id; e0; e1; e2; e3; e4; e5; e6; e7 ]

let pretty_exp_with_typ exp =
  let typ_pretty =
    try IG.pretty_typ (IG.typeof IG.empty_env exp) with
    | Failure msg -> "Type inference failed; " ^ msg
  in
  String.concat [ LC.pretty_exp exp; " : "; typ_pretty ]
;;

let () =
  List.iter
    ~f:(fun exp ->
      pretty_exp_with_typ exp |> print_endline;
      IG.reset_gensym ())
    test_exps
;;
