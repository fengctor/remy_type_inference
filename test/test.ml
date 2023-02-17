open! Core
open Remy_type_inference.Type_inference
module LC = Remy_type_inference.Lambda_calculus
module IG = Remy_type_inference.Inefficient_generalization
module LBE = Remy_type_inference.Level_based_eager
open Expect_test_helpers_base

(* Simple expressions *)
let id = LC.Lam ("x", Var "x")
let e1 = LC.Lam ("x", Lam ("y", App (Var "x", Var "y")))

(* Unsound without environment check *)
let e2 = LC.Lam ("x", Let ("y", Var "x", Var "y"))
let e3 = LC.Lam ("x", Let ("y", Lam ("z", Var "x"), Var "y"))
let e4 = LC.Lam ("x", Let ("y", Lam ("z", App (Var "x", Var "z")), Var "y"))
let e5 = LC.Lam ("x", Lam ("y", Let ("x", App (Var "x", Var "y"), App (Var "x", Var "y"))))
let e6 = LC.Lam ("x", Let ("y", Let ("z", App (Var "x", id), Var "z"), Var "y"))

(* Fails occurs-check *)
let e7 = LC.Lam ("x", Let ("y", Var "x", App (Var "y", Var "y")))

let sexp_of_typeof (module M : TYPE_INFERENCE with type exp = LC.exp) exp =
  M.sexp_of_typ (M.typeof M.empty_env exp)
;;

let sexp_of_types_of_list_of_exp (module M : TYPE_INFERENCE with type exp = LC.exp) lst =
  sexp_of_list Fun.id (List.map ~f:(sexp_of_typeof (module M)) lst)
;;

let%expect_test "inefficient success correct" =
  print_s (sexp_of_types_of_list_of_exp (module IG) [ id; e1; e2; e3; e4; e5; e6 ]);
  [%expect
    {|
    ((TArrow
       (TVar (Unbound a))
       (TVar (Unbound a)))
     (TArrow
       (TArrow
         (TVar (Unbound b))
         (TVar (Unbound c)))
       (TArrow
         (TVar (Unbound b))
         (TVar (Unbound c))))
     (TArrow
       (TVar (Unbound a))
       (TVar (Unbound a)))
     (TArrow
       (TVar (Unbound a))
       (TArrow
         (TVar (Unbound c))
         (TVar (Unbound a))))
     (TArrow
       (TArrow
         (TVar (Unbound b))
         (TVar (Unbound c)))
       (TArrow
         (TVar (Unbound b))
         (TVar (Unbound c))))
     (TArrow
       (TArrow
         (TVar (Unbound b))
         (TArrow
           (TVar (Unbound b))
           (TVar (Unbound d))))
       (TArrow
         (TVar (Unbound b))
         (TVar (Unbound d))))
     (TArrow
       (TArrow
         (TArrow
           (TVar (Unbound b))
           (TVar (Unbound b)))
         (TVar (Unbound c)))
       (TVar (Unbound c)))) |}]
;;

let%expect_test "inefficient failure correct" =
  require_does_raise [%here] (fun () -> IG.typeof IG.empty_env e7);
  [%expect {| (Failure "Occurs check failed") |}]
;;

let%expect_test "level-based eager success correct" =
  print_s (sexp_of_types_of_list_of_exp (module LBE) [ id; e1; e2; e3; e4; e5; e6 ]);
  [%expect
    {|
    ((TArrow
       (TVar (Unbound a 1))
       (TVar (Unbound a 1)))
     (TArrow
       (TArrow
         (TVar (Unbound b 1))
         (TVar (Unbound c 1)))
       (TArrow
         (TVar (Unbound b 1))
         (TVar (Unbound c 1))))
     (TArrow
       (TVar (Unbound a 1))
       (TVar (Unbound a 1)))
     (TArrow
       (TVar (Unbound a 1))
       (TArrow
         (TVar (Unbound c 1))
         (TVar (Unbound a 1))))
     (TArrow
       (TArrow
         (TVar (Unbound b 1))
         (TVar (Unbound c 1)))
       (TArrow
         (TVar (Unbound b 1))
         (TVar (Unbound c 1))))
     (TArrow
       (TArrow
         (TVar (Unbound b 1))
         (TArrow
           (TVar (Unbound b 1))
           (TVar (Unbound d 1))))
       (TArrow
         (TVar (Unbound b 1))
         (TVar (Unbound d 1))))
     (TArrow
       (TArrow
         (TArrow
           (TVar (Unbound b 1))
           (TVar (Unbound b 1)))
         (TVar (Unbound c 1)))
       (TVar (Unbound c 1)))) |}]
;;

let%expect_test "level-based failure correct" =
  require_does_raise [%here] (fun () -> LBE.typeof LBE.empty_env e7);
  [%expect {| (Failure "Occurs check failed: 'a") |}]
;;
