open! Core

type varname = string [@@deriving sexp, eq]

(* Expressions *)
type exp =
  | Var of varname
  | App of exp * exp
  | Lam of varname * exp
  | Let of varname * exp * exp
[@@deriving sexp, eq]

let rec pretty_exp = function
  | Var name -> name
  | App (e1, e2) -> String.concat [ "("; pretty_exp e1; " "; pretty_exp e2; ")" ]
  | Lam (v, e) -> String.concat [ "(λ "; v; ". "; pretty_exp e; ")" ]
  | Let (v, e, e2) ->
    String.concat [ "(let "; v; " = "; pretty_exp e; " in "; pretty_exp e2; ")" ]
;;
