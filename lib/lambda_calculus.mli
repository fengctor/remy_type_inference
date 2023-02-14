open! Core

type varname = string [@@deriving sexp, eq]

type exp =
  | Var of varname
  | App of exp * exp
  | Lam of varname * exp
  | Let of varname * exp * exp
[@@deriving sexp, eq]

val pretty_exp : exp -> string
