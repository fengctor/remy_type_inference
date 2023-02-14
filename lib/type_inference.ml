module type TYPE_INFERENCE = sig
  type env
  type exp
  type typ

  val empty_env : env
  val typeof : env -> exp -> typ
  val pretty_typ : typ -> string
end
