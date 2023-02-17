module type TYPE_INFERENCE = sig
  type env
  type exp
  type typ

  val inference_strategy : string
  val empty_env : env
  val typeof : env -> exp -> typ
  val pretty_typ : typ -> string
end
