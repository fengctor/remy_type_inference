open Type_inference
module LC = Lambda_calculus

type qname = string [@@deriving sexp, eq]

include TYPE_INFERENCE with type exp = LC.exp

val reset_gensym : unit -> unit
