include module type of Types

val string_of_lam : lam -> string
val string_of_type : ty -> string
val print_lam : lam -> unit
val print_type : ty -> unit

val is_alpha_convertible : lam -> lam -> bool
val reduce : lam -> unit
val normal : lam -> lam
val betastep : lam -> lam option
val infer_type : env -> lam -> ty option
val typecheck : env -> lam -> ty -> bool
val fill : lam -> lam -> lam option
