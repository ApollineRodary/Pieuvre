include module type of Types

val is_alpha_convertible: lam -> lam -> bool
val reduce: lam -> unit
val betastep: lam -> lam option
val infer_type: (var*ty) list -> lam -> ty option
val typecheck: (var*ty) list -> lam -> ty -> bool
val print_lam: lam -> unit
val print_type: ty -> unit