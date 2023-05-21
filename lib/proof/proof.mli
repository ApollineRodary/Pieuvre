include module type of Types

val proof_start : Lambda.ty -> proof
val use_tactic : tactic -> proof -> proof

val absurd : Lambda.ty -> goal list -> Lambda.lam * goal list
val admit : (goal list) -> (Lambda.lam * goal list)
val apply : Lambda.var -> goal list -> Lambda.lam * goal list
val assumption : (goal list) -> (Lambda.lam * goal list)
val cut : Lambda.ty -> goal list -> Lambda.lam * goal list
val elim : Lambda.var -> goal list -> Lambda.lam * goal list
val exact : Lambda.lam -> (goal list) -> (Lambda.lam * goal list)
val exfalso : goal list -> Lambda.lam * goal list
val intro : Lambda.var -> (goal list) -> (Lambda.lam * goal list)
val intros : (Lambda.var list) -> (goal list) -> (Lambda.lam * goal list)
val left : goal list -> (Lambda.lam * goal list)
val right : goal list -> (Lambda.lam * goal list)
val split : (goal list) -> (Lambda.lam * goal list)
