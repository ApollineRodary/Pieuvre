include module type of Types

val start_proof : Lambda.ty -> proof
val use_tactic : tactic -> proof -> proof

val exact : Lambda.lam -> (goal list) -> (Lambda.lam * goal list)
val assumption : (goal list) -> (Lambda.lam * goal list)
val intro : Lambda.var -> (goal list) -> (Lambda.lam * goal list)
val intros : (Lambda.var list) -> (goal list) -> (Lambda.lam * goal list)
val apply : Lambda.var -> goal list -> Lambda.lam * goal list
val cut : Lambda.ty -> goal list -> Lambda.lam * goal list
val exfalso : Lambda.var -> goal list -> Lambda.lam * goal list
val elim : Lambda.lam -> goal list -> Lambda.lam * goal list
val absurd : Lambda.ty -> goal list -> Lambda.lam * goal list
val admit : (goal list) -> (Lambda.lam * goal list)
val qed : (goal list) -> (Lambda.lam * goal list)
val admitted : (goal list) -> (Lambda.lam * goal list)
