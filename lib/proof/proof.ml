include Types

let start_proof (a : Lambda.ty) : proof = (Hole, [([], a)])
let use_tactic = Tactics.use_tactic