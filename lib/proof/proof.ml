include Types

let start_proof (a : Lambda.ty) : proof = (Hole, [([], a)])
let use_tactic = Tactics.use_tactic

let exact = Tactics.exact
let assumption = Tactics.assumption
let intro = Tactics.intro
let intros = Tactics.intros
let apply = Tactics.apply
let cut = Tactics.cut
let exfalso = Tactics.exfalso
let elim = Tactics.elim
let absurd = Tactics.absurd
let admit = Tactics.admit
let qed = Tactics.qed
let admitted = Tactics.admitted
