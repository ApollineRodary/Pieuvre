include Types

let proof_start (a : Lambda.ty) : proof = (Hole, [([], a)])
let use_tactic = Tactics.use_tactic

let absurd = Tactics.absurd
let admit = Tactics.admit
let apply = Tactics.apply
let assumption = Tactics.assumption
let cut = Tactics.cut
let elim = Tactics.elim
let exact = Tactics.exact
let exfalso = Tactics.exfalso
let intro = Tactics.intro
let intros = Tactics.intros
let left = Tactics.left
let right = Tactics.right
let split = Tactics.split