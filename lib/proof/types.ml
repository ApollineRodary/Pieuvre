open Lambda

(* A type (what to prove) and the associated hypotheses *)
type goal = (env * ty)

(* A lambda-term corresponding to the advancement of the proof thus far, and a list of goals left to prove *)
type proof = lam * (goal list)

(* type tactic =
| Assumption
| Exact of lam
| Intro of var
| Intros of var list
| Admit
| Qed
| Admitted *)

type tactic = (goal list) -> (lam * goal list)

exception Proven
exception Proof_Admitted
exception No_Goals_Left
exception Incomplete_Proof
exception Cannot_Apply_Tactic
