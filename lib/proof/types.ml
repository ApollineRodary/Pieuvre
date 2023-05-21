open Lambda

(* A type (what to prove) and the associated hypotheses *)
type goal = (env * ty)

(* A lambda-term corresponding to the advancement of the proof thus far, and a list of goals left to prove *)
type proof = lam * (goal list)

type tactic = (goal list) -> (lam * goal list)

type command =
    | Qed
    | Admitted
    | UseTactic of tactic * string
    | Print

exception No_Goals_Left
exception Cannot_Apply_Tactic