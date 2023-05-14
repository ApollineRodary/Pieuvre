open Lambda
open Types

let rec assoc_reverse_opt (l,x : ('a*'b) list * 'b) : 'a option =
    match l with
    | [] -> None
    | (a, b)::_ when b = x -> Some a
    | _::q -> assoc_reverse_opt (q, x)

let assumption (gs : goal list) : (lam * goal list) =
    match gs with
    | [] -> raise No_Goals_Left
    | g::gs ->
        begin
            match assoc_reverse_opt g with
            | Some x -> (Var x, gs)
            | None -> raise Cannot_Apply_Tactic
        end

let exact (m : lam) (gs: goal list) : (lam * goal list) =
    match gs with
    | [] -> raise No_Goals_Left
    | (gam, a)::gs ->
        if typecheck gam m a then (m, gs)
        else raise Cannot_Apply_Tactic

let intro (x : var) (gs : goal list) : (lam * goal list) =
    match gs with
    | [] -> raise No_Goals_Left
    | (gam, Arrow(a, b))::gs ->
        Abstraction (x, a, Hole),
        ((x, a)::gam, b)::gs
    | _ -> raise Cannot_Apply_Tactic

let intros (l : var list) (gs : goal list) : (lam * goal list) =
    let rec aux (l : var list) (gs : goal list) (m : lam) =
        match l with
        | [] -> (m, gs)
        | x::xs ->
            let (n, gs) = intro x gs in
            aux xs gs (Option.get (fill m n))
    in aux l gs Hole

let apply (x : var) (gs : goal list) : (lam * goal list) =
    let rec aux (goal_type : ty) (gam : env) (m : lam) (t : ty) (gs : goal list) : (lam * goal list) = 
        if t = goal_type then (m, gs) else
        match t with
        | Arrow (a, b) -> aux goal_type gam (Application (m, Hole)) b ((gam, a)::gs)
        | _ -> raise Cannot_Apply_Tactic
    in
    match gs with
    | [] -> raise No_Goals_Left
    | (gam, a)::gs -> begin
        match List.assoc_opt x gam with
        | None -> raise Cannot_Apply_Tactic
        | Some h -> 
            let (m, gs') = aux a gam (Var x) h [] in
            (m, (List.rev gs') @ gs)
        end

let cut (t : ty) (gs : goal list) : (lam * goal list) = match gs with
    | [] -> raise No_Goals_Left
    | (gam, a)::gs -> (Application (Hole, Hole), (gam, Arrow(t, a))::(gam, t)::gs)

let admit (gs : goal list) : (lam * goal list) =
    match gs with
    | [] -> raise No_Goals_Left
    | _::xs -> Hole, xs

let admitted (gs : goal list) : (lam * goal list) =
    match gs with
    | [] -> raise Proof_Admitted
    | _ -> raise Incomplete_Proof

let qed (gs : goal list) : (lam * goal list) =
    match gs with
    | [] -> raise Proven
    | _ -> raise Incomplete_Proof

let use_tactic (t : tactic) ((l, gs): proof) : proof =
    let (m, gs) = t gs in 
    (Option.get (fill l m), gs)
