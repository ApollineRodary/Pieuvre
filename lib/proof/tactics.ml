open Lambda
open Types

let rec assoc_reverse_opt (l,x : ('a*'b) list * 'b) : 'a option =
    match l with
    | [] -> None
    | (a, b)::_ when b = x -> Some a
    | _::q -> assoc_reverse_opt (q, x)

let absurd (t : ty) (gs : goal list) : (lam * goal list) = match gs with
    | [] -> raise No_Goals_Left
    | (gam, a)::gs -> (Exf(Application (Hole, Hole), a), (gam, Arrow (t, False))::(gam, t)::gs)

let admit (gs : goal list) : (lam * goal list) =
    match gs with
    | [] -> raise No_Goals_Left
    | _::gs -> Hole, gs

let apply (x : var) (gs : goal list) : (lam * goal list) =
    let rec aux (t_arr : ty) (t_goal : ty) (env : env) (m : lam)  (gs : goal list) : (lam * goal list) = 
        (*Takes a chain of arrows (e.g. A->B->C->D) ending with a goal type and returns goals for each of the intermediate steps*)
        match t_arr with
        | t when t = t_goal -> (m, gs)
        | Arrow (t1, t2) ->
            let lam, gs = aux t2 t_goal env (Application (m, Hole)) gs in
            (lam, (env, t1)::gs)
        | _ -> raise Cannot_Apply_Tactic
        in
    match gs with
    | [] -> raise No_Goals_Left
    | (env, ty)::gs ->
        begin
            try
                let t = List.assoc x env in
                aux t ty env (Var x) gs
            with Not_found -> raise Cannot_Apply_Tactic
        end

let assumption (gs : goal list) : (lam * goal list) =
    match gs with
    | [] -> raise No_Goals_Left
    | g::gs ->
        begin
            match assoc_reverse_opt g with
            | Some x -> (Var x, gs)
            | None -> raise Cannot_Apply_Tactic
        end

let cut (t : ty) (gs : goal list) : (lam * goal list) =
    match gs with
    | [] -> raise No_Goals_Left
    | (env, ty)::gs -> 
        let lam = Application (Hole, Hole)
        and gs = (env, Arrow (t, ty)) :: (env, t) :: gs in 
        (lam, gs)

let elim (x : var) (gs : goal list) : (lam * goal list) =
    match gs with
    | [] -> raise No_Goals_Left
    | (env, ty)::gs -> begin
        try
            match List.assoc x env with 
            | False -> (Exf (Var x, ty), gs)
            | And (a, b) ->
                let appl = Application (Application (Hole, Fst (Var x)), Snd (Var x)) in
                let ty = Arrow (a, Arrow (b, ty)) in
                appl, (env, ty)::gs
            | Or (a, b) ->
                let lam = Case (Var x, Hole, Hole)
                and goal_a = (env, Arrow (a, ty))
                and goal_b = (env, Arrow (b, ty)) in
                (lam, goal_a::goal_b::gs)
            | _ -> raise Cannot_Apply_Tactic
        with Not_found -> raise Cannot_Apply_Tactic
    end

let exact (m : lam) (gs : goal list) : (lam * goal list) =
    match gs with
    | [] -> raise No_Goals_Left
    | (gam, a)::gs ->
        if typecheck gam m a then (m, gs)
        else raise Cannot_Apply_Tactic

let exfalso (gs : goal list) : (lam * goal list) = match gs with
    | [] -> raise No_Goals_Left
    | (gam, a)::gs -> (Exf (Hole, a), (gam, False)::gs)

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

let left (gs : goal list) : (lam * goal list) =
    match gs with
    | [] -> raise No_Goals_Left
    | (env, ty)::gs ->
        begin
            match ty with
            | Or (a, b) ->
                let goal = env, a
                and lam = Ig (Hole, b) in
                (lam, goal::gs)
            | _ -> raise Cannot_Apply_Tactic
        end

let right (gs : goal list) : (lam * goal list) =
    match gs with
    | [] -> raise No_Goals_Left
    | (env, ty)::gs ->
        begin
            match ty with
            | Or (a, b) ->
                let goal = env, b
                and lam = Id (Hole, a) in
                (lam, goal::gs)
            | _ -> raise Cannot_Apply_Tactic
        end

let split (gs : goal list) : (lam * goal list) =
    match gs with
    | [] -> raise No_Goals_Left
    | (env, ty)::gs ->
        begin
            match ty with
            | And (a, b) ->
                let a_goal = (env, a)
                and b_goal = (env, b)
                and lam = Couple (Hole, Hole) in
                (lam, a_goal::b_goal::gs)
            | _ -> raise Cannot_Apply_Tactic
        end

let use_tactic (t : tactic) ((l, gs): proof) : proof =
    let (m, gs) = t gs in
    try Option.get (fill l m), gs
    with Invalid_argument _ -> raise Cannot_Apply_Tactic
