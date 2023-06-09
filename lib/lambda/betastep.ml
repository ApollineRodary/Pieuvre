open Is_alpha_convertible
open Types

let alpha_convert (m : lam) (x : var) : lam =
    (*Renames x to a fresh variable in m*)
    let x' = find_fresh_variable x m in
    alpha_convert_fixed m x x'

let rec alpha_convert_list (m : lam) (l : var list) : lam = match l with
    (*Alpha-convert all variables of l in m*)
    | [] -> m
    | x::xs -> alpha_convert_list (alpha_convert m x) xs

let rec subst (m : lam) (x : var) (n : lam) : lam =
    (*Returns m[n/x]*)
    match m with
    | Application (m1, m2) -> Application (subst m1 x n, subst m2 x n)
    | Abstraction (y, t, m') ->
        if y = x then m
        else Abstraction (y, t, subst m' x n)
    | Var y -> if y = x then n else m
    | Exf (m', t) -> Exf (subst m' x n, t)
    | Hole -> failwith "Tried to apply subst to a lambda term with holes. I don't know what to do with that."

    | Couple (m1, m2) -> Couple (subst m1 x n, subst m2 x n)
    | Fst m -> Fst (subst m x n)
    | Snd m -> Snd (subst m x n)
    | Unit -> Unit
    | Ig (m, t) -> Ig (subst m x n, t)
    | Id (m, t) -> Id (subst m x n, t)
    | Case (m, m1, m2) -> Case (subst m x n, subst m1 x n, subst m2 x n)

let rec get_free_variables (m : lam) : var list =
    (*Returns free variables used in m*)
    match m with
    | Application (m1, m2) -> (get_free_variables m1) @ (get_free_variables m2)
    | Abstraction (x, _, m) ->
        begin
            let rec del_t l = 
                match l with
                | [] -> []
                | t::ts when t = x -> del_t ts
                | t::ts -> t::(del_t ts)
            in
            del_t (get_free_variables m)
        end
    | Var y -> [y]
    | Exf (m, _) -> get_free_variables m
    | Hole -> failwith "Tried to apply get_free_variables to a lambda term with holes. I don't know what to do with that."

    | Couple (m1, m2) -> (get_free_variables m1) @ (get_free_variables m2)
    | Fst m -> get_free_variables m
    | Snd m -> get_free_variables m
    | Unit -> []
    | Ig (m, _) -> get_free_variables m
    | Id (m, _) -> get_free_variables m
    | Case (m, n, n') -> (get_free_variables m) @ (get_free_variables n) @ (get_free_variables n')

let rec betastep (m : lam) : lam option =
    match m with
    | Application (m1, m2) -> 
        begin
            match (betastep m1, betastep m2) with
            | (Some m1', _) -> Some (Application (m1', m2))
            | (None, Some m2') -> Some (Application (m1, m2'))
            | (None, None) ->
                begin
                    match m1 with
                    | Abstraction (x, _, m1') -> 
                        begin
                            let l = get_free_variables m2 in
                            let m1'' = alpha_convert_list m1' l in
                            let m1''' = alpha_convert m1'' x in
                            Some (subst m1''' x m2)
                        end
                    | _ -> None 
                end 
        end
    
    | Abstraction (x, t, m) -> 
        begin
            match betastep m with
            | Some m' -> Some (Abstraction (x, t, m'))
            | None -> None
        end
    
    | Var _ -> None

    | Exf (m, t) ->
        begin
            match betastep m with
            | Some m' -> Some (Exf (m', t))
            | None -> None
        end
    
    | Hole -> failwith "Tried to apply reduce to a lambda term with holes. I don't know what to do with that."
    
    | Couple (m, n) -> 
        begin
            match betastep m, betastep n with
            | (Some m', _) -> Some (Couple (m', n))
            | (None, Some n') -> Some (Couple (m, n'))
            | _ -> None
        end
    
    | Fst (Couple (m, _)) -> Some m
    | Fst _ -> None
    | Snd (Couple (_, n)) ->  Some n
    | Snd _ -> None
    | Unit -> None
    | Ig (m, t) ->
        begin 
            match betastep m with
            | Some m' -> Some (Ig (m', t))
            | None -> None
        end
    | Id (m, t) ->
        begin 
            match betastep m with
            | Some m' -> Some (Id (m', t))
            | None -> None
        end
    | Case (Ig (m, _), n, _) -> Some (Application (n, m))
    | Case (Id (m, _), _, n') -> Some (Application (n', m))
    | Case _ -> None
