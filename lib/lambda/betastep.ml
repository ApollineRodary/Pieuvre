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
        if y = x then begin
            print_endline "Problem in alpha-conversion: need to substitute a variable that is in abstraction"; 
            m
        end else
            Abstraction (y, t, subst m' x n)
    | Var y -> if y = x then n else m
    | Exf (m', t) -> Exf (subst m' x n, t)
    | Hole _ -> failwith "Substition in lambda term with hole(s)"

let rec get_free_variables (m : lam) : var list =
    (*Returns free variables used in m*)
    match m with
    | Application (m1, m2) -> (get_free_variables m1) @ (get_free_variables m2)
    | Abstraction (x, _, m) ->
        begin
            let rec del_t l = match l with
                | [] -> []
                | t::ts when t = x -> del_t ts
                | t::ts -> t::(del_t ts)
            in
            del_t (get_free_variables m)
        end
    | Var y -> [y]
    | Exf (m, _) -> get_free_variables m
    | Hole _ -> failwith "Try to get free variables in lambda term with hole(s)"

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
    
    | Hole _ -> failwith "Try to reduce a lambda term with hole(s)"
    