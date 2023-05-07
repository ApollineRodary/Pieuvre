open Is_alpha_convertible
open Types

let alpha_convert (m : lam) (x : var) : lam =
    let x' = find_fresh_variable x m in
    alpha_convert_fixed m x x'
    

let rec subst (m : lam) (x : var) (n : lam) : lam = match m with
    | Application (m1, m2) -> Application (subst m1 x n, subst m2 x n)
    | Abstraction (y, t, m') ->
        if y = x then 
            begin
                print_string "Problem in alpha-conversion: need to substitute a variable that is in abstraction";
                print_newline (); 
                m
            end
        else
            Abstraction (y, t, subst m' x n)
    | Var y ->
        if y = x then 
            n
        else
            m
    
    | Exf (m', t) -> Exf (subst m' x n, t)

let rec get_free_variables (m : lam) : var list = match m with
    | Application (m1, m2) -> (get_free_variables m1) @ (get_free_variables m2)
    | Abstraction (x, _, m) ->
        begin
            let rec del_t l = match l with
                | [] -> []
                | t::q when t = x -> del_t q
                | t::q -> t::(del_t q)
            in
            del_t (get_free_variables m)
        end
    | Var y -> [y]
    | Exf (m, _) -> get_free_variables m

let rec alpha_convert_list (m : lam) (l : var list) : lam = match l with
    | [] -> m
    | x::q -> alpha_convert_list (alpha_convert m x) q

let rec betastep (m : lam) : lam option = match m with
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