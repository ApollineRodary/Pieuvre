type var = string
type t = string

type lam =
    | Abstraction of var*t*lam
    | Application of lam*lam
    | Var of var
    | Exf of lam*t

let rec print_lam (l:lam): unit = match l with
    | Abstraction (v, t, l) -> begin
        print_string ("fun (" ^ v ^ ":" ^ t ^ ") => ");
        print_lam l
    end
    | Application (m, n) -> begin
        print_lam m;
        print_lam n
    end
    | Var x -> print_string x;
    | Exf (l, t) -> begin
        print_string "exf(";
        print_lam l;
        print_string (":" ^ t ^ ")")
    end

let alpha_convert_fixed (m : lam) (x : var) (x' : var) : lam = 
    let rec aux b m x x' = match m with (*b represents if the variable we want to alpha-convert is bound or not*)
        | Abstraction (y, t, n) -> 
            if x != y then 
                Abstraction (y, t, aux b n x x')
            else if b then
                m
            else
                Abstraction (x', t, aux true n x x')
        | Application (m, n) -> Application ((aux b m x x'), (aux b n x x'))
        | Var y ->
            if y != x then
                m
            else if b then
                Var x'
            else
                m
        | Exf (n, t) -> Exf (aux b n x x', t)
    in aux false m x x'

let find_fresh_variable (x : var) (env : (var*var) list) : var =
    let rec aux (i : int) (x : var) (env : (var*var) list) : var = match env with
        | [] -> x ^ (string_of_int i)
        | (y', y)::q -> 
            if y' = x then 
                begin
                    match int_of_string_opt (String.sub y (String.length x) (String.length y - String.length x)) with
                        | Some i' -> aux (i'+1) x q
                        | None -> aux i x q
                end
                
            else
                aux i x q
    in aux 0 x env 
        

 let alpha_convert (m : lam) (x : var) : lam =
    let rec aux (m : lam) (x : var) (env : (var*var) list) = match m with
        | Abstraction (y, t, n) ->
            if x != y then
                Abstraction (y, t, aux n x env)
            else
                let x' = find_fresh_variable x env in
                Abstraction (x', t, aux n x ((x, x')::env))
        | Application (m, n) -> Application ((aux m x env), (aux n x env))
        | Var y -> 
            begin
                match List.assoc_opt y env with
                    | Some y' -> Var y'
                    | None -> Var y
            end
        
        | Exf (n, t) -> Exf (aux n x env, t)
    
    in aux m x []
        
let is_alpha_convertible (m : lam) (n : lam) : bool = 
    let rec aux (m : lam) (n : lam) (env : (var*var) list)= match (m, n) with
        | (Application (m1, m2), Application (n1, n2)) -> (aux m1 n1 env) && (aux m2 n2 env)
        | (Abstraction (x, _, m), Abstraction (y, _, n)) ->
            let x' = find_fresh_variable x env in
            aux m n ((x,x')::(y,x')::env)
        | (Var x, Var y) ->
            begin
                match (List.assoc_opt x env, List.assoc_opt y env) with
                    | (None, None) -> x = y
                    | (Some x', Some y') -> x' = y'
                    | _ -> false
            end
        | _ -> false
    in aux m n []
