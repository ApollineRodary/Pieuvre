type var = string
type t = string

type lam =
    | Abstraction of var*t*lam
    | Application of lam*lam
    | Var of var
    | Exf of lam*t

let print_lam (l:lam): unit =
    let rec aux (l:lam) (p:bool) = match l with
        | Abstraction (v, t, l) -> begin
            if p then print_string "(";
            print_string ("fun (" ^ v ^ ":" ^ t ^ ") => ");
            aux l true;
            if p then print_string ")";
        end
        | Application (m, n) -> begin
            if p then print_string "(";
            aux m true;
            print_string " ";
            aux n true;
            if p then print_string ")";
        end
        | Var x -> print_string x;
        | Exf (l, t) -> begin
            print_string "exf(";
            aux l false;
            print_string (":" ^ t ^ ")")
        end
    in aux l false

let alpha_convert_fixed (m : lam) (x : var) (x' : var) : lam = 
    let rec aux b m x x' = match m with (*b represents whether the variable we want to alpha-convert is bound or not*)
        | Abstraction (y, t, n) -> 
            if x != y then 
                Abstraction (y, t, aux b n x x')
            
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


let get_suffix_number (x : var) (y : var) : int option = int_of_string_opt (String.sub y (String.length x) (String.length y - String.length x))
        
let find_fresh_variable (x : var) (m : lam) : var =
    let rec aux (i : int) (x : var) (m : lam) : var = match m with
        | Application (m, n) -> 
            begin
                let x1 = aux i x m in
                let x2 = aux i x n in
                match (get_suffix_number x x1, get_suffix_number x x2) with
                    | (Some i1, Some i2) -> x ^ (string_of_int ((max i1 i2)))
                    | (_, Some i2) -> x ^ (string_of_int (i2))
                    | (Some i1, _) -> x ^ (string_of_int (i1))
                    | _ -> x
            end
        | Abstraction (y, _, n) -> 
            begin
                let x2 = aux i x n in
                match (get_suffix_number x y, get_suffix_number x x2) with
                    | (Some i1, Some i2) -> x ^ (string_of_int ((max i1 i2)))
                    | (_, Some i2) -> x ^ (string_of_int (i2))
                    | (Some i1, _) -> x ^ (string_of_int (i1))
                    | _ -> x
            end
        | Var y ->
            begin
                if x = y then
                    x ^ (string_of_int (i))
                else
                    match get_suffix_number x y with
                        | Some i -> x ^ (string_of_int (i+1))
                        | None -> x
            end
        
        | Exf (n, _) -> aux i x n
    in aux 0 x m

let alpha_convert (m : lam) (x : var) : lam =
    let x' = find_fresh_variable x m in
    alpha_convert_fixed m x x'

let rec is_alpha_convertible (m : lam) (n : lam) : bool = match (m, n) with
    | (Application (m1, m2), Application (n1, n2)) -> (is_alpha_convertible m1 n1) && (is_alpha_convertible m2 n2)
    | (Abstraction (x, _, m'), Abstraction (y, _, n')) -> 
        if x = y then 
            is_alpha_convertible m' n'
        else
            let x' = find_fresh_variable x m in
            let m1 = alpha_convert_fixed m x x' in
            let n1 = alpha_convert_fixed n y x' in
            is_alpha_convertible m1 n1
    
    | (Var x, Var y) -> x = y
    | (Exf (m, _),  Exf (n, _)) -> is_alpha_convertible m n
    | _ -> false

        