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
