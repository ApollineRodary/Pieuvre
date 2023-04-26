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
