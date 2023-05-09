open Lambda
open Holes

let rec assoc_reverse_opt (l : ('a*'b) list) (x : 'b) : 'a option = match l with
    | [] -> None
    | (a, b)::_ when b = x -> Some a
    | _::q -> assoc_reverse_opt q x

let assumption ((gam, a) as g: goal) : (lam*goal) option = match assoc_reverse_opt gam a with
    | Some x -> Some (Var x, g)
    | None -> print_endline "Assumption not found in hypothesis"; None

let exact (m : lam) ((gam, a) as g: goal) : (lam*goal) option =
    if typecheck gam m a then Some (m, g)
    else begin
        print_endline "Lambda term does not correspond to the goal";
        None
    end

let intro (x : var) ((gam, a) : goal) : (lam*goal) option = match a with
    | Arrow (a, b) -> Some (Abstraction (x, a, Hole), (((x, a)::gam), b))
    | _ -> print_endline "Impossible to intro: not an arrow type"; None

let intros (l : var list) ((gam, a): goal) : (lam * goal) option = 
    let rec aux ((gam, a) as g : goal) (l : var list) (m : lam) = match l with
        | [] -> Some (m, g)
        | x::q -> 
            begin
                match intro x ((gam, a)) with
                | Some (n, (gam', b)) -> 
                    begin
                        match fill n (Abstraction (x, b, Hole)) with
                        | Some m' -> aux (gam', b) q m'
                        | None -> None
                    end
                | None -> None
            end
    in aux ((gam, a)) l Hole 
