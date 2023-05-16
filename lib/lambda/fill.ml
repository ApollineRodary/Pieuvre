open Types

let fill (m : lam) (n : lam) : lam option =
    (* Fill in the first hole in m with n
       Returns None if nothing was replaced *)
    let rec aux (m : lam) (n : lam) : lam option = match m with
        | Abstraction (x, t, m) -> 
            begin
                match aux m n with
                | Some m' -> Some (Abstraction (x, t, m'))
                | None -> None
            end
        | Application (m1, m2) -> 
            begin
                match (aux m1 n, aux m2 n) with
                | Some m1', _ -> Some (Application (m1', m2))
                | None, Some m2' -> Some (Application (m1, m2'))
                | None, None -> None
            end
        | Var _ -> None
        | Exf (m, t) ->
            begin
                match aux m n with
                | Some m' -> Some (Exf (m', t))
                | None -> None
            end
        | Hole -> Some n
        | Couple (m1, m2) -> 
            begin 
                match (aux m1 n, aux m2 n) with
                | Some m1', _ -> Some (Couple (m1', m2))
                | None, Some m2' -> Some (Couple (m1, m2'))
                | None, None -> None
            end
        | Fst m -> 
            begin
                match aux m n with
                | Some m' -> Some (Fst m')
                | None -> None
            end
        | Snd m -> 
            begin
                match aux m n with
                | Some m' -> Some (Snd m')
                | None -> None
            end
        | Unit -> None

    in aux m n
