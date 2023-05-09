open Lambda

type goal = (env*ty)

type proof = lam*(goal list)

let start (a : ty) : proof = (Hole, [([], a)])

let fill (m : lam) (n : lam) : lam option =
    let rec aux (m : lam) (n : lam) : lam option = match m with
        | Abstraction (x, t, m) -> 
            begin
                match aux m n with
                | Some m' -> Some (Abstraction (x, t, m'))
                | None -> None
            end
        | Application (m1, m2) -> 
            begin
                match aux m1 n with
                | Some m1' -> Some (Application (m1', m2))
                | None ->
                    begin
                        match aux m2 n with
                        | Some m2' -> Some (Application (m1, m2'))
                        | None -> None
                    end
            end
        
        | Var _ -> None
        | Exf (m, t) ->
            begin
                match aux m n with
                | Some m' -> Some (Exf (m', t))
                | None -> None
            end
        | Hole -> Some n
    in aux m n
