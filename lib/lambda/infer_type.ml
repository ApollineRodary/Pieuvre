open Types

let rec infer_type (gam : env) (m : lam) : ty option =
    match m with
    | Abstraction (x, t, m) -> 
        begin
            match infer_type ((x, t)::gam) m with
            | Some t' -> Some (Arrow (t, t'))
            | _ -> None
        end
    | Application (m, n) -> 
        begin
            let t1opt = infer_type gam n in
            let t2opt = infer_type gam m in
            match t1opt, t2opt with
            | Some t1, Some(Arrow (t2, t2')) when t2 = t1 -> Some t2'
            | _ -> None
        end
    | Var y -> List.assoc_opt y gam
    | Exf (m, t) ->
        if infer_type gam m = Some False then Some t
        else None
    | Hole -> failwith "Try to infer type of lambda term with hole(s)"
    | Couple (m, n) -> 
        begin
            match infer_type gam m, infer_type gam n with
            | Some t1, Some t2 -> Some (And (t1, t2))
            | _ -> None
        end
    | Fst m -> 
        begin
            match infer_type gam m with
            | Some (And (t1, _)) -> Some t1
            | _ -> None
        end
    | Snd m -> 
        begin
            match infer_type gam m with
            | Some (And (_, t2)) -> Some t2
            | _ -> None
        end
    | Unit -> Some True
    | Ig (m, b) -> 
        begin
            match infer_type gam m with
            | Some a -> Some (Or (a, b))
            | None -> None
        end
    | Id (m, a) -> 
        begin
            match infer_type gam m with
            | Some b -> Some (Or (a, b))
            | None -> None
        end
    | Case (m, n, n') ->
        begin
            match infer_type gam m, infer_type gam n, infer_type gam n' with
            | Some (Or (a, b)), Some (Arrow (a', c)), Some (Arrow (b', c')) when (a=a' && b=b' && c=c') -> Some c
            | _ -> None
        end
