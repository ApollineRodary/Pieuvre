open Infer_type
open Types

let rec typecheck (gam : env) (m : lam) (t : ty) : bool = match m, t with
    | Abstraction (x, t, m), Arrow (t1, t2) -> 
        (t1 = t) && typecheck ((x, t)::gam) m t2
    | Application (m, n), _ ->
        begin
            match (infer_type gam m, infer_type gam n) with
            | Some (Arrow (t1, t2)), Some t3 -> (t1 = t3) && (t2 = t)
            | _ -> false
        end
    | Var y, _ -> 
        begin
            match List.assoc_opt y gam with
            | Some t' -> t = t'
            | None -> false
        end
    
    | Exf (m, t'), _ -> (typecheck gam m False) && (t' = t)
    | Hole, _ -> failwith "Try to typecheck a lambda term with hole(s)"
    | Couple (m, n), And (t1, t2) -> (typecheck gam m t1) && (typecheck gam n t2)
    | Fst m, t -> 
        begin
            match (infer_type gam m) with
            | Some (And (t1, _)) -> t1 = t
            | _ -> false
        end
    | Snd m, t -> 
        begin
            match (infer_type gam m) with
            | Some (And (_, t2)) -> t2 = t
            | _ -> false
        end
    
    | Unit, True -> true

    | Ig (m, t), Or (a, b) -> t=b && typecheck gam m a
    | Id (m, t), Or (a, b) -> t=a && typecheck gam m b

    | Case (m, n, n'), t ->
        begin
            match infer_type gam m, infer_type gam n, infer_type gam n' with
            | Some (Or (a, b)), Some (Arrow (a', c)), Some (Arrow (b', c')) when (a=a' && b=b' && c=c') -> t=c
            | _ -> false
        end

    | _ -> false
