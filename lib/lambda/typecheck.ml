open Infer_type
open Types

let rec typecheck (gam : (var * ty) list) (m : lam) (t : ty) : bool = match m, t with
    | (Abstraction (x, t, m), Arrow (t1, t2)) -> t1 = t2 && typecheck ((x, t)::gam) m t2
    | (Application (m, n), _) ->
        begin
            match (infer_type gam m, infer_type gam n) with
                | (Some (Arrow (t1, t2)), Some t3) -> t1 = t3 && t2 = t
                | _ -> false
        end
    | (Var y, _) -> 
        begin
            match List.assoc_opt y gam with
                | Some t' -> t = t'
                | None -> false
        end
    
    | (Exf (m, t'), _) -> (typecheck gam m False) && t' = t 
    | _ -> false
