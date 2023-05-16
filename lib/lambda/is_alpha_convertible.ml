open Types

let alpha_convert_fixed (m : lam) (x : var) (x' : var) : lam =
    let rec aux b m x x' = match m with (*b represents whether the variable we want to alpha-convert is bound or not*)
        | Abstraction (y, t, n) -> 
            if x = y then Abstraction (x', t, aux true n x x')
            else Abstraction (y, t, aux b n x x')
        | Application (m, n) -> Application ((aux b m x x'), (aux b n x x'))
        | Var y ->
            if (b && y = x) then Var x'
            else m
        | Exf (n, t) -> Exf (aux b n x x', t)
        | Hole -> failwith "Try to alpha convert a lambda term with hole(s)"
        | Couple (m, n) -> Couple (aux b m x x', aux b n x x')
        | Fst m -> Fst (aux b m x x')
        | Snd m -> Snd (aux b m x x')
        | Unit -> Unit
        | Ig (m, t) -> Ig (aux b m x x', t)
        | Id (m, t) -> Id (aux b m x x', t)
        | Case (m, n, n') -> Case (aux b m x x', aux b n x x', aux b n' x x')
    in aux false m x x'

let get_suffix_number (x : var) (y : var) : int option =
    try
        int_of_string_opt (String.sub y (String.length x) (String.length y - String.length x))
    with
    | Invalid_argument s when s = "String.sub / Bytes.sub" -> None
    | e -> raise e

let find_fresh_variable (x : var) (m : lam) : var =
    let rec aux (i : int) (x : var) (m : lam) : var = match m with
        | Application (m, n) -> 
            begin
                let x1 = aux i x m in
                let x2 = aux i x n in
                match (get_suffix_number x x1, get_suffix_number x x2) with
                | Some i1, Some i2 -> x ^ string_of_int (max i1 i2)
                | _, Some i2 -> x ^ string_of_int i2
                | Some i1, _ -> x ^ string_of_int i1
                | _ -> x
            end
        | Abstraction (y, _, n) -> 
            begin
                let x2 = aux i x n in
                match (get_suffix_number x y, get_suffix_number x x2) with
                | Some i1, Some i2 -> x ^ string_of_int (max i1 i2)
                | _, Some i2 -> x ^ string_of_int i2
                | Some i1, _ -> x ^ string_of_int i1
                | _ -> x
            end
        | Var y ->
            begin
                if x = y then x ^ string_of_int i
                else
                    match get_suffix_number x y with
                    | Some i -> x ^ string_of_int (i+1)
                    | None -> x
            end
        | Exf (n, _) -> aux i x n
        | Hole -> failwith "Try to find a fresh variable in lambda term with hole(s)"
        | Couple (m, n) -> 
            begin
                let x1 = aux i x m in
                let x2 = aux i x n in
                match (get_suffix_number x x1, get_suffix_number x x2) with
                | Some i1, Some i2 -> x ^ string_of_int (max i1 i2)
                | _, Some i2 -> x ^ string_of_int i2
                | Some i1, _ -> x ^ string_of_int i1
                | _ -> x
            end
        | Fst m -> aux i x m
        | Snd m -> aux i x m
        | Unit -> "h"
        | Ig (m, _) -> aux i x m
        | Id (m, _) ->  aux i x m
        | Case (m, n, n') -> 
            begin
                let x1 = aux i x m in
                let x2 = aux i x n in
                let x3 = aux i x n' in
                match (get_suffix_number x x1, get_suffix_number x x2, get_suffix_number x x3) with
                | Some i1, Some i2, Some i3 -> x ^ string_of_int (max (max i1 i2) i3)
                | Some i1, Some i2, None -> x ^ string_of_int (max i1 i2)
                | Some i1, None, Some i3 -> x ^ string_of_int (max i1 i3)
                | None, Some i2, Some i3 -> x ^ string_of_int (max i2 i3)
                | Some i1, None, None -> x ^ string_of_int i1
                | None, Some i2, None -> x ^ string_of_int i2
                | None, None, Some i3 -> x ^ string_of_int i3
                | _ -> x
            end
    in aux 0 x m

let rec is_alpha_convertible (m : lam) (n : lam) : bool = match (m, n) with
    | Application (m1, m2), Application (n1, n2) -> (is_alpha_convertible m1 n1) && (is_alpha_convertible m2 n2)
    | Abstraction (x, _, m'), Abstraction (y, _, n') -> 
        if x = y then
            is_alpha_convertible m' n'
        else
            let x' = find_fresh_variable x m in
            let m1 = alpha_convert_fixed m x x' in
            let n1 = alpha_convert_fixed n y x' in
            is_alpha_convertible m1 n1
    | Var x, Var y -> x = y
    | Exf (m, _),  Exf (n, _) -> is_alpha_convertible m n
    | (Hole, _) -> failwith "Try to test alpha convertibility with a lambda term with hole(s) (First argument)"
    | (_, Hole) -> failwith "Try to test alpha convertibility with a lambda term with hole(s) (Second argument)"
    | (Couple (m1, n1), Couple (m2, n2)) -> (is_alpha_convertible m1 n1) && (is_alpha_convertible m2 n2)
    | Fst m, Fst n -> is_alpha_convertible m n
    | Snd m, Snd n -> is_alpha_convertible m n
    | Unit, Unit -> true
    | Ig (m, _), Ig (n, _) -> is_alpha_convertible m n
    | Id (m, _), Id (n, _) -> is_alpha_convertible m n
    | Case (m1, n1, n1'), Case (m2, n2, n2') -> is_alpha_convertible m1 m2 && is_alpha_convertible n1 n2 && is_alpha_convertible n1' n2'
    | _ -> false
