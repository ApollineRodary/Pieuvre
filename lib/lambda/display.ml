open Types

let string_of_type (t:ty): string =
    let rec aux (t:ty) (p:bool) = (match t with
        | Arrow (t1, t2) ->
              (if p then "(" else "")
            ^ aux t1 true
            ^ " -> "
            ^ aux t2 false
            ^ (if p then ")" else "")
        | TypeVar x -> x
        | False -> "False"
    ) in aux t false

let print_type (t:ty): unit = print_string (string_of_type t)

let string_of_lam (l:lam): string =
    let rec aux (l:lam) (p:bool) = (match l with
        | Abstraction (v, t, l) ->
              (if p then "(" else "")
            ^ "fun ("
            ^ v
            ^ ":"
            ^ string_of_type t
            ^ ") => "
            ^ aux l true
            ^ (if p then ")" else "")
        | Application (m, n) ->
              (if p then "(" else "")
            ^ aux m true
            ^ " "
            ^ aux n true
            ^ (if p then ")" else "")
        | Var x -> x
        | Exf (l, t) ->
              "exf("
            ^ aux l false
            ^ ":"
            ^ string_of_type t
            ^ ")"
        )
    in aux l false

    let print_lam (l:lam): unit = print_string (string_of_lam l)