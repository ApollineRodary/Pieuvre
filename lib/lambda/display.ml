open Types

let string_of_type (t:ty): string =
    let rec aux (t:ty) (p:bool) =
        (* p: whether to parenthesize the type if it is "complex enough"*)
        match t with
        | Arrow (t1, t2) ->
              (if p then "(" else "")
            ^ aux t1 true
            ^ " -> "
            ^ aux t2 false
            ^ (if p then ")" else "")
        | TypeVar x -> x
        | False -> "False"
        | And (t1, t2) ->
              (if p then "(" else "")
            ^ aux t1 true
            ^ " /\\ "
            ^ aux t2 true
            ^ (if p then ")" else "")
        | True -> "True"
    in aux t false

let print_type (t:ty): unit = print_string (string_of_type t)

let string_of_lam (l:lam): string =
    let rec aux (l:lam) (p:bool) =
        (* p: whether to parenthesize the lambda-term if it is "complex enough" *)
        match l with
        | Abstraction (v, t, l) ->
              (if p then "(" else "")
            ^ "fun ("
            ^ v
            ^ " : "
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
              "exf ("
            ^ aux l false
            ^ ":"
            ^ string_of_type t
            ^ ")"
        | Hole -> " ? "
        | Couple (m, n) ->
              "("
            ^ aux m false
            ^ ","
            ^ aux n false
            ^ ")"
        | Fst m -> 
              "fst ("
            ^ aux m false
            ^ ")"
        | Snd m -> 
            "snd ("
            ^ aux m false
            ^ ")"
        | Unit -> "I"
    in aux l false

let print_lam (l:lam): unit = print_string (string_of_lam l)
