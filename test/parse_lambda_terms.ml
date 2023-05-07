open Parsing
open Lambda

let () = print_endline "=== Parsing lambda terms ==="

let lambda_strings = [
    "x";
    "fun (x:A) => x x";
    "x y z";
    "fun (x:A) => fun (y:B) => x y"
]

let parse_and_print s = match (parse_string s) with
    | Some x -> print_endline (string_of_lam x)
    | None -> print_endline "Parsing error"

let _ = List.map parse_and_print lambda_strings