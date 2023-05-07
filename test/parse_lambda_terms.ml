open Parsing.Main

let lambda_strings = [
    "x";
    "fun (x:A) => x x";
    "x y z";
    "fun (x:A) => fun (y:B) => x y"
]

let parse_and_print s = match (parse_string s) with
    | Some x -> Lambda.print_lam x
    | None -> ()

let _ = List.map parse_and_print lambda_strings