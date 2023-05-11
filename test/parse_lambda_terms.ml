open Parsing
open Lambda

let lambda_strings = [
    "x";
    "fun (x:A) => x x";
    "x y z";
    "fun (x:A) => fun (y:B) => x y"
]

let parse_and_print s = match (lam_of_string s) with
    | Some x -> print_endline (string_of_lam x)
    | None -> print_endline "Parsing error"

let%expect_test _ = begin
    ignore (List.map parse_and_print lambda_strings);

    [%expect {|
      x
      fun (x : A) => (x x)
      (x y) z
      fun (x : A) => (fun (y : B) => (x y))
    |}]
end
