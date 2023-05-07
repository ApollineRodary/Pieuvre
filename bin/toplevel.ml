open Parsing
open Lambda

let continue = ref true

let () =
print_endline "\r        Pieuvre Toplevel                    ";
print_newline ();

while (!continue) do
    print_string "> ";
    let s = read_line () in match s with
    | "exit" -> continue := false
    | _ -> (match (parse_string s) with
        | Some x -> print_endline (string_of_lam x)
        | None -> print_endline "Parsing error"
    )
done