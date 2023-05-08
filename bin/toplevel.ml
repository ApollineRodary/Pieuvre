open Parsing
open Lambda

let continue = ref true

let () = begin
    print_endline "\r        Pieuvre Toplevel                    ";
    print_newline ();

    while (!continue) do
        print_string "> ";
        let s = read_line () in
        match s with
        | "exit" -> continue := false
        | _ ->
            begin
                match (parse_string s) with
                | Some x -> print_endline (string_of_lam x)
                | None -> ()
            end
    done
end