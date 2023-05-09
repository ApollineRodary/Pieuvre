open Holes
open Lambda

let rec print_env (gam : env) = 
    print_newline ();
    match gam with
    | [] -> ()
    | (x, t)::q -> begin
        print_string (x ^ ": "); 
        print_type t; 
        print_env q;
    end

let print_goals (goals : goal list) : unit =
    let nb = List.length goals in begin
        print_endline ((string_of_int nb) ^ " goal" ^ (if nb > 1 then "s" else "")); print_newline ();
        let rec aux (goals : goal list) (i : int) : unit = begin
            match goals with
            | [] -> print_newline ();
            | (gam, t)::q ->
                if i = 1 then (print_env gam) else ();
                print_endline ("___________________________(" ^ (string_of_int i) ^ "/" ^ (string_of_int nb) ^ ")");
                print_type t;
                print_newline ();
                aux q (i+1);
            end
        in aux goals 1
    end
