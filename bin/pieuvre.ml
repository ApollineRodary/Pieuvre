open Parsing
open Lambda

let filename = ref ""
let alpha_m = ref false
let reduce_m = ref false
let typecheck_m = ref false



let cli_options = [
    ("-alpha", Arg.Set alpha_m, "Tests if two lambda expression are alpha convertible");
    ("-reduce", Arg.Set reduce_m, "Reduces a lambda expression until a normal form is found. If the term is divergent, the program never stops.");
    ("-typecheck", Arg.Set typecheck_m, "Checks if the given lambda term has the given type");
]

let continue = ref true

let get_lam (msg : string) : lam option =
    print_string msg;
    let s = read_line () in
    match s with
        | "exit" -> continue := false; None
        | _ -> lam_of_string s

let get_type (msg : string) : ty option =
    print_string msg;
    let s = read_line () in
    match s with
        | "exit" -> continue := false; None
        | _ -> ty_of_string s

let print_mode () = 
    print_endline "Print mode";
    while (!continue) do
        print_newline ();
        match get_lam "> " with
            | Some x -> print_endline (string_of_lam x);
            | None -> ()
    done

let alpha_mode () =
    print_endline "Alpha mode";
    while (!continue) do
        print_newline ();
        let o1 = get_lam "Lam 1 > " in
        if (!continue) then
            match o1 with
                | Some e1 ->
                    begin
                        match (get_lam "Lam 2 > ") with
                            | Some e2 -> print_endline (string_of_bool (is_alpha_convertible e1 e2));
                            | _ -> ()
                    end
                | _ -> ()
    done

let reduce_mode () =
    print_endline "Reduce mode";
    while (!continue) do
        print_newline ();
        match (get_lam "> ") with
            | Some e -> reduce e;
            | None -> ()
    done

let typecheck_mode () =
    print_endline "Typecheck mode";
    while (!continue) do
        print_newline ();
        let o = get_lam "Lam > " in
        if (!continue) then
            match o with
                | Some e ->
                    begin
                        match (get_type "Type > ") with
                            | Some t -> print_endline (string_of_bool (typecheck [] e t));
                            | _ -> ()
                    end
                | _ -> ()
    done

let () = begin
    print_endline "\r========== Pieuvre ==========                              "; print_newline ();
    Arg.parse cli_options (fun s -> filename := s) "";
    if (!alpha_m) then alpha_mode ()
    else if (!reduce_m) then reduce_mode ()
    else if (!typecheck_m) then typecheck_mode ()
    else print_mode ()
end
