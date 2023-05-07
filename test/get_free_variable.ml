open Types
open Lambda

let () = print_endline "=== Test get free variables ===";

let a = TypeVar "A" in

let l1 = (*(la x . la y . t x y)(la u . y u)*)
    Application (
        Abstraction ("x", a, Abstraction ("y", a, 
            Application (
                Application(Var ("t"), Var ("x")),
                Var ("y")
            )
        )),

        Abstraction ("u", a, Application (Var ("y"), Var ("u")))
    )
in
let rec print_list (l : string list) = match l with
    | [] -> print_newline ();
    | t::q -> print_string t ; print_list q;
in print_list (get_free_variables l1);
