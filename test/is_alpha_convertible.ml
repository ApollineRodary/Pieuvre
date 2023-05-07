open Lambda

let () = print_endline "=== Is alpha convertible ===";

let print_bool b = if b then print_string "true" else print_string "false" in

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

let l2 = (*(la z . la k . t z k)(la v . y v)*)
    Application (
        Abstraction ("z", a, Abstraction ("k", a, 
            Application (
                Application(Var ("t"), Var ("z")),
                Var ("k")
            )
        )),

        Abstraction ("v", a, Application (Var ("y"), Var ("v")))
    )
in
let l3 = (*(la z . la k . t z k)(la v . k v)*)
    Application (
        Abstraction ("z", a, Abstraction ("k", a, 
            Application (
                Application(Var ("t"), Var ("z")),
                Var ("k")
            )
        )),

        Abstraction ("v", a, Application (Var ("k"), Var ("v")))
    )
in
print_bool (is_alpha_convertible l1 l2) ; print_newline();
print_bool (is_alpha_convertible l2 l1) ; print_newline();
print_bool (is_alpha_convertible l2 l3) ; print_newline();
print_bool (is_alpha_convertible l3 l1) ; print_newline();
