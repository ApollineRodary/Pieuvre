open Lambda

let print_bool b = print_string (if b then "true" else "false")

let a = TypeVar "A"

let l1 =
    (*(la x . la y . t x y)(la u . y u)*)
    Application (
        Abstraction ("x", a, Abstraction ("y", a, 
            Application (
                Application(Var ("t"), Var ("x")),
                Var ("y")
            )
        )),
        Abstraction ("u", a, Application (Var ("y"), Var ("u")))
    )

let l2 =
    (*(la z . la k . t z k)(la v . y v)*)
    Application (
        Abstraction ("z", a, Abstraction ("k", a, 
            Application (
                Application(Var ("t"), Var ("z")),
                Var ("k")
            )
        )),
        Abstraction ("v", a, Application (Var ("y"), Var ("v")))
    )

let l3 =
    (*(la z . la k . t z k)(la v . k v)*)
    Application (
        Abstraction ("z", a, Abstraction ("k", a, 
            Application (
                Application(Var ("t"), Var ("z")),
                Var ("k")
            )
        )),
        Abstraction ("v", a, Application (Var ("k"), Var ("v")))
    )

let () = begin
    print_endline "=== Is alpha convertible ===";
    
    print_bool (is_alpha_convertible l1 l2);
    print_newline ();
    print_bool (is_alpha_convertible l2 l1);
    print_newline();
    print_bool (is_alpha_convertible l2 l3);
    print_newline();
    print_bool (is_alpha_convertible l3 l1);
end