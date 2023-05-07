open Types
open Lambda
open Display

let () = print_endline "=== Test alpha_convert_fixed ===";

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
in print_lam l1; print_newline (); let l1' = alpha_convert_fixed l1 "x" "z" in print_lam l1';
