open Types
open Lambda
open Display

let () = print_endline "=== Test alpha_convert ===";

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
let l1' = alpha_convert l1 "x" in print_lam l1'; print_newline ();

let l1'' = alpha_convert l1 "y" in print_lam l1''; print_newline();

let l2 = Abstraction ("x", a, Abstraction ("x", TypeVar "B", Application (Var "x", Var "x"))) in 
print_lam l2; print_newline();
let l2' = alpha_convert l2 "x" in print_lam l2'; print_newline ();
let l2'' = alpha_convert l2 "a" in print_lam l2''; print_newline ();

let l3 = Application (Var "u", Var "y") in let l3' = alpha_convert l3 "y0" in print_lam l3';
