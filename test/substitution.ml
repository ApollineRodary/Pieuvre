open Types
open Lambda
open Display

let () = print_endline "====== Test Substitution ======";

let a = TypeVar "A" in

let l1 = (*(la x . la y . t x y)(la u . z u)*)
    Application (
        Abstraction ("x", a, Abstraction ("y", a, 
            Application (
                Application(Var ("t"), Var ("x")),
                Var ("y")
            )
        )),

        Abstraction ("u", a, Application (Var ("z"), Var ("u")))
    )
in
let l2 = (*la x . x*)
    Abstraction ("x", a, Var "x")
in
print_lam (subst l1 "z" l2)
