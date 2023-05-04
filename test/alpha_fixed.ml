print_string "=== Test alpha_convert_fixed ==="; print_newline();

let a = Lambda.TypeVar "A" in

let l1 = (*(la x . la y . t x y)(la u . y u)*)
    Lambda.Application (
        Lambda.Abstraction ("x", a, Lambda.Abstraction ("y", a, 
            Lambda.Application (
                Lambda.Application(Var ("t"), Var ("x")),
                Lambda.Var ("y")
            )
        )),

        Lambda.Abstraction ("u", a, Lambda.Application (Var ("y"), Lambda.Var ("u")))
    )
in Lambda.print_lam l1; print_newline (); let l1' = Lambda.alpha_convert_fixed l1 "x" "z" in Lambda.print_lam l1';
