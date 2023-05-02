let l1 = (*(la x . la y . t x y)(la u . y u)*)
    Lambda.Application (
        Lambda.Abstraction ("x", "A", Lambda.Abstraction ("y", "A", 
            Lambda.Application (
                Lambda.Application(Var ("t"), Var ("x")),
                Lambda.Var ("y")
            )
        )),

        Lambda.Abstraction ("u", "A", Lambda.Application (Var ("y"), Lambda.Var ("u")))
    )
in let l1'' = Lambda.alpha_convert l1 "x" in Lambda.print_lam l1''; print_newline ();

let l2 = Lambda.Abstraction ("x", "A", Lambda.Abstraction ("x", "B", Application (Var "x", Var "x"))) in 
Lambda.print_lam l2; print_newline();
let l2' = Lambda.alpha_convert l2 "x" in Lambda.print_lam l2'; print_newline ();
