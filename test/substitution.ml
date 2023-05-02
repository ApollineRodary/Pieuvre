let l1 = (*(la x . la y . t x y)(la u . z u)*)
    Lambda.Application (
        Lambda.Abstraction ("x", "A", Lambda.Abstraction ("y", "A", 
            Lambda.Application (
                Lambda.Application(Var ("t"), Lambda.Var ("x")),
                Lambda.Var ("y")
            )
        )),

        Lambda.Abstraction ("u", "A", Lambda.Application (Lambda.Var ("z"), Lambda.Var ("u")))
    )
in
let l2 = (*la x . x*)
    Lambda.Abstraction ("x", "A", Lambda.Var "x")
in
Lambda.print_lam (Lambda.subst l1 "z" l2)
