let a = Lambda.TypeVar "A" in

let l1 = (*(la x . la y . t x y)(la u . z u)*)
    Lambda.Application (
        Lambda.Abstraction ("x", a, Lambda.Abstraction ("y", a, 
            Lambda.Application (
                Lambda.Application(Var ("t"), Lambda.Var ("x")),
                Lambda.Var ("y")
            )
        )),

        Lambda.Abstraction ("u", a, Lambda.Application (Lambda.Var ("z"), Lambda.Var ("u")))
    )
in
let l2 = (*la x . x*)
    Lambda.Abstraction ("x", a, Lambda.Var "x")
in
Lambda.print_lam (Lambda.subst l1 "z" l2)
