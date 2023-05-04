print_string "=== Test alpha_convert ==="; print_newline();

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
in
let l1' = Lambda.alpha_convert l1 "x" in Lambda.print_lam l1'; print_newline ();

let l1'' = Lambda.alpha_convert l1 "y" in Lambda.print_lam l1''; print_newline();

let l2 = Lambda.Abstraction ("x", a, Lambda.Abstraction ("x", Lambda.TypeVar "B", Application (Var "x", Var "x"))) in 
Lambda.print_lam l2; print_newline();
let l2' = Lambda.alpha_convert l2 "x" in Lambda.print_lam l2'; print_newline ();
let l2'' = Lambda.alpha_convert l2 "a" in Lambda.print_lam l2''; print_newline ();

let l3 = Lambda.Application (Lambda.Var "u", Lambda.Var "y") in let l3' = Lambda.alpha_convert l3 "y0" in Lambda.print_lam l3';
