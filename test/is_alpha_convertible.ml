print_string "=== Is alpha convertible ==="; print_newline();

let print_bool b = if b then print_string "true" else print_string "false" in

let l1 = (*(la x . la y . t x y)(la u . y u)*)
    Lambda.Application (
        Lambda.Abstraction ("x", "A", Lambda.Abstraction ("y", "A", 
            Lambda.Application (
                Lambda.Application(Var ("t"), Lambda.Var ("x")),
                Lambda.Var ("y")
            )
        )),

        Lambda.Abstraction ("u", "A", Lambda.Application (Lambda.Var ("y"), Lambda.Var ("u")))
    )
in

let l2 = (*(la z . la k . t z k)(la v . y v)*)
    Lambda.Application (
        Lambda.Abstraction ("z", "A", Lambda.Abstraction ("k", "A", 
            Lambda.Application (
                Lambda.Application(Lambda.Var ("t"), Lambda.Var ("z")),
                Lambda.Var ("k")
            )
        )),

        Lambda.Abstraction ("v", "A", Lambda.Application (Lambda.Var ("y"), Lambda.Var ("v")))
    )
in
let l3 = (*(la z . la k . t z k)(la v . k v)*)
    Lambda.Application (
        Lambda.Abstraction ("z", "A", Lambda.Abstraction ("k", "A", 
            Lambda.Application (
                Lambda.Application(Lambda.Var ("t"), Lambda.Var ("z")),
                Lambda.Var ("k")
            )
        )),

        Lambda.Abstraction ("v", "A", Lambda.Application (Lambda.Var ("k"), Lambda.Var ("v")))
    )
in
print_bool (Lambda.is_alpha_convertible l1 l2) ; print_newline();
print_bool (Lambda.is_alpha_convertible l2 l1) ; print_newline();
print_bool (Lambda.is_alpha_convertible l2 l3) ; print_newline();
print_bool (Lambda.is_alpha_convertible l3 l1) ; print_newline();
