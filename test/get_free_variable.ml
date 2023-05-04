print_string "=== Test get free variables ==="; print_newline();

let a = Lambda.TypeVar "A" in

let l1 = (*(la x . la y . t x y)(la u . y u)*)
    Lambda.Application (
        Lambda.Abstraction ("x", a, Lambda.Abstraction ("y", a, 
            Lambda.Application (
                Lambda.Application(Var ("t"), Lambda.Var ("x")),
                Lambda.Var ("y")
            )
        )),

        Lambda.Abstraction ("u", a, Lambda.Application (Lambda.Var ("y"), Lambda.Var ("u")))
    )
in
let rec print_list (l : string list) = match l with
    | [] -> print_newline ();
    | t::q -> print_string t ; print_list q;
in print_list (Lambda.get_free_variables l1);
