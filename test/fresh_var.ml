print_string "=== Test fresh_var ==="; print_newline();


let l1 = Lambda.Application (
    Lambda.Abstraction ("x", "A", 
        Lambda.Abstraction ("x1", "A",
            Lambda.Application (
                Lambda.Var ("x2"),
                Lambda.Var ("x5")
            )
        )
    ),
    Lambda.Abstraction ("x5a", "A", Lambda.Var "x0")
)
in
let x = Lambda.find_fresh_variable "x" l1 in print_string x; print_newline ();

let l2 = Lambda.Var "x" in let x = Lambda.find_fresh_variable "x" l2 in print_string x; print_newline ();
let l3 = Lambda.Application (Lambda.Var "x", Lambda.Var "y") in let x = Lambda.find_fresh_variable "y" l3 in print_string x; print_newline ();

let l4 = Lambda.Application (Lambda.Var "x0", Lambda.Var "y1") in let x = Lambda.find_fresh_variable "x" l4 in print_string x;
