open Types
open Lambda

let () = print_endline "=== Test fresh_var ===";

let a = TypeVar "A" in


let l1 = Application (
    Abstraction ("x", a, 
        Abstraction ("x1", a,
            Application (
                Var ("x2"),
                Var ("x5")
            )
        )
    ),
    Abstraction ("x5a", a, Var "x0")
)
in
let x = find_fresh_variable "x" l1 in print_string x; print_newline ();

let l2 = Var "x" in let x = find_fresh_variable "x" l2 in print_string x; print_newline ();
let l3 = Application (Var "x", Var "y") in let x = find_fresh_variable "y" l3 in print_string x; print_newline ();

let l4 = Application (Var "x0", Var "y1") in let x = find_fresh_variable "x" l4 in print_string x;
