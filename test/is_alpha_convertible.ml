open Lambda


let print_bool b = print_endline (if b then "true" else "false")

let a = TypeVar "A"

let l1 =
    (*(la x . la y . t x y)(la u . y u)*)
    Application (
        Abstraction ("x", a, Abstraction ("y", a, 
            Application (
                Application(Var ("t"), Var ("x")),
                Var ("y")
            )
        )),
        Abstraction ("u", a, Application (Var ("y"), Var ("u")))
    )

let l2 =
    (*(la z . la k . t z k)(la v . y v)*)
    Application (
        Abstraction ("z", a, Abstraction ("k", a, 
            Application (
                Application(Var ("t"), Var ("z")),
                Var ("k")
            )
        )),
        Abstraction ("v", a, Application (Var ("y"), Var ("v")))
    )

let l3 =
    (*(la z . la k . t z k)(la v . k v)*)
    Application (
        Abstraction ("z", a, Abstraction ("k", a, 
            Application (
                Application(Var ("t"), Var ("z")),
                Var ("k")
            )
        )),
        Abstraction ("v", a, Application (Var ("k"), Var ("v")))
    )

let l4 = Abstraction ("x", a, Var "x")
let l5 = Abstraction ("y", a, Var "x")

let l6_opt = Parsing.parse_string "fun (x:A) => x"
let l7_opt = Parsing.parse_string "fun (y:A) => y"

let l6, l7 =
    match l6_opt, l7_opt with
    | Some u, Some v -> u, v
    | _ -> failwith "Parsing error"

let comparisons = [
    (l1, l2);
    (l2, l1);
    (l2, l3);
    (l3, l1);
    (l4, l5);
    (l6, l7)
]

let print_alpha_convertibility (x, y) = print_bool (is_alpha_convertible x y)

let () = begin
    print_endline "=== Is alpha convertible ===";
    ignore (List.map print_alpha_convertibility comparisons)
end