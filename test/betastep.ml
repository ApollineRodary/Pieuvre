print_string "=== Beta reduction ==="; print_newline();


let l = Lambda.Application(
    Lambda.Abstraction ("x", "A", 
        Lambda.Abstraction ("y", "A",
            Lambda.Application (
                Lambda.Var "x",
                Lambda.Var "y"
            )
        )
    ),
    Lambda.Abstraction ("u", "A", 
        Lambda.Application (
            Var "u",
            Var "y"
        )
    )
)
in
(match Lambda.betastep l with
    | Some l' -> Lambda.print_lam l'; print_newline ();
    | None -> print_string "erreur"; print_newline ();
);
let l' = Lambda.Abstraction ("y0", "A",
    Lambda.Application (
        Lambda.Abstraction ("u", "A", Lambda.Application (Lambda.Var "u", Lambda.Var "y")),
        Lambda.Var "y0"
    )
)
in match Lambda.betastep l' with
    | Some l'' -> Lambda.print_lam l'';
    | None -> print_string "erreur";
