print_string "=== Beta reduction ==="; print_newline();

let a = Lambda.TypeVar "A" in

let l = Lambda.Application(
    Lambda.Abstraction ("x", a, 
        Lambda.Abstraction ("y", a,
            Lambda.Application (
                Lambda.Var "x",
                Lambda.Var "y"
            )
        )
    ),
    Lambda.Abstraction ("u", a, 
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
let l' = Lambda.Abstraction ("y0", a,
    Lambda.Application (
        Lambda.Abstraction ("u", a, Lambda.Application (Lambda.Var "u", Lambda.Var "y")),
        Lambda.Var "y0"
    )
)
in match Lambda.betastep l' with
    | Some l'' -> Lambda.print_lam l'';
    | None -> print_string "erreur";
