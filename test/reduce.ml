print_string "=== Reduction ==="; print_newline();

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
in Lambda.reduce l;
