print_string "=== Reduction ==="; print_newline();


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
in Lambda.reduce l;
