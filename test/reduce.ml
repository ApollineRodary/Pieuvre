open Lambda

let () = print_endline "=== Reduction ===";

let a = TypeVar "A" in

let l = Application(
    Abstraction ("x", a, 
        Abstraction ("y", a,
            Application (
                Var "x",
                Var "y"
            )
        )
    ),
    Abstraction ("u", a, 
        Application (
            Var "u",
            Var "y"
        )
    )
)
in reduce l;
