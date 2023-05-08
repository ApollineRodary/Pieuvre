open Lambda

let a = TypeVar "A"

let l = Application (
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

let () = begin
    print_endline "=== Reduction ===";
    reduce l
end