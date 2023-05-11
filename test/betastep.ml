open Lambda

let a = TypeVar "A"

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

let l' = Abstraction ("y0", a,
    Application (
        Abstraction ("u", a, Application (Var "u", Var "y")),
        Var "y0"
    )
)

let%expect_test _ = begin
    begin
        match betastep l with
        | Some l1 -> print_lam l1;
        | None -> print_string "Erreur"
    end;

    [%expect{|
      fun (y0 : A) => ((fun (u : A) => (u y)) y0)
    |}];

    print_newline ();

    begin
        match betastep l' with
        | Some l'' -> print_lam l'';
        | None -> print_string "Erreur"
    end;
    
    [%expect{|
      fun (y0 : A) => (y0 y)
    |}]
end