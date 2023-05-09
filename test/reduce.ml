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

let%expect_test _ = begin
    reduce l;
    [%expect {|
      (fun (x:A) => (fun (y:A) => (x y))) (fun (u:A) => (u y))
      fun (y0:A) => ((fun (u:A) => (u y)) y0)
      fun (y0:A) => (y0 y) |}]
end