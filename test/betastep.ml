open Types
open Lambda
open Display

let () = print_endline "=== Beta reduction ===";

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
in
(match betastep l with
    | Some l' -> print_lam l'; print_newline ();
    | None -> print_string "erreur"; print_newline ();
);
let l' = Abstraction ("y0", a,
    Application (
        Abstraction ("u", a, Application (Var "u", Var "y")),
        Var "y0"
    )
)
in match betastep l' with
    | Some l'' -> print_lam l'';
    | None -> print_string "erreur";
