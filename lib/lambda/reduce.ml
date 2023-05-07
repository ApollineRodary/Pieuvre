open Betastep
open Display
open Types

let rec reduce (m : lam) : unit = 
    print_lam m;
    print_newline ();
    match betastep m with
        | Some m' -> reduce m';
        | None -> ()