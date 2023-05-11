open Betastep
open Display
open Types

let rec reduce (m : lam) : unit = 
    print_endline (string_of_lam m);
    match betastep m with
    | Some m' -> reduce m'
    | None -> ()

let rec normal (m : lam) : lam = match betastep m with
    | Some m' -> normal m'
    | None -> m
