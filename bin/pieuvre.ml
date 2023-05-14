open Parsing
open Lambda
open Proof_assistant

type pieuvreMode =
| Alpha
| Reduce
| Typecheck
| InteractiveSession

let filename = ref ""
let pieuvre_mode = ref InteractiveSession

let arg_specs = [
    ("-alpha", Arg.Unit (fun () -> pieuvre_mode := Alpha), "Test if two lambda terms are alpha convertible");
    ("-reduce", Arg.Unit (fun () -> pieuvre_mode := Reduce), "Reduce a lambda term to a normal form ");
    ("-typecheck", Arg.Unit (fun () -> pieuvre_mode := Typecheck), "Check if the given lambda term has the given type");
]

let get_channel (filename : string ref) =
    (*If no file name is provided, use standard input*)
    if (!filename = "") then stdin
    else open_in !filename

let alpha_mode () =
    let request = parse (Lexing.from_channel (get_channel filename)) (Parsing__Parser.alpha_request) in
    match request with
    | Some (m, n) -> print_endline (string_of_bool (is_alpha_convertible m n))
    | None -> ()

let reduce_mode () =
    match lam_of_channel (get_channel filename) with
    | Some l -> reduce l;
    | None -> ()

let typecheck_mode () =
    let request = parse (Lexing.from_channel (get_channel filename)) (Parsing__Parser.typecheck_request) in
    match request with
    | Some (l, t) -> print_endline (string_of_bool (typecheck [] l t))
    | None -> ()

let () = begin
    Arg.parse arg_specs (fun s -> filename := s) "dune exec -- pieuvre [mode] [filename]";
    print_newline ();

    match (!pieuvre_mode) with
    | Alpha -> alpha_mode ()
    | Reduce -> reduce_mode ()
    | Typecheck -> typecheck_mode ()
    | InteractiveSession -> start_proof ()
end
