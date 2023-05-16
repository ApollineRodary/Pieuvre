open Parsing
open Lambda
open Proof

let read_prop () =
    let request = parse (Lexing.from_channel stdin) (Parsing__Parser.property_request) in
    Option.get request

let read_and_apply_tactic (proof_state : (proof ref * string ref * bool ref)) =
    let proof, message, continue = proof_state in
    let (l, g) = !proof in begin
        message := "";
        Proof__Display.print_goals g;
        print_lam l;
        print_newline ();
        print_newline ();
    end;
    try
        let tactic = Option.get (parse (Lexing.from_channel stdin) (Parsing__Parser.ptactic)) in
        proof := use_tactic tactic !proof
    with
    | Cannot_Apply_Tactic -> message := "\x1B[31mCould not apply tactic\x1B[39m\n\n"
    | Incomplete_Proof -> message := "\x1B[31mThere are still subgoals to prove\x1B[39m\n\n"
    | No_Goals_Left -> message := "\x1B[31mThere are no subgoals to apply this tactic on\x1B[39m\n\n"
    | Proof_Admitted ->
        begin
            message := "\x1B[33mAdmitted\x1B[39m\n\n";
            continue := false
        end
    | Proven ->
        begin
            message := "\x1B[32mSuccessfully proven\x1B[39m\n\n";
            continue := false
        end

let start_proof () =
    ignore (Sys.command "clear");

    let prop = read_prop () in
    let proof = ref (proof_start prop)
    and message = ref ""
    and continue = ref true in
    let proof_state = (proof, message, continue) in
    begin   
        while (!continue) do
            ignore (Sys.command "clear");
            print_string !message;
            read_and_apply_tactic proof_state
        done;
        
        ignore (Sys.command "clear");
        print_type prop;
        print_newline ();
        print_string !message;

        let m = normal (fst !proof) in
        begin if typecheck [] m prop then
            print_endline (string_of_lam m)
        else
            print_endline "\x1B[33mFailed to match lambda-term with the given lemma. This is expected if a goal was admitted, not so much otherwise.\x1B[31m"
        end
    end