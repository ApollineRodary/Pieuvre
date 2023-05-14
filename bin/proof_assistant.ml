open Parsing
open Lambda
open Proof

let start_proof () =
    ignore (Sys.command "clear");

    let request = parse (Lexing.from_channel stdin) (Parsing__Parser.property_request) in
    let prop = Option.get request
    in begin
        let proof = ref (proof_start prop) in
        let message = ref "" in
        let continue = ref true in
        while (!continue) do 
            let (l, g) = !proof in begin
                ignore (Sys.command "clear");
                print_string !message;
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
            | Cannot_Apply_Tactic -> message := "Could not apply tactic\n"
            | Incomplete_Proof -> message := "There are still subgoals to prove\n"
            | No_Goals_Left -> message := "There are no subgoals to apply this tactic on\n"
            | Proof_Admitted ->
                begin
                    message := "Admitted\n";
                    continue := false
                end
            | Proven ->
                begin
                    message := "Proven\n";
                    continue := false
                end
            | Invalid_argument e -> (*message := "Parsing error :(\n"*) raise (Invalid_argument e)
            ;
        done;
        
        ignore (Sys.command "clear");
        print_type prop;
        print_newline ();
        print_string !message;

        let m = normal (fst !proof) in
        begin if typecheck [] m prop then
            print_endline (string_of_lam m)
        else
            print_endline "Failed to match lambda-term with the given lemma. This is expected if a goal was admitted, not so much otherwise."
        end
            
    end