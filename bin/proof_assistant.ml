open Parsing
open Lambda
open Proof

let clear () = ignore (Sys.command "clear")

(* Warnings, error messages and status messages *)
let could_not_apply_tactic = "\x1B[31mCould not apply tactic\x1B[39m\n\n"
let incomplete_proof = "\x1B[31mThere are still subgoals to prove\x1B[39m\n\n"
let no_goals = "\x1B[31mThere are no subgoals to apply this tactic on\x1B[39m\n\n"
let admitted = "\x1B[33mAdmitted\x1B[39m\n\n"
let successfully_proven = "\x1B[32mSuccessfully proven\x1B[39m\n\n"
let typecheck_failure = "\x1B[33mFailed to match lambda-term with the given lemma. This is expected if a goal was admitted, not so much otherwise.\x1B[31m"

let read_prop (lexbuf : Lexing.lexbuf) =
    let request = parse lexbuf (Parsing__Parser.property_request) in
    Option.get request

let read_commands (lexbuf : Lexing.lexbuf) (proof : proof ref) (continue : bool ref) (out : out_channel) =
    Proof__Display.print_goals (snd !proof);
    print_newline ();
    print_newline ();

    let command = Option.get (parse lexbuf (Parsing__Parser.command)) in
    match command with
    | UseTactic (t, cmd) ->
        begin
            clear ();
            try
                proof := use_tactic t !proof;
                Printf.fprintf out "%s.\n" cmd
            with
            | Cannot_Apply_Tactic -> print_string could_not_apply_tactic
            | No_Goals_Left -> print_string no_goals
        end

    | Admitted ->
        let goals = snd !proof in
        begin
            clear ();
            match goals with
            | [] -> 
                begin
                    print_string admitted;
                    Printf.fprintf out "Admitted.";
                    continue := false
                end
            | _ -> print_string incomplete_proof
        end

    | Qed ->
        let goals = snd !proof in
        begin
            clear ();
            match goals with
            | [] -> 
                begin
                    print_string successfully_proven;
                    Printf.fprintf out "Qed.";
                    continue := false
                end
            | _ -> print_string incomplete_proof
        end
    
    | Print ->
        begin
            clear ();
            print_lam (fst !proof); print_newline ()
        end

let start_proof (lexbuf : Lexing.lexbuf) =
    clear ();

    let prop = read_prop lexbuf in
    let proof = ref (proof_start prop)
    and message = ref ""
    and continue = ref true
    and oc = open_out "proof.8pus" in
    begin
        Printf.fprintf oc "%s.\n" (Lambda__Display.string_of_type prop);
        while (!continue) do
            read_commands lexbuf proof continue oc
        done;
        
        clear ();
        print_type prop;
        print_newline ();
        print_string !message;

        try
            let m = normal (fst !proof) in
            if typecheck [] m prop then
                begin
                    print_endline (string_of_lam m);
                    Printf.fprintf (open_out "proof.lam") "%s" (string_of_lam m)
                end
            else raise (Failure ":(")
        with
            (* Runs either if normal or typecheck fails (because there are holes in the lambda-term) or if it returns false (for whatever reason) *)
            Failure _ -> print_endline typecheck_failure
    end
