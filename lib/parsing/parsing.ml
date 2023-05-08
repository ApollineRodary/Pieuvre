let parse (lexbuf:Lexing.lexbuf) (start) =
    try (start Lexer.tokenize lexbuf) with
    | Lexer.SyntaxError msg ->
        begin
            print_endline ("Syntax error: " ^ msg);
            None
        end
    | Parser.Error ->
        begin
            print_endline "Parsing error";
            None
        end

let lam_of_channel (channel:in_channel) = parse (Lexing.from_channel channel) (Parser.lterm_option)

let lam_of_string (str:string) = parse (Lexing.from_string str) (Parser.lterm_option)

let ty_of_channel (channel:in_channel) = parse (Lexing.from_channel channel) (Parser.ptype_option)

let ty_of_string (str:string) = parse (Lexing.from_string str) (Parser.ptype_option)
