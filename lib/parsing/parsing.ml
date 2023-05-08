let parse (lexbuf:Lexing.lexbuf) =
    try (Parser.lterm_option Lexer.tokenize lexbuf) with
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

let parse_channel (channel:in_channel) = parse (Lexing.from_channel channel)

let parse_string (str:string) = parse (Lexing.from_string str)