let parse (lexbuf:Lexing.lexbuf) =
    try (Parser.lterm_option Lexer.tokenize lexbuf) with
    | Lexer.SyntaxError msg -> (
        print_endline ("Syntax error: " ^ msg);
        None
    )
    | Parser.Error -> (
        print_endline "Parsing error";
        None
    )

let parse_channel (channel:in_channel) = parse (Lexing.from_channel channel)
let parse_string (str:string) = parse (Lexing.from_string str)