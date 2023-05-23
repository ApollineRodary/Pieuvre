open Lexing

let parse (lex : lexbuf) (start) =
    try Some (start Lexer.tokenize lex) with
    | Lexer.SyntaxError msg ->
        begin
            print_endline ("Syntax error: " ^ msg);
            None
        end
    | Parser.Error -> None

let lam_of_channel (channel:in_channel) = parse (from_channel channel) (Parser.lterm_eof)

let lam_of_string (str:string) = parse (from_string str) (Parser.lterm_eof)

let ty_of_channel (channel:in_channel) = parse (from_channel channel) (Parser.ptype_eof)

let ty_of_string (str:string) = parse (from_string str) (Parser.ptype_eof)
