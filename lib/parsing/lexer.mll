{
    open Parser
    exception SyntaxError of string
}

let whitespace = [' ' '\t' '\r' '\n']+
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule tokenize = parse
    | whitespace    { tokenize lexbuf }
    | "exf"         { EXF }
    | "fun"         { FUN }
    | "=>"          { MAPSTO }
    | '('           { LPAREN }
    | ')'           { RPAREN }
    | ':'           { COLON }
    | id            { VAR (Lexing.lexeme lexbuf) }
    | _             { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
    | eof           { EOF }