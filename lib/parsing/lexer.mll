{
    open Parser
    exception SyntaxError of string
}

let whitespace = [' ' '\t' '\r' '\n']+
let id = ['a'-'z']+ ['0'-'9']*
let typeid = ['A'-'Z']+ ['0'-'9']*

rule tokenize = parse
    | whitespace    { tokenize lexbuf }
    | "fun"         { FUN }
    | "exf"         { EXF }
    | "=>"          { MAPSTO }
    | ':'           { COLON }

    | "False"       { FALSE }
    | "->"          { ARR }
    | '~'           { TILDE }
    | "/\\"         { AND }
    | "\\/"         { OR }

    | '('           { LPAREN }
    | ')'           { RPAREN }

    | '.'           { PERIOD }
    | '&'           { AMP }

    | "apply"       { APPLY }
    | "assumption"  { ASSUMPTION }
    | "cut"         { CUT }
    | "exact"       { EXACT }
    | "intro"       { INTRO }
    | "intros"      { INTROS }
    | "apply"       { APPLY }
    | "cut"         { CUT }
    | "exfalso"     { EXFALSO }
    | "elim"        { ELIM }
    | "absurd"      { ABSURD }
    | "admit"       { ADMIT }
    | "Admitted"    { ADMITTED }
    | "Qed"         { QED }

    | id            { VAR (Lexing.lexeme lexbuf) }
    | typeid        { TYPEVAR (Lexing.lexeme lexbuf) }
    | _             { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
    | eof           { EOF }
