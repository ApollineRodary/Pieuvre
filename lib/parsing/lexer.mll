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
    | "True"        { TRUE }
    | "->"          { ARR }
    | '~'           { TILDE }
    | "/\\"         { AND }
    | "\\/"         { OR }

    | '('           { LPAREN }
    | ')'           { RPAREN }
    | ','           { COMMA }

    | "fst"         { FST }
    | "snd"         { SND }
    | "ig"          { IG }
    | "id"          { ID }
    | "case"        { CASE }

    | '.'           { PERIOD }
    | '&'           { AMP }

    | "absurd"      { ABSURD }
    | "admit"       { ADMIT }
    | "Admitted"    { ADMITTED }
    | "apply"       { APPLY }
    | "assumption"  { ASSUMPTION }
    | "cut"         { CUT }
    | "elim"        { ELIM }
    | "exact"       { EXACT }
    | "exfalso"     { EXFALSO }
    | "intro"       { INTRO }
    | "intros"      { INTROS }
    | "split"       { SPLIT }
    | "Qed"         { QED }

    | "I"           { UNIT }

    | id            { VAR (Lexing.lexeme lexbuf) }
    | typeid        { TYPEVAR (Lexing.lexeme lexbuf) }
    | _             { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
    | eof           { EOF }
