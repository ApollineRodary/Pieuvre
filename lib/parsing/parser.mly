%{
    open Lambda
%}

%token FUN
%token LPAREN
%token RPAREN
%token <string> VAR
%token COLON
%token MAPSTO
%token EXF
%token EOF
%start <lam option> main
%%

main:
    | t = term; EOF
        { Some t }
    | EOF
        { None }
;

term:
    | t = simple_term
        { t }
    | a = application
        { a }
    | FUN; LPAREN; x=VAR; COLON; a=VAR; RPAREN; MAPSTO; t=term
        { Abstraction (x, a, t) }
    | EXF; LPAREN; t=term; COLON; a=VAR; RPAREN
        { Exf (t, a) }
;

application:
    | m = application; n = simple_term
        { Application (m, n) }
    | m = simple_term; n = simple_term
        { Application (m, n) }
;

simple_term:
    | x = VAR
        { Var x };