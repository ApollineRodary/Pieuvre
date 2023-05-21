%{
    open Lambda
    open Proof
%}

%token FUN MAPSTO EXF COLON FST SND IG ID CASE
%token TILDE ARR FALSE AND OR TRUE
%token LPAREN RPAREN COMMA
%token AMP PERIOD
%token UNIT
%token <string> VAR
%token <string> TYPEVAR
%token ADMITTED QED PRINT
%token ABSURD ADMIT APPLY ASSUMPTION CUT ELIM EXACT EXFALSO INTRO INTROS LEFT RIGHT SPLIT
%token EOF
%right ARR
%left AND OR
%nonassoc UTILDE

%start <lam> lterm_eof
%start <ty> ptype_eof
%start <lam * lam> alpha_request
%start <lam * ty> typecheck_request
%start <ty> property_request
%start <command> command
%%

(* Lambda terms *)

lterm_eof:
    | lterm EOF
        { $1 }
;

lterm:
    | simple_lterm
        { $1 }
    | application
        { $1 }
    | FUN LPAREN; x=VAR; COLON; a=ptype; RPAREN MAPSTO; t=lterm
        { Abstraction (x, a, t) }
    | EXF LPAREN; t=lterm; COLON; a=ptype; RPAREN
        { Exf (t, a) }
    | LPAREN; m=lterm; COMMA; n=lterm; RPAREN
        { Couple (m, n) }
    | UNIT
        { Unit }
    | FST LPAREN; t=lterm; RPAREN
        { Fst t }
    | SND LPAREN; t=lterm; RPAREN
        { Snd t }
    | IG LPAREN; t=lterm; COMMA; a=ptype; RPAREN
        { Ig (t, a) }
    | ID LPAREN; t=lterm; COMMA; a=ptype; RPAREN
        { Id (t, a) }
    | CASE LPAREN; m=lterm ; COMMA; n=lterm ; COMMA; o=lterm; RPAREN
        { Case (m, n, o) }
;

application:
    | application simple_lterm
        { Application ($1, $2) }
    | simple_lterm simple_lterm
        { Application ($1, $2) }
;

simple_lterm:
    | VAR
        { Var $1 }
    | LPAREN lterm RPAREN
        { $2 }
;

(* Types *)

ptype_eof:
    | ptype EOF
        { $1 }
;

ptype:
    | FALSE
        { False }
    | TRUE
        { True }
    | TYPEVAR
        { TypeVar $1 }
    | TILDE ptype %prec UTILDE
        { Arrow ($2, False) }
    | ptype AND ptype
        { And ($1, $3) }
    | ptype OR ptype
        { Or ($1, $3) }
    | ptype ARR ptype
        { Arrow ($1, $3) }
    | LPAREN ptype RPAREN
        { $2 }
;

(* Request for pieuvre -alpha *)

alpha_request:
    | lterm AMP lterm PERIOD
        { $1, $3 }
;

(* Request for pieuvre -typecheck *)

typecheck_request:
    | lterm COLON ptype PERIOD
        { $1, $3 }
;

(* Request for property to prove *)

property_request:
    | ptype PERIOD
        { $1 }
;

command:
    | ptactic PERIOD
        { UseTactic $1 }
    | ADMITTED PERIOD
        { Admitted }
    | QED PERIOD
        { Qed }
    | PRINT PERIOD
        { Print }
;

ptactic:
    | ABSURD ptype
        { absurd $2 }
    | ADMIT
        { admit }
    | APPLY VAR
        { apply $2 }
    | ASSUMPTION
        { assumption }
    | CUT ptype
        { cut $2 }
    | ELIM VAR
        { elim $2 }
    | EXACT lterm
        { exact $2 }
    | EXFALSO
        { exfalso }
    | INTRO VAR
        { intro $2 }
    | INTROS var_list
        { intros $2 }
    | LEFT
        { left }
    | RIGHT
        { right }
    | SPLIT
        { split }
;

var_list:
    | VAR
        { [$1] }
    | VAR var_list
        { $1 :: $2 }
;
