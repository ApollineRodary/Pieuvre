%{
    open Lambda
    open Proof
%}

%token FUN MAPSTO EXF COLON
%token TILDE ARR FALSE AND OR
%token LPAREN RPAREN COMMA
%token AMP PERIOD
%token <string> VAR
%token <string> TYPEVAR
%token ABSURD ADMIT ADMITTED APPLY ASSUMPTION CUT ELIM EXACT EXFALSO INTRO INTROS QED
%token EOF
%left ARR
%left AND
%nonassoc UTILDE

%start <lam> lterm_eof
%start <ty> ptype_eof
%start <lam * lam> alpha_request
%start <lam * ty> typecheck_request
%start <ty> property_request
%start <tactic> ptactic
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
    | TYPEVAR
        { TypeVar $1 }
    | TILDE ptype %prec UTILDE
        { Arrow ($2, False) }
    | ptype AND ptype
        { And ($1, $3) }
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

ptactic:
    | ABSURD ptype PERIOD
        { absurd $2 }
    | ADMIT PERIOD
        { admit }
    | ADMITTED PERIOD
        { admitted }
    | ASSUMPTION PERIOD
        { assumption }
    | CUT ptype PERIOD
        { cut $2 }
    | ELIM VAR PERIOD
        { elim $2 }
    | EXACT lterm PERIOD
        { exact $2 }
    | EXFALSO PERIOD
        { exfalso }
    | INTRO VAR PERIOD
        { intro $2 }
    | INTROS var_list PERIOD
        { intros $2 }
    | APPLY VAR PERIOD
        { apply $2 }
    | QED PERIOD
        { qed }
;

var_list:
    | VAR
        { [$1] }
    | VAR var_list
        { $1 :: $2 }
;
