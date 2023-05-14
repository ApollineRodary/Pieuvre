%{
    open Lambda
    open Proof
%}

%token FUN MAPSTO EXF COLON
%token TILDE ARR FALSE
%token LPAREN RPAREN
%token AMP PERIOD
%token <string> VAR
%token <string> TYPEVAR
%token ASSUMPTION EXACT INTRO INTROS APPLY CUT ADMIT ADMITTED QED
%token EOF

%start <lam option> lterm_option
%start <ty option> ptype_option
%start <(lam * lam) option> alpha_request
%start <(lam * ty) option> typecheck_request
%start <ty option> property_request
%start <tactic option> ptactic
%%

(* Lambda terms *)

lterm_option:
    | lterm EOF
        { Some $1 }
    | EOF
        { None }
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

ptype_option:
    | ptype EOF
        { Some $1 }
    | EOF
        { None }
;

ptype:
    | simple_type
        { $1 }
    | type_arrow
        { $1 }
;

simple_type:
    | FALSE
        { False }
    | TYPEVAR
        { TypeVar $1 }
    | TILDE simple_type
        { Arrow ($2, False) }
    | LPAREN ptype RPAREN
        { $2 }
;

type_arrow:
    | simple_type ARR type_arrow
        { Arrow ($1, $3) }
    | simple_type ARR simple_type
        { Arrow ($1, $3) }
;

(* Request for pieuvre -alpha *)

alpha_request:
    | lterm AMP lterm PERIOD
        { Some ($1, $3) }
;

(* Request for pieuvre -typecheck *)

typecheck_request:
    | lterm COLON ptype PERIOD
        { Some ($1, $3) }
;

(* Request for property to prove *)

property_request:
    | ptype PERIOD
        { Some $1 }
;

ptactic:
    | tactic_contents PERIOD
        { Some $1 }
;

tactic_contents:
    | ASSUMPTION
        { assumption }
    | EXACT lterm
        { exact $2 }
    | INTRO VAR
        { intro $2 }
    | INTROS var_list
        { intros $2 }
    | APPLY VAR
        { apply $2 }
    | CUT ptype
        { cut $2 }
    | ADMIT
        { admit }
    | QED
        { qed }
    | ADMITTED
        { admitted }
;

var_list:
    | VAR
        { [$1] }
    | VAR var_list
        { $1 :: $2 }
