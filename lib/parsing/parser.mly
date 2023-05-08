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
%token TILDE
%token ARR
%token AMP
%token PERIOD
%token FALSE
%start <lam option> lterm_option
%start <ty option> ptype_option
%start <(lam * lam) option> alpha_request
%start <(lam * ty) option> typecheck_request
%%

(* Lambda terms *)

lterm_option:
    | lterm; EOF
        { Some $1 }
    | EOF
        { None }
;

lterm:
    | simple_lterm
        { $1 }
    | application
        { $1 }
    | FUN; LPAREN; x=VAR; COLON; a=ptype; RPAREN; MAPSTO; t=lterm
        { Abstraction (x, a, t) }
    | EXF; LPAREN; t=lterm; COLON; a=ptype; RPAREN
        { Exf (t, a) }
;

application:
    | m = application; n = simple_lterm
        { Application (m, n) }
    | m = simple_lterm; n = simple_lterm
        { Application (m, n) }
;

simple_lterm:
    | VAR
        { Var $1 }
    | LPAREN lterm RPAREN
        { $2 }
;

(* Types *)

ptype_option:
    | t = ptype; EOF
        { Some t }
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
    | VAR
        { TypeVar $1 }
    | TILDE; simple_type
        { Arrow ($2, False) }
    | LPAREN ptype RPAREN
        { $2 }
;

type_arrow:
    | a = type_arrow; ARR; b = simple_type
        { Arrow (a, b) }
    | a = simple_type; ARR; b = simple_type
        { Arrow (a, b) }
;

(* Request for pieuvre -alpha *)

alpha_request:
    | m = lterm; AMP; n = lterm; PERIOD
        { Some (m, n) }
;

(* Request for pieuvre -typecheck *)

typecheck_request:
    | l = lterm; COLON; t = ptype; PERIOD
        { Some (l, t) }
;
