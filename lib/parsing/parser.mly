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
        { let (a, b) = $1 in UseTactic (a, b) }
    | ADMITTED PERIOD
        { Admitted }
    | QED PERIOD
        { Qed }
    | PRINT PERIOD
        { Print }
;

ptactic:
    | ABSURD ty = ptype
        { (absurd ty, "absurd " ^ Lambda__Display.string_of_type ty) }
    | ADMIT
        { (admit, "admit") }
    | APPLY var = VAR
        { (apply var, "apply " ^ var) }
    | ASSUMPTION
        { assumption, "assumption" }
    | CUT ty = ptype
        { (cut ty, "cut " ^ Lambda__Display.string_of_type ty) }
    | ELIM var = VAR
        { (elim var, "elim " ^ var) }
    | EXACT t = lterm
        { (exact t, "exact " ^ Lambda__Display.string_of_lam t) }
    | EXFALSO
        { (exfalso, "exfalso") }
    | INTRO var = VAR
        { (intro var, "intro " ^ var) }
    | INTROS vl = var_list
        { (intros vl, List.fold_left (fun x y -> x ^ " " ^ y) "intros" vl) }
    | LEFT
        { (left, "left") }
    | RIGHT
        { (right, "right") }
    | SPLIT
        { (split, "split") }
;

var_list:
    | VAR
        { [$1] }
    | VAR var_list
        { $1 :: $2 }
;
