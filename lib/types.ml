type var = string

type ty = 
    | TypeVar of var
    | Arrow of ty*ty
    | False

type lam =
    | Abstraction of var*ty*lam
    | Application of lam*lam
    | Var of var
    | Exf of lam*ty
