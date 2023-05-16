type var = string

type ty = 
    | TypeVar of var
    | Arrow of ty * ty
    | False
    | And of ty * ty
    | True
    | Or of ty * ty

type lam =
    | Abstraction of var * ty * lam
    | Application of lam * lam
    | Var of var
    | Exf of lam * ty
    | Hole
    | Couple of lam * lam
    | Fst of lam
    | Snd of lam
    | Unit
    | Ig of lam * ty
    | Id of lam * ty
    | Case of lam * lam * lam 

type env = (var*ty) list
