open Types

let print_type (t : ty) : unit =
  let rec aux (t:ty) (p:bool) = match t with
      | Arrow (t1, t2) -> begin
          if p then print_string "(";
          aux t1 true;
          print_string (" -> ");
          aux t2 false;
          if p then print_string ")";
      end
      | TypeVar x -> print_string x;
      | False -> print_string "False"
  in aux t false

let print_lam (l:lam): unit =
  let rec aux (l:lam) (p:bool) = match l with
      | Abstraction (v, t, l) -> begin
          if p then print_string "(";
          print_string ("fun (" ^ v ^ ":");
          print_type t;
          print_string (") => ");
          aux l true;
          if p then print_string ")";
      end
      | Application (m, n) -> begin
          if p then print_string "(";
          aux m true;
          print_string " ";
          aux n true;
          if p then print_string ")";
      end
      | Var x -> print_string x;
      | Exf (l, t) -> begin
          print_string "exf(";
          aux l false;
          print_string (":");
          print_type t;
          print_string (")");
      end
  in aux l false
