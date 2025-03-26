open Symbolicheap
open Variable

type boogie_graph_node = int

type boogie_term = 
  Int of int
  | Var of boogie_var
  | Times of int * boogie_term
  | Sum of boogie_term * boogie_term
  | Read of boogie_avar * boogie_term

type array_term = 
  Array of boogie_avar | Store of boogie_avar * boogie_term * boogie_term

type boogie_formula = 
    Leq of boogie_term * boogie_term
  | Eq of boogie_term * boogie_term
  | AEq of array_term * array_term
  | And of boogie_formula * boogie_formula
  | Or of boogie_formula * boogie_formula
  | Not of boogie_formula

type boogie_instr = Assign of boogie_var * boogie_term | AAssign of boogie_avar * array_term| Assume of boogie_formula

type boogie_graph = 
  { entry : boogie_graph_node
  ; nodes : (boogie_graph_node * boogie_instr list) list
  ; edges : (boogie_graph_node * boogie_graph_node) list
  }


let rec boogie_term_of_pointer_term (p : pointer_term) = 
  match p with 
  | Pointer p -> Var (boogie_var_of_pvar p)
  | PointerSum (p, t) -> Sum (boogie_term_of_pointer_term p, boogie_term_of_int_term t)
and boogie_term_of_int_term (t: int_term) = 
  match t with 
  | Int i -> Int i
  | Var v -> Var (boogie_var_of_var v)
  | Times (i, t) -> Times (i, boogie_term_of_int_term t)
  | Sum (t1, t2) -> Sum (boogie_term_of_int_term t1, boogie_term_of_int_term t2)
  | Offset p -> boogie_term_of_pointer_term p
  | PSub (p1, p2) -> Sum (boogie_term_of_pointer_term p1, (Times (-1, boogie_term_of_pointer_term p2)))
  
let code_of_boogie_graph = 
  raise (Failure "Not implemented")