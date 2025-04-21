open Symbolicheap
open Variable

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

type boogie_instr = Assign of boogie_var * boogie_term | AAssign of boogie_avar * array_term| Assume of boogie_formula | Assert of boogie_formula | Error

module BGNode : sig
  type t = Llvmutil.LlvmNode.t * symbolicheap
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end
module BGEdge : sig
  type t = boogie_instr list 
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val default : t
end

val boogie_term_of_int_term : int_term -> boogie_term 
val boogie_term_of_pointer_term : pointer_term -> boogie_term

val code_of_boogie_graph : Graph.Persistent.Digraph.ConcreteLabeled(BGNode)(BGEdge).t -> string