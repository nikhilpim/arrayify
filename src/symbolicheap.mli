open Variable

type int_term =
    Int of int
  | Var of var
  | Times of int * int_term
  | Sum of int_term * int_term
  | PSub of pointer_term * pointer_term
  | Offset of pointer_term
and pointer_term = Pointer of pvar | PointerSum of pointer_term * int_term
type block = Block of pointer_term | BVar of bvar
type formula =
  | True
  | Leq of int_term * int_term
  | Eq of int_term * int_term
  | BlockEq of block * block
  | And of formula * formula
  | Or of formula * formula
  | Not of formula

  module HeapElem : sig 
    type t = Array of bvar | TrueHeap 
    val compare : t -> t -> int
  end


type symbolicheap = formula * Set.Make(HeapElem).t

val true_sheap : symbolicheap

val empty_sheap : symbolicheap
val sheap_equals : symbolicheap -> symbolicheap -> bool
val sheap_single_b : symbolicheap -> pvar -> bvar option 

val quantify_out_var : symbolicheap -> var -> symbolicheap