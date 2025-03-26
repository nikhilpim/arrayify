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
    Leq of int_term * int_term
  | Eq of int_term * int_term
  | BlockEq of block * block
  | And of formula * formula
  | Or of formula * formula
  | Not of formula
type heap = Emp | Array of block | Star of heap * heap | TrueHeap
type symbolicheap = formula * heap
