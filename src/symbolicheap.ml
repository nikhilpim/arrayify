open Variable
open Srkmin
open SrkZ3.Solver

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
  |  Leq of int_term * int_term
  | Eq of int_term * int_term
  | BlockEq of block * block
  | And of formula * formula
  | Or of formula * formula
  | Not of formula

module HeapElem = struct 
  type t = Array of bvar | TrueHeap 
  let hash = Hashtbl.hash
  let compare a b = if hash a < hash b then -1 else if hash a > hash b then 1 else 0
end
module Heap = Set.Make(HeapElem)
type symbolicheap = formula * Heap.t

let rec int_term_to_syntaxterm srk t = 
  match t with 
  | Int i -> Syntax.mk_int srk i
  | Var v -> Variable.var_to_svar srk v
  | Times (i, t) -> Syntax.mk_mul srk [(Syntax.mk_int srk i); (int_term_to_syntaxterm srk t)]
  | Sum (t1, t2) -> Syntax.mk_add srk [(int_term_to_syntaxterm srk t1); (int_term_to_syntaxterm srk t2)]
  | PSub (p1, p2) -> Syntax.mk_sub srk (pointer_term_to_offsetsyntaxterm srk p1) (pointer_term_to_offsetsyntaxterm srk p2)
  | Offset p -> pointer_term_to_offsetsyntaxterm srk p
and pointer_term_to_offsetsyntaxterm srk p =
  match p with 
  | Pointer p -> Variable.pvaroffset_to_svar srk p
  | PointerSum (p, t) -> Syntax.mk_add srk [(pointer_term_to_offsetsyntaxterm srk p); (int_term_to_syntaxterm srk t)]

  let rec pointer_term_to_blocksyntaxterm srk p =
    match p with 
    | Pointer p -> Variable.pvarblock_to_svar srk p
    | PointerSum (p, _) -> pointer_term_to_blocksyntaxterm srk p
  let block_to_syntaxterm srk b =
  match b with
  | Block p -> pointer_term_to_blocksyntaxterm srk p
  | BVar b -> Variable.bvar_to_svar srk b


  let rec formula_to_syntaxformula srk (f : formula) : 'a Syntax.formula = 
  match f with 
  | True -> Syntax.mk_true srk 
  | Leq (t1, t2) -> Syntax.mk_leq srk (int_term_to_syntaxterm srk t1) (int_term_to_syntaxterm srk t2)
  | Eq (t1, t2) -> Syntax.mk_eq srk (int_term_to_syntaxterm srk t1) (int_term_to_syntaxterm srk t2)
  | BlockEq (b1, b2) -> Syntax.mk_eq srk (block_to_syntaxterm srk b1) (block_to_syntaxterm srk b2)
  | And (f1, f2) -> Syntax.mk_and srk [formula_to_syntaxformula srk f1; formula_to_syntaxformula srk f2]
  | Or (f1, f2) -> Syntax.mk_or srk [formula_to_syntaxformula srk f1; (formula_to_syntaxformula srk f2)]
  | Not f1 -> Syntax.mk_not srk (formula_to_syntaxformula srk f1)

let empty_sheap = True, Heap.empty

let sheap_equals (f1, h1) (f2, h2) = Heap.equal h1 h2 && (f1 = f2 || (
    let srk = Global.srk in
    let solver = Global.solver in 
    let s1 = formula_to_syntaxformula srk f1 in
    let s2 = formula_to_syntaxformula srk f2 in
    reset solver; add solver [s1; Syntax.mk_not srk s2];
    let result = check solver in 
    match result with 
    | `Sat -> false
    | `Unsat -> (
      reset solver; add solver [s2; Syntax.mk_not srk s1];
      let result = check solver in 
      match result with 
      | `Sat -> false
      | `Unsat -> true
      | `Unknown -> raise (Failure "Unknown equivalence from Z3")
      )
    | `Unknown -> raise (Failure "Unknown equivalence from Z3")
))

let sheap_single_b (f, h : symbolicheap) (p : pvar) : bvar option = 
  let s = formula_to_syntaxformula Global.srk f in 
  match Heap.find_first_opt (fun e -> 
    match e with 
    | Array b -> (
      let b = Variable.bvar_to_svar Global.srk b in 
      let p = Variable.pvarblock_to_svar Global.srk p in 
      let eq = Syntax.mk_eq Global.srk b p in
      reset Global.solver; add Global.solver [s; Syntax.mk_not Global.srk eq];
      match check Global.solver with 
      | `Sat -> false
      | `Unsat -> true
      | `Unknown -> raise (Failure "Unknown equivalence from Z3"))
    | _ -> false
    ) h with 
  | Some (Array b) -> Some b
  | _ -> None
