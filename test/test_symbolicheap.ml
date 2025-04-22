open Arrayify
open Symbolicheap
open Variable
module Heap = Set.Make(HeapElem)



let equality_test () = (
  let b = new_bvar ~v:0 () in
  let p = new_pvar ~v:"p" () in
  let x = new_var ~v:"x" () in
  let formula = And ((Eq (Var x, Int 2), And (BlockEq (Block (Pointer p), BVar b), Eq (Offset (Pointer p), Int 7)))) in
  let heap = Heap.singleton (Array b) in
  let sheap = formula, heap in 

  let formula' = And (Eq ((Var x), (Sum (Int 1, Int 1))), And (BlockEq ((Block (Pointer p), BVar b)), Eq (Offset (Pointer p), (Sum ((Int 3), (Int 4)))))) in
  let sheap' = formula', heap in 

  assert (sheap_equals sheap sheap')
)

let () = (
  equality_test (); ()
)