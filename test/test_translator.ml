open Arrayify
open Translator
open Variable 
open Symbolicheap

module Heap = Set.Make(HeapElem)

let () = (
  print_string "\n\n\nRunning Translation text...\n\n\n";
  let b = new_bvar ~v:0 () in 
  let p = new_pvar ~v:"%0" () in 
  let formula = And (BlockEq (Block (Pointer p), BVar b), Eq (Offset (Pointer p), Int 0)) in 
  let heap = Heap.singleton (Array b) in 
  let sheap = (formula, heap) in
  translate "/Users/np6641/dev/arrayify/test/files/min.c" sheap 
)

