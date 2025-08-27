open Arrayify
open Translator
open Variable 
open Symbolicheap

module Heap = Set.Make(HeapElem)

let min_test () = (
  print_string "\n\n\nRunning Translation on test/files/min.c...\n\n\n";
  let b = new_bvar 0 in 
  let p = new_pvar "%0" in 
  let formula = And (BlockEq (Block (Pointer p), BVar b), Eq (Offset (Pointer p), Int 0)) in 
  let heap = Heap.singleton (Array b) in 
  let sheap = (formula, heap) in
  translate "/Users/np6641/dev/arrayify/test/files/min.c" sheap
)

let comp_test () = (
  print_string "\n\n\nRunning Translation on test/files/comp.c...\n\n\n";
  let b = new_bvar 0 in 
  let p = new_pvar "%1" in 
  let formula = And (BlockEq (Block (Pointer p), BVar b), Eq (Offset (Pointer p), Int 0)) in 
  let heap = Heap.singleton (Array b) in 
  let sheap = (formula, heap) in
  translate "/Users/np6641/dev/arrayify/test/files/comp.c" sheap
)

let basic_test () = (
  print_string "\n\n\nRunning Translation on test/files/basic.c...\n\n\n";
  let formula = True in 
  let heap = Heap.empty in 
  let sheap = (formula, heap) in
  translate "/Users/np6641/dev/arrayify/test/files/basic.c" sheap
)

let rotation_test () = (
  print_string "\n\n\nRunning Translation on test/files/rotation.c...\n\n\n";
  let formula = True in 
  let heap = Heap.empty in 
  let sheap = (formula, heap) in
  translate "/Users/np6641/dev/arrayify/test/files/rotation.c" sheap
)

let () = (
   let _ = min_test, comp_test, basic_test, rotation_test in
   (* min_test (); *)
   (* comp_test (); *)
   (* basic_test (); *)
   (* rotation_test (); *)
   ()
)

