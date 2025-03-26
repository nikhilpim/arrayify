open Symbolicheap
open Llvm
let execute (func : llvalue) (precondition : symbolicheap) : llvalue = 
  (* Prototype: widening points computed by depth-first numbering for simplicity *)
  let all_blocks = Llvm.fold_left_blocks (fun acc block -> block :: acc) [] func in
  
  
  let worklist = [(List.hd all_blocks, precondition)] in 
  let _ = precondition in 

  let go worklist = 
    if worklist = [] then func else
    (* let (block, (formula, heap)), worklist = List.hd worklist, worklist in *)
    func
  in
  go worklist

