open Symbolicheap
open Llvmutil
open Boogieir
open Llvm

module LlvmGraph = Graph.Persistent.Digraph.ConcreteLabeled(LlvmNode)(LlvmEdge)
module BGraph = Graph.Persistent.Digraph.ConcreteLabeled(BGNode)(BGEdge)

module LLvmNodeSet = Set.Make(LlvmNode)

let symbolic_update_instr (instr : llvalue) (cond : symbolicheap) : symbolicheap * boogie_instr list = 
  match instr_opcode instr with 
    | 	Opcode.Invalid -> raise (Failure "Not implemented") 	(*	
    Not an instruction
      *)
    | 	Opcode.Ret -> cond, [] 	(*	
    Terminator Instructions
      *)
    | 	Opcode.Br -> raise (Failure "Not implemented")
    | 	Opcode.Switch -> raise (Failure "Not implemented")
    | 	Opcode.IndirectBr -> raise (Failure "Not implemented")
    | 	Opcode.Invoke -> raise (Failure "Not implemented")
    | 	Opcode.Invalid2 -> raise (Failure "Not implemented")
    | 	Opcode.Unreachable -> raise (Failure "Not implemented")
    | 	Opcode.Add  -> raise (Failure "Not implemented")	(*	
    Standard Binary Operators
      *)
    | 	Opcode.FAdd -> raise (Failure "Not implemented")
    | 	Opcode.Sub -> raise (Failure "Not implemented")
    | 	Opcode.FSub -> raise (Failure "Not implemented")
    | 	Opcode.Mul -> raise (Failure "Not implemented")
    | 	Opcode.FMul -> raise (Failure "Not implemented")
    | 	Opcode.UDiv -> raise (Failure "Not implemented")
    | 	Opcode.SDiv -> raise (Failure "Not implemented")
    | 	Opcode.FDiv -> raise (Failure "Not implemented")
    | 	Opcode.URem -> raise (Failure "Not implemented")
    | 	Opcode.SRem -> raise (Failure "Not implemented")
    | 	Opcode.FRem -> raise (Failure "Not implemented")
    | 	Opcode.Shl  -> raise (Failure "Not implemented")	(*	
    Logical Operators
      *)
    | 	Opcode.LShr -> raise (Failure "Not implemented")
    | 	Opcode.AShr -> raise (Failure "Not implemented")
    | 	Opcode.And -> raise (Failure "Not implemented")
    | 	Opcode.Or -> raise (Failure "Not implemented")
    | 	Opcode.Xor -> raise (Failure "Not implemented")
    | 	Opcode.Alloca  -> raise (Failure "Not implemented")	(*	
    Memory Operators
      *)
    | 	Opcode.Load -> raise (Failure "Not implemented")
    | 	Opcode.Store -> let value = operand instr 0 |> string_of_llvalue in
                    assert (String.sub value 0 4 = "i32 ");
                    let _int_value = String.sub value 4 (String.length value - 4) in
                    let pointer_string = operand instr 1 |> string_of_llvalue in
                    assert (String.sub pointer_string 0 4 = "ptr ");
                    let _pointer = String.sub pointer_string 4 (String.length pointer_string - 4) in
                    let _boogie_instrs = [] in 
                    raise (Failure "Partially Implemented")  
    | 	Opcode.GetElementPtr -> raise (Failure "Not implemented")
    | 	Opcode.Trunc -> raise (Failure "Not implemented") 	(*	
    Cast Operators
      *)
    | 	Opcode.ZExt -> raise (Failure "Not implemented")
    | 	Opcode.SExt -> raise (Failure "Not implemented")
    | 	Opcode.FPToUI -> raise (Failure "Not implemented")
    | 	Opcode.FPToSI -> raise (Failure "Not implemented")
    | 	Opcode.UIToFP -> raise (Failure "Not implemented")
    | 	Opcode.SIToFP -> raise (Failure "Not implemented")
    | 	Opcode.FPTrunc -> raise (Failure "Not implemented")
    | 	Opcode.FPExt -> raise (Failure "Not implemented")
    | 	Opcode.PtrToInt -> raise (Failure "Not implemented")
    | 	Opcode.IntToPtr -> raise (Failure "Not implemented")
    | 	Opcode.BitCast -> raise (Failure "Not implemented")
    | 	Opcode.ICmp  -> raise (Failure "Not implemented")	(*	
    Other Operators
      *)
    | 	Opcode.FCmp -> raise (Failure "Not implemented")
    | 	Opcode.PHI -> raise (Failure "Not implemented")
    | 	Opcode.Call -> raise (Failure "Not implemented")
    | 	Opcode.Select -> raise (Failure "Not implemented")
    | 	Opcode.UserOp1 -> raise (Failure "Not implemented")
    | 	Opcode.UserOp2 -> raise (Failure "Not implemented")
    | 	Opcode.VAArg -> raise (Failure "Not implemented")
    | 	Opcode.ExtractElement -> raise (Failure "Not implemented")
    | 	Opcode.InsertElement -> raise (Failure "Not implemented")
    | 	Opcode.ShuffleVector -> raise (Failure "Not implemented")
    | 	Opcode.ExtractValue -> raise (Failure "Not implemented")
    | 	Opcode.InsertValue -> raise (Failure "Not implemented")
    | 	Opcode.Fence -> raise (Failure "Not implemented")
    | 	Opcode.AtomicCmpXchg -> raise (Failure "Not implemented")
    | 	Opcode.AtomicRMW -> raise (Failure "Not implemented")
    | 	Opcode.Resume -> raise (Failure "Not implemented")
    | 	Opcode.LandingPad -> raise (Failure "Not implemented")
    |   _ -> raise (Failure "Not implemented")


(* If Llvm basic block [weight] is executed from state [precondition], the output (postcondition, instrs) should be the post state [postcondition] and the equivalent translation in Boogie [instrs] *)
let symbolic_update (weight: LlvmEdge.t) (precondition : symbolicheap) : symbolicheap * boogie_instr list = 
  fold_left_instrs (fun (cond, blist) instr -> let post, boogie_translation = symbolic_update_instr instr cond in post, blist @ boogie_translation) (precondition, []) weight 

(* Given a state [postcondition], computes most precise widening in finite class *)
let widen (postcondition : symbolicheap) : symbolicheap = 
  let _ = postcondition in 
  raise (Failure "Not implemented")

(* Checks if [from] entails [towards] up to arbitrary permutation of the block variables. Returns None if not, or otherwise Some (to, instrs) where instrs encodes the permutation in Boogie *)
let check_rotate_entails (from : BGNode.t) (towards : BGNode.t) : (BGNode.t * boogie_instr list) option = 
  let _ = (from, towards) in 
  raise (Failure "Not implemented")

let execute (entry : LlvmNode.t) (graph : LlvmGraph.t) (precondition : symbolicheap) : BGraph.t = 
  (* For now, widening points will be computed via backedges *) 
   let rec go worklist bgraph = 
    if worklist = [] then bgraph else (
      let (repeat_ct, node, precondition) = List.hd worklist in 
      let worklist = List.tl worklist in
      let worklist, bgraph = LlvmGraph.fold_succ_e (fun (src, weight, tgt) (worklist, bgraph) -> 
            let postcondition, boogie_instrs = symbolic_update (weight) precondition in 
            let post_repeat = BGraph.fold_vertex (fun (r, l, _) m -> if LlvmNode.equal l tgt then max m (r + 1) else m) bgraph (0) in 
            if (LlvmNode.compare src tgt < 0) then 
              let widened_postcondition = widen postcondition in
              match BGraph.fold_vertex (fun node find -> match find with Some _ -> find | None -> check_rotate_entails (post_repeat, tgt, widened_postcondition) node) bgraph None with 
              | None -> 
                let bgraph = BGraph.add_edge_e bgraph ((repeat_ct, node, precondition), boogie_instrs, (post_repeat, tgt, widened_postcondition)) in 
                (post_repeat, tgt, widened_postcondition) :: worklist, bgraph
              | Some (repeat, rotation) ->
                let bgraph = BGraph.add_edge_e bgraph ((repeat_ct, node, precondition), boogie_instrs @ rotation, repeat) in
                worklist, bgraph
            else (
            let bgraph = BGraph.add_edge_e bgraph ((repeat_ct, node, precondition), boogie_instrs, (post_repeat, tgt, postcondition)) in 
            (post_repeat, tgt, postcondition) :: worklist, bgraph)
           ) graph node (worklist, bgraph) in 
      go worklist bgraph )
  in 
  go [0, entry, precondition] BGraph.empty
