open Symbolicheap
open Llvmutil
open Boogieir
open Llvm
open Variable
module LlvmGraph = Graph.Persistent.Digraph.ConcreteLabeled(LlvmNode)(LlvmEdge)
module BGraph = Graph.Persistent.Digraph.ConcreteLabeled(BGNode)(BGEdge)

module LLvmNodeSet = Set.Make(LlvmNode)


let extract_int_term (v : llvalue) : int_term = 
  let s = string_of_llvalue v in 
  match (String.sub s 0 4) with 
  | "i32 " -> 
    let value_str = String.sub s 4 (String.length s - 4) in
    if String.get value_str 0 = '%' then Var (new_var ~v:value_str ()) 
      else Int (String.sub s 4 (String.length s - 4) |> int_of_string)
  | _ -> raise (Failure "unimplemented int term extraction")

let symbolic_update_instr (instr : llvalue) (cond : symbolicheap) (_src : LlvmNode.t) (tgt : LlvmNode.t) : symbolicheap * boogie_instr list = 
  match instr_opcode instr with 
    | 	Opcode.Invalid -> raise (Failure "Not implemented") 	(*	
    Not an instruction
      *)
    | 	Opcode.Ret -> cond, [] 	(*	
    Terminator Instructions
      *)
    | 	Opcode.Br -> (
      match get_branch instr with 
        | Some (`Conditional (_, taken_block, not_taken_block))  -> (
            let cmp_flag_name = (string_of_llvalue instr |> String.trim |> String.split_on_char ' ' |> List.nth) 2 in 
            let cmp_flag_name = String.sub cmp_flag_name 0 (String.length cmp_flag_name - 1) in
            let cmp_flag = new_var ~v:cmp_flag_name () in
            let taken = Llvmutil.block_id taken_block in 
            let not_taken = Llvmutil.block_id not_taken_block in
            if ((LlvmNode.llvm_identifier tgt) = taken) then (
              let sheap_taken = Symbolicheap.Eq (Var cmp_flag, Int 1) in 
              let boogie_taken = [Assume (Eq (Var (boogie_var_of_var cmp_flag), Int 1))] in 
              let f, h = cond in 
              (Symbolicheap.And (f, sheap_taken), h), boogie_taken
              ) else if (LlvmNode.llvm_identifier tgt = not_taken) then (
                let sheap_not_taken = Symbolicheap.Eq (Var cmp_flag, Int 0) in
                let boogie_not_taken = [Assume (Eq (Var (boogie_var_of_var cmp_flag), Int 0))] in
                let f, h = cond in 
                (Symbolicheap.And (f, sheap_not_taken), h), boogie_not_taken
              ) else (
                raise (Failure ("Symbolic update stepping to LLVM identifier that is not in branch statement: Branches are "^(string_of_int taken)^" and "^(string_of_int not_taken)^" but tgt is "^(LlvmNode.name tgt)))
              ))
        | Some (`Unconditional _) -> cond, []
        | _ -> raise (Failure "Not implemented") 
      )
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
    | 	Opcode.Store -> (let value = operand instr 0 |> extract_int_term in 
                    let pointer_string = operand instr 1 |> string_of_llvalue in
                    assert (String.sub pointer_string 0 4 = "ptr ");
                    let pointer_pvar = (new_pvar  ~v:(String.sub pointer_string 4 (String.length pointer_string - 4)) ()) in 
                    match sheap_single_b cond pointer_pvar with 
                    | Some b -> let boogie_instrs = [
                        Assert (Leq (Int 0, Var (boogie_var_of_pvar pointer_pvar))); 
                        Assert (Leq (Var (boogie_var_of_pvar pointer_pvar), Var (boogie_length_of_boogie_avar (boogie_avar_of_bvar b)))); 
                        AWrite (boogie_avar_of_bvar b, Var (boogie_var_of_pvar pointer_pvar), boogie_term_of_int_term value)
                      ] in 
                        cond, boogie_instrs 
                    | None -> (
                      let boogie_instrs = [ Assert (Not (True))] in 
                      true_sheap, boogie_instrs
                    )
                    )
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
    | 	Opcode.ICmp  -> (
      let lhs = operand instr 0 |> extract_int_term in
      let rhs = operand instr 1 |> extract_int_term in
      (* This is a hacky way of extracting the variable that a operation assigns to. *)
      let cmp_flag_name = string_of_llvalue instr |> String.trim |> String.split_on_char ' ' |> List.hd in 
      let cmp_flag = new_var ~v:cmp_flag_name () in 
      let op, bop = match icmp_predicate instr with 
        | Some Llvm.Icmp.Eq -> Symbolicheap.Eq (lhs, rhs), Eq (boogie_term_of_int_term lhs, boogie_term_of_int_term rhs)
        | Some Llvm.Icmp.Ne -> Not (Eq (lhs, rhs)), Not (Eq (boogie_term_of_int_term lhs, boogie_term_of_int_term rhs))
        | Some Llvm.Icmp.Sgt -> Not (Leq (lhs, rhs)), Not (Leq (boogie_term_of_int_term lhs, boogie_term_of_int_term rhs))
        | Some Llvm.Icmp.Sge -> Leq (rhs, lhs), Leq (boogie_term_of_int_term rhs, boogie_term_of_int_term lhs)
        | Some Llvm.Icmp.Slt -> Not (Leq (rhs, lhs)), Not (Leq (boogie_term_of_int_term rhs, boogie_term_of_int_term lhs))
        | Some Llvm.Icmp.Sle -> Leq (lhs, rhs),  Leq (boogie_term_of_int_term lhs, boogie_term_of_int_term rhs)
        | _ -> print_string ((string_of_llvalue instr)); raise (Failure "Not implemented")
      in
      let boogie_instrs = [
        IteAssign ((boogie_var_of_var cmp_flag), bop, Int 1, Int 0);
      ]
      in
      let f, h = (quantify_out_var cond cmp_flag) in
      let op_or_not_op = Symbolicheap.Or (And (op, Eq (Var cmp_flag, Int 1)), And (Not op, Eq (Var cmp_flag, Int 0))) in  
      let postcond = Symbolicheap.And (op_or_not_op, f), h in 
      postcond, boogie_instrs
)
        (*	
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
let symbolic_update (weight: LlvmEdge.t) (precondition : symbolicheap) (src : LlvmNode.t) (tgt : LlvmNode.t) : symbolicheap * boogie_instr list = 
  fold_left_instrs (fun (cond, blist) instr -> let post, boogie_translation = symbolic_update_instr instr cond src tgt in post, blist @ boogie_translation) (precondition, []) weight 

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
            let postcondition, boogie_instrs = symbolic_update weight precondition src tgt in 
            let post_repeat = BGraph.fold_vertex (fun (r, l, _) m -> if LlvmNode.equal l tgt then max m (r + 1) else m) bgraph (0) in 
            if ((not (LlvmNode.equal tgt Llvmutil.sink)) && LlvmNode.compare tgt src < 0) then (
              print_string (LlvmNode.name src); print_string (LlvmNode.name tgt);
              let widened_postcondition = widen postcondition in
              match BGraph.fold_vertex (fun node find -> match find with Some _ -> find | None -> check_rotate_entails (post_repeat, tgt, widened_postcondition) node) bgraph None with 
              | None -> 
                let bgraph = BGraph.add_edge_e bgraph ((repeat_ct, node, precondition), boogie_instrs, (post_repeat, tgt, widened_postcondition)) in 
                (post_repeat, tgt, widened_postcondition) :: worklist, bgraph
              | Some (repeat, rotation) ->
                let bgraph = BGraph.add_edge_e bgraph ((repeat_ct, node, precondition), boogie_instrs @ rotation, repeat) in
                worklist, bgraph )
            else (
            let bgraph = BGraph.add_edge_e bgraph ((repeat_ct, node, precondition), boogie_instrs, (post_repeat, tgt, postcondition)) in 
            (post_repeat, tgt, postcondition) :: worklist, bgraph)
           ) graph node (worklist, bgraph) in 
      go worklist bgraph )
  in 
  go [0, entry, precondition] BGraph.empty
