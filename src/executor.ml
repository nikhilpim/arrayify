open Symbolicheap
open Llvmutil
open Boogieir
open Llvm
open Variable
module LlvmGraph = Graph.Persistent.Digraph.Concrete(LlvmNode)
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

let symbolic_update_instr (instr : llvalue) (cond : symbolicheap) : symbolicheap * boogie_instr list = 
  match instr_opcode instr with 
    | 	Opcode.Invalid -> raise (Failure "Not implemented") 	(*	
    Not an instruction
      *)
    | 	Opcode.Ret -> cond, [] (*	
    Terminator Instructions
      *)
    | 	Opcode.Br -> cond, []
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
let symbolic_update (src : LlvmNode.t) (precondition : symbolicheap) : symbolicheap * boogie_instr list = 
  fold_left_instrs (fun (cond, blist) instr -> let post, boogie_translation = symbolic_update_instr instr cond in post, (blist @ boogie_translation)) (precondition, []) (LlvmNode.llvm_block src) 

(* Given a state [postcondition], computes most precise widening in finite class *)
let widen (postcondition : symbolicheap) : symbolicheap = 
  let _ = postcondition in 
  raise (Failure "Not implemented")

(* Checks if [from] entails [towards] up to arbitrary permutation of the block variables. Returns None if not, or otherwise Some (to, instrs) where instrs encodes the permutation in Boogie *)
let check_rotate_entails (from : symbolicheap) (towards : symbolicheap) (_recipient : BGNode.t): (BGNode.t * boogie_instr list) option = 
  let _ = (from, towards) in 
  raise (Failure "Not implemented")

let gen_boogie_edge (from : BGNode.t) (towards : BGNode.t) (rotation : boogie_instr list option) : BGEdge.t = 
  let (_, from_node, _, _) = from in 
  let (_, towards_node, _, _) = towards in
  match block_terminator (LlvmNode.llvm_block from_node) with
  | None -> None, rotation
  | Some terminator -> if (is_conditional terminator) then 
  (
    match get_branch terminator with 
    | Some (`Conditional (_, taken_block, not_taken_block)) -> (
      let cmp_flag_name = (string_of_llvalue terminator |> String.trim |> String.split_on_char ' ' |> List.nth) 2 in 
            let cmp_flag_name = String.sub cmp_flag_name 0 (String.length cmp_flag_name - 1) in
            let cmp_flag = new_var ~v:cmp_flag_name () in
            let branch_condition = if (Llvmutil.block_eq (LlvmNode.llvm_block towards_node) (taken_block)) then ((Eq (Var (boogie_var_of_var cmp_flag), Int 1))) 
            else (if (Llvmutil.block_eq (LlvmNode.llvm_block towards_node) not_taken_block) then ((Eq (Var (boogie_var_of_var cmp_flag), Int 0))) 
            else (raise (Failure "towards not in branch statement"))) in 
      Some branch_condition, rotation
    )
    | Some (`Unconditional _) -> None, rotation
    | None -> raise (Failure "Not implemented")
  )
  else (None, rotation) 

let gen_branch_condition (from : LlvmNode.t) (towards : LlvmNode.t) : formula = 
  match block_terminator (LlvmNode.llvm_block from) with 
  | None -> Symbolicheap.True
  | Some terminator -> if (is_conditional terminator) then 
  (
    match get_branch terminator with 
    | Some (`Conditional (_, taken_block, not_taken_block)) -> (
      let cmp_flag_name = (string_of_llvalue terminator |> String.trim |> String.split_on_char ' ' |> List.nth) 2 in 
      let cmp_flag_name = String.sub cmp_flag_name 0 (String.length cmp_flag_name - 1) in
      let cmp_flag = new_var ~v:cmp_flag_name () in
      let branch_condition = if (Llvmutil.block_eq (LlvmNode.llvm_block towards) (taken_block)) then ((Symbolicheap.Eq (Var cmp_flag, Int 1))) 
      else (if (Llvmutil.block_eq (LlvmNode.llvm_block towards) not_taken_block) then ((Eq (Var cmp_flag, Int 0))) 
      else (raise (Failure "towards not in branch statement"))) in 
      branch_condition
    )
    | Some (`Unconditional _) -> Symbolicheap.True
    | None -> raise (Failure "Not implemented")
  )
  else Symbolicheap.True 


let execute (entry : LlvmNode.t) (graph : LlvmGraph.t) (precondition : symbolicheap) : BGraph.t * BGNode.t = 
  (* For now, widening points will be computed via backedges *) 
   let rec go worklist bgraph edges = 
    if worklist = [] then bgraph, edges else (
      let (repeat_ct, node, precondition) = List.hd worklist in 
      let worklist = List.tl worklist in
      let post, boogie_instrs = symbolic_update node precondition in 
      let (replacement_bnode : BGNode.t) = (repeat_ct, node, precondition, boogie_instrs) in 
      let bgraph = BGraph.add_vertex bgraph replacement_bnode in
      let worklist, edges = LlvmGraph.fold_succ (fun succ (worklist, edges) ->
          let post_with_branch = Symbolicheap.And ((fst post), gen_branch_condition node succ), snd post in
          let new_repeat_ct = BGraph.fold_vertex (fun (r, l, _, _) m -> if LlvmNode.equal l succ then max m (r + 1) else m) bgraph (0) in
          let new_repeat_ct = List.fold_left (fun m (r, l, _) -> if LlvmNode.equal l succ then max m (r + 1) else m) new_repeat_ct worklist in
          if (LlvmNode.compare succ node < 0) then (
            let widened_postcondition = widen post_with_branch in
            let exists_covering_vertex = BGraph.fold_vertex (fun previous_bnode find -> 
              let (_, previous_node, previous_cond, _) = previous_bnode in 
              match (find, LlvmNode.equal previous_node succ) with 
              | (Some _), _ | _, false -> find 
              | None, true -> check_rotate_entails widened_postcondition previous_cond previous_bnode) bgraph None in 
            match exists_covering_vertex with 
              | None -> (new_repeat_ct, succ, widened_postcondition) :: worklist, (replacement_bnode, (new_repeat_ct, succ), None) :: edges
              | Some ((r, n, _, _), rotation) -> worklist, (replacement_bnode, (r, n), Some rotation) :: edges 
          ) else (
            (new_repeat_ct, succ, post_with_branch) :: worklist, (replacement_bnode, (new_repeat_ct, succ), None) :: edges
          )
        ) graph node (worklist, edges) in 
      go worklist bgraph edges
    )
      in 
      let bgraph, edges = go [0, entry, precondition] BGraph.empty [] in 

      BGraph.fold_vertex (fun bnode bgraph -> 
          let relevant_edges = List.filter (fun e -> let (_, (r, n), _) = e in let (r', n', _, _) = bnode in r = r' && LlvmNode.equal n n') edges in
          List.fold_left (fun bgraph (pred, _, rotation) -> 
              BGraph.add_edge_e bgraph (pred, gen_boogie_edge pred bnode rotation, bnode) 
            ) bgraph relevant_edges
      ) bgraph bgraph,
      (0, entry, precondition, symbolic_update entry precondition |> snd)
      
