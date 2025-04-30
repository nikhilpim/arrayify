open Llvm
open Llvm_irreader
open Variable

let llvm_block_to_string (block : llbasicblock) : string =
  let instrs = fold_left_instrs (fun acc instr -> string_of_llvalue instr :: acc) [] block in
  String.concat "\n" (List.rev instrs)

  let block_eq b1 b2 = 
    let b1_str = llvm_block_to_string b1 in
    let b2_str = llvm_block_to_string b2 in
    String.equal b1_str b2_str

module LlvmNode = struct 
  (* [fst t] is the LLVM label. [snd t] distinguishes between duplicated phi nodes, and should be 0 if it is not a phi node. *)
    type t = int * int 
    let compare (l, p) (l', p') = if Int.compare l l' != 0 then Int.compare l l' else Int.compare p p'
    let equal (l, p) (l', p')  = Int.equal l l' && Int.equal p p'
    let hash a = Hashtbl.hash a
    let name (l, p) = let l_str = if (l = (-1)) then "ret" else string_of_int l in 
      if p = 0 then l_str else "phi"^l_str^"_"^(string_of_int p)
    let llvm_identifier (t : t) = fst t
end

let sink : LlvmNode.t = (-1, 0)

module LlvmEdge  = struct
  type t = llbasicblock 
  let hash = Hashtbl.hash
  let compare a b = Int.compare (hash a) (hash b)
  let equal = block_eq
  let default = block_of_value (const_int (i32_type (create_context ())) 90210)
end

module LlvmGraph = Graph.Persistent.Digraph.ConcreteLabeled(LlvmNode)(LlvmEdge)




let generate_llvm_ir c_file =
  let ctx = (create_context ()) in 
  let llvm_ir_file_unoptimized = Filename.temp_file "output" ".ll" in
  let llvm_ir_file_optimized = Filename.temp_file "output_o" ".ll" in
  let clang_command = Printf.sprintf "clang -Xclang -disable-O0-optnone -O0 -emit-llvm -S %s -o %s" c_file llvm_ir_file_unoptimized in
  let opt_command = Printf.sprintf "opt -passes mem2reg -S %s -o %s" llvm_ir_file_unoptimized llvm_ir_file_optimized in
  let _ = Sys.command clang_command in
  let _ = Sys.command opt_command in
  let mem_buffer = MemoryBuffer.of_file llvm_ir_file_optimized in
  parse_ir ctx mem_buffer

(* A phi node is a LlvmNode where some outgoing edge has a phi instruction in it. 
LIMITATION: we currently only handle phi nodes which have only one outgoing edge and which only contain one phi instruction. 
This transformation duplicates the phi node, replaces the phi instruction with the relevant set, and fixes the predecessor edges.
*)
let duplicate_phi_nodes (g : LlvmGraph.t) : LlvmGraph.t = 
  let phi_nodes = LlvmGraph.fold_vertex (fun v acc ->
      let successor_count = LlvmGraph.out_degree g v in  
      let is_phi = LlvmGraph.fold_succ_e (fun (_, w, _) acc -> 
          fold_left_instrs (fun acc instr -> 
            match instr_opcode instr with 
            | Opcode.PHI -> (if acc then raise (Failure "Multiple phi instructions in a single block") else if successor_count > 1 then raise (Failure "phi node with multiple successors") else true) 
            | _ -> acc) acc w
        ) g v false in
      if is_phi then v :: acc else acc
    ) g [] in 
    List.fold_left (fun g phi_node -> 
        let with_new_edges, _ = LlvmGraph.fold_pred_e (fun (pred, w, _) (g, index) -> 
            let new_node = (fst phi_node, index) in
            LlvmGraph.add_edge_e g (pred, w, new_node), index + 1
          ) g phi_node (g, 1) in
        LlvmGraph.remove_vertex with_new_edges phi_node 
      ) g phi_nodes

let block_id block = 
  let func = block_parent block in 
  let entry = entry_block func in
  if block_eq entry block then 0 else (
  let block_str = string_of_llvalue (value_of_block block) |> String.trim |> String.split_on_char ' ' |> List.hd in
  let id_str = String.sub block_str 0 (String.length block_str - 1) in
  int_of_string id_str)



let generate_llvm_graph_from_ir m = 
  (* For now, we require that the input is a single function. *)
  let func = fold_left_functions (fun acc func -> 
    match acc with 
    | [] -> [func]
    | _ -> raise (Failure "Multiple functions in LLVM IR not supported.")
  ) [] m |> List.hd in 

  let params = fold_left_params (fun acc param -> 
    let param_str = string_of_llvalue param in
    let header = String.sub param_str 0 4 in
    let name = String.sub param_str 4 (String.length param_str - 4) in
    if (header = "ptr ") then 
      let pvar = new_pvar ~v:name () in
      (Pointer pvar) :: acc
    else if (header = "i32 ") then 
      let var = new_var ~v:name () in 
      (Variable var) :: acc 
    else 
      raise (Failure "Unknown parameter type")
    ) [] func in 

  (* The entry is the first block of this function that the LLVM module returns *)
  let all_blocks = fold_left_blocks (fun acc block -> block :: acc) [] func 
    |> List.map (fun e -> ((block_id e, 0), e)) in
  let g = List.fold_left (fun graph (i, _) -> LlvmGraph.add_vertex graph i) LlvmGraph.empty all_blocks in
  (* (-1, 0) is the designated end vertex. *)
  let g = LlvmGraph.add_vertex g (-1, 0) in
  let g = List.fold_left (fun graph (i, block) -> 
      let terminator = block_terminator block in
      match terminator with
      | None -> graph
      | Some term -> 
        (* If we're looking at a return, we want to add an edge to a designated end location (-1). I assume that returns are all terminators with no successors. *)
        if num_successors term = 0 then (LlvmGraph.add_edge_e graph (i, block, (-1, 0))) else 
          (fold_successors (fun succ graph -> 
            LlvmGraph.add_edge_e graph (i, block, (block_id succ, 0))
          ) term graph)
    ) g all_blocks in
    g |> duplicate_phi_nodes, (0, 0), params

let generate_llvm_graph c_file = generate_llvm_graph_from_ir (generate_llvm_ir c_file)

let print_llvm_block (block : llbasicblock) : unit =
  print_string "\n Block begin:";
  iter_instrs (fun instr -> print_string "\n Instr:"; print_string (string_of_llvalue instr)) block;
  print_string "\n Block end\n"

