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
    type t = int 
    let compare = Int.compare
    let equal = Int.equal
    let hash a = Hashtbl.hash a
    let name t = if t = (-1) then "negone" else string_of_int t
end

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
    |> List.mapi (fun i e -> (i, e)) in
  let g = List.fold_left (fun graph (i, _) -> LlvmGraph.add_vertex graph i) LlvmGraph.empty all_blocks in
  (* -1 is the designated end vertex. *)
  let g = LlvmGraph.add_vertex g (-1) in
  let g = List.fold_left (fun graph (i, block) -> 
      let terminator = block_terminator block in
      match terminator with
      | None -> graph
      | Some term -> 
        (* If we're looking at a return, we want to add an edge to a designated end location (-1). I assume that returns are all terminators with no successors. *)
        if num_successors term = 0 then (LlvmGraph.add_edge_e graph (i, block, -1)) else 
          (fold_successors (fun succ graph -> 
            let (i', _) = List.find (fun (_, block') -> block_eq block' succ) all_blocks in
            LlvmGraph.add_edge_e graph (i, block, i')
          ) term graph)
    ) g all_blocks in
    g, (List.length all_blocks - 1), params

let generate_llvm_graph c_file = generate_llvm_graph_from_ir (generate_llvm_ir c_file)

let print_llvm_block (block : llbasicblock) : unit =
  print_string "\n Block begin:";
  iter_instrs (fun instr -> print_string "\n Instr:"; print_string (string_of_llvalue instr)) block;
  print_string "\n Block end\n"

