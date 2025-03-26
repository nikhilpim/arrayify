open Llvm
open Llvm_irreader

type llvm_node = { label : int ; instr : llbasicblock}

type llvm_graph = 
    { entry : llvm_node
    ; nodes : llvm_node list
    ; edges : (llvm_node * llvm_node) list
    ; 
    }

let generate_llvm_ir c_file =
  let ctx = (create_context ()) in 
  let llvm_ir_file = Filename.temp_file "output" ".ll" in
  let clang_command = Printf.sprintf "clang -S -emit-llvm %s -o %s" c_file llvm_ir_file in
  let _ = Sys.command clang_command in
  let mem_buffer = MemoryBuffer.of_file llvm_ir_file in
  parse_ir ctx mem_buffer



let print_llvm_block (block : llbasicblock) : unit =
  print_string "\n Block begin:";
  iter_instrs (fun instr -> print_string "\n Instr:"; print_string (string_of_llvalue instr)) block;
  print_string "\n Block end\n"

