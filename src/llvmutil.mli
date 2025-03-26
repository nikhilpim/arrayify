open Llvm
type llvm_node = { label : int ; instr : llbasicblock}

type llvm_graph = 
    { entry : llvm_node
    ; nodes : llvm_node list
    ; edges : (llvm_node * llvm_node) list
    ; 
    }

val generate_llvm_ir : string -> llmodule
val print_llvm_block : llbasicblock -> unit
