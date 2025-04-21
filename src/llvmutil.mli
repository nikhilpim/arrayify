open Llvm

module LlvmNode :
  sig
    type t 
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int
  end

module LlvmEdge :
  sig
    type t = llbasicblock
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val default : t
  end

val llvm_block_to_string : Llvm.llbasicblock -> string
val block_eq : Llvm.llbasicblock -> Llvm.llbasicblock -> bool
val generate_llvm_ir : string -> Llvm.llmodule
val generate_llvm_graph_from_ir : Llvm.llmodule -> Graph.Persistent.Digraph.ConcreteLabeled(LlvmNode)(LlvmEdge).t * LlvmNode.t
val generate_llvm_graph : string -> Graph.Persistent.Digraph.ConcreteLabeled(LlvmNode)(LlvmEdge).t * LlvmNode.t
val print_llvm_block : Llvm.llbasicblock -> unit
