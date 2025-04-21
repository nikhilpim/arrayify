open Symbolicheap
open Llvmutil
open Boogieir

val execute : LlvmNode.t -> Graph.Persistent.Digraph.ConcreteLabeled(LlvmNode)(LlvmEdge).t -> symbolicheap -> Graph.Persistent.Digraph.ConcreteLabeled(BGNode)(BGEdge).t