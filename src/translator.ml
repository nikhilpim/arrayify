

let translate fn precondition =
  let llvm_graph, entry = Llvmutil.generate_llvm_graph fn in
  let boogie_graph = Executor.execute entry llvm_graph precondition in 
  let boogie_code = Boogieir.code_of_boogie_graph boogie_graph in
  let oc = open_out "output.bpl" in
  output_string oc boogie_code;
  close_out oc