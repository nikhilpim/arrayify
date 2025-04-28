

let translate fn precondition =
  let llvm_graph, entry = Llvmutil.generate_llvm_graph fn in
  let boogie_graph = Executor.execute entry llvm_graph precondition in 
  let boogie_code = Boogieir.code_of_boogie_graph (0, entry, precondition) boogie_graph in
  let oc = open_out "/Users/np6641/dev/arrayify/output.bpl" in
  output_string oc boogie_code; print_string boogie_code;
  close_out oc