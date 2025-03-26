open Llvm
open Translator

let test_parser fn = 
  print_string ("\n\n Beginning test of file " ^ fn ^ "\n\n");
  let x = Llvmutil.generate_llvm_ir fn in
  fold_left_functions (fun _ f -> 
    iter_params (fun p -> 
      print_string "Param: ";
      print_endline (string_of_llvalue p)
    ) f;
    fold_left_blocks (fun _ b -> 
      Llvmutil.print_llvm_block b;
      match block_terminator b with
      | None -> print_string "No terminator"
      | Some terminator -> 
        iter_successors (fun s -> 
          print_string "Successor: ";
          Llvmutil.print_llvm_block s
        ) terminator
    ) () f
    ) () x



let () = (
  test_parser"/Users/np6641/dev/translator/test/files/min.c";
  test_parser"/Users/np6641/dev/translator/test/files/basic.c";
  )