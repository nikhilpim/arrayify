open Arrayify
open Boogieir
open Boogie_driver  (* your parser entry point *)

let () =
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Usage: %s <file.bpl>\n" Sys.argv.(0);
    exit 1
  );

  let filename = Sys.argv.(1) in
  let bgraph = parse filename in   (* parse returns boogie_instr list *)

  BGraph.iter_vertex (fun v ->
    let (id, _, _, _) = v in 
    Printf.printf "Node: %s\n" (string_of_int id);  (* or your own node printer *)
    BGraph.iter_succ (fun succ ->
      let (sid, _, _, _) = succ in
      Printf.printf "  -> %s\n" (string_of_int sid)
    ) bgraph v
  ) bgraph
