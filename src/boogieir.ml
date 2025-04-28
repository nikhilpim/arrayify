open Symbolicheap
open Variable

module BoAvarSet = Set.Make(struct 
  type t = boogie_avar
  let compare a b = if a < b then -1 else if a > b then 1 else 0
end)

module BoVarSet = Set.Make(struct 
  type t = boogie_var
  let compare a b = if a < b then -1 else if a > b then 1 else 0
end)

type boogie_term = 
  Int of int
  | Var of boogie_var
  | Times of int * boogie_term
  | Sum of boogie_term * boogie_term
  | Read of boogie_avar * boogie_term


type boogie_formula = 
    Leq of boogie_term * boogie_term
  | Eq of boogie_term * boogie_term
  | AEq of boogie_avar * boogie_avar
  | And of boogie_formula * boogie_formula
  | Or of boogie_formula * boogie_formula
  | Not of boogie_formula
  | True

type boogie_instr = Assign of boogie_var * boogie_term | AAssign of boogie_avar * boogie_avar | AWrite of boogie_avar * boogie_term * boogie_term | Assume of boogie_formula | Assert of boogie_formula | Error

module BGNode = struct 
  type t = int * Llvmutil.LlvmNode.t * symbolicheap
  let hash = Hashtbl.hash 

  let compare (a, b, _) (c, d, _) = if b < d then -1 else if b > d then 1 else if a < c then -1 else if a > c then 1 else 0

  let equal (a, b, _) (c, d, _) = (b = d) && (a = c)
end

module BGEdge = struct
  type t = boogie_instr list 
  let hash = Hashtbl.hash
  let compare a b = if hash a < hash b then -1 else if hash a > hash b then 1 else 0 
  let equal a b = (hash a = hash b)
  let default = []
end

module BGraph = Graph.Persistent.Digraph.ConcreteLabeled(BGNode)(BGEdge)


let rec boogie_term_of_pointer_term (p : pointer_term) = 
  match p with 
  | Pointer p -> Var (boogie_var_of_pvar p)
  | PointerSum (p, t) -> Sum (boogie_term_of_pointer_term p, boogie_term_of_int_term t)
and boogie_term_of_int_term (t: int_term) = 
  match t with 
  | Int i -> Int i
  | Var v -> Var (boogie_var_of_var v)
  | Times (i, t) -> Times (i, boogie_term_of_int_term t)
  | Sum (t1, t2) -> Sum (boogie_term_of_int_term t1, boogie_term_of_int_term t2)
  | Offset p -> boogie_term_of_pointer_term p
  | PSub (p1, p2) -> Sum (boogie_term_of_pointer_term p1, (Times (-1, boogie_term_of_pointer_term p2)))
  

let get_avars (g : BGraph.t) : boogie_avar list = 
  let rec fold_bt t acc = 
    match t with 
    | Int _ -> acc
    | Var _ -> acc
    | Times (_, t) -> fold_bt t acc
    | Sum (t1, t2) -> fold_bt t1 (fold_bt t2 acc)
    | Read (at, t) -> BoAvarSet.add at (fold_bt t acc)
  in 
  BGraph.fold_edges_e (fun (_, ops, _) acc -> List.fold_left (fun acc op -> 
    match op with 
    | AAssign (a1, a2) -> BoAvarSet.add a1 (BoAvarSet.add a2 acc)
    | AWrite (a, t1, t2) -> BoAvarSet.add a (fold_bt t1 (fold_bt t2 acc))
    | Assign (_, t) -> fold_bt t acc
    | Assume _ -> acc
    | Assert _ -> acc
    | Error -> acc
    ) acc ops) g BoAvarSet.empty
  |> BoAvarSet.elements

let get_vars (g : BGraph.t) : boogie_var list = 
  let rec fold_bt t acc = 
    match t with 
    | Int _ -> acc
    | Var v -> BoVarSet.add v acc
    | Times (_, t) -> fold_bt t acc
    | Sum (t1, t2) -> fold_bt t1 (fold_bt t2 acc)
    | Read (_, t) -> fold_bt t acc
  in 
  BGraph.fold_edges_e (fun (_, ops, _) acc -> List.fold_left (fun acc op -> 
    match op with 
    | Assign (v, t) -> fold_bt t (BoVarSet.add v acc)
    | AWrite (_, t1, t2) -> fold_bt t1 (fold_bt t2 acc)
    | AAssign _ -> acc
    | Assume _ -> acc
    | Assert _ -> acc
    | Error -> acc
  ) acc ops) g BoVarSet.empty
  |> BoVarSet.elements

let rec bt_text (t : boogie_term) : string = 
  match t with 
  | Int i -> string_of_int i
  | Var v -> boogie_var_name v
  | Times (i, t) -> string_of_int i^" * "^(bt_text t)
  | Sum (t1, t2) -> "("^(bt_text t1)^" + "^(bt_text t2)^")"
  | Read (a, t) ->  (boogie_avar_name a)^"["^(bt_text t)^"]"

  let rec bf_text (f : boogie_formula) : string =
  match f with
  | True -> "true"
  | Leq (t1, t2) -> "("^(bt_text t1)^" <= "^(bt_text t2)^")"
  | Eq (t1, t2) -> "("^(bt_text t1)^" == "^(bt_text t2)^")"
  | AEq (a1, a2) -> "("^(boogie_avar_name a1)^" == "^(boogie_avar_name a2)^")"
  | And (f1, f2) -> "("^(bf_text f1)^" && "^(bf_text f2)^")"
  | Or (f1, f2) -> "("^(bf_text f1)^" || "^(bf_text f2)^")"
  | Not f1 -> "!("^(bf_text f1)^")"

let boogie_instr_text (boogie_instr : boogie_instr) : string = 
  match boogie_instr with 
  | Assign (v, t) -> boogie_var_name v^" := "^(bt_text t)^";\n"
  | AAssign (a1, a2) -> (boogie_avar_local_name a1)^" := "^(boogie_avar_name a1)^";\n"^(boogie_avar_name a1) ^" := "^(boogie_avar_name a2)^";\n"
  | AWrite (a, t1, t2) -> (boogie_avar_name a)^" := "^(boogie_avar_name a)^";\n"^(boogie_avar_name a)^"["^(bt_text t1)^"] := "^(bt_text t2)^";\n"
  | Assume f -> "assume "^bf_text f^";\n"
  | Assert f -> "assert "^bf_text f^";\n"
  | Error -> "error;\n"

let code_of_boogie_graph (entry : BGNode.t) (g : BGraph.t) : string = 
  let avars = get_avars g in
  let array_variables = List.fold_left (fun acc b -> ((boogie_avar_name b), boogie_var_name (boogie_length_of_boogie_avar b)) :: acc) [] avars in
  let array_initialization = List.fold_left (fun acc (arr, len) -> acc ^"var "^arr^" : [int]int;\n var "^len^" : int;\n") "" array_variables in 
  let modifies_statement = List.fold_left (fun acc (arr, len) -> "modifies "^arr^","^len^";\n" ^ acc) "" array_variables in 

  let vars = get_vars g in 
  let var_declarations = List.fold_left (fun acc v -> "var "^(boogie_var_name v)^" : int;\n" ^ acc) "" vars 
      ^ (List.fold_left (fun acc a -> "var "^(boogie_avar_local_name a)^" : [int]int;\n"^ acc ) "" avars) in
  
  let node_name = (fun (i, j, _) -> "codelabel"^(string_of_int i)^"_"^(Llvmutil.LlvmNode.name j)) in

  let rec go node seen = 
    if List.mem (node_name node) seen then "goto "^(node_name node)^";\n" else 
      let seen = (node_name node) :: seen in 
      let succs = BGraph.succ_e g node in 
      let succ_texts = List.map (fun (_, instrs, succ_node) -> 
        let instr_texts = String.concat "" (List.map boogie_instr_text instrs) in 
        let succ_text = go succ_node seen in
      instr_texts ^ succ_text
      ) succs in 

      let rec if_creator succ_texts = 
        match succ_texts with 
        | [] -> ""
        | [x] -> x
        | x :: xs -> "if (*) {\n" ^ x ^ "} else \n" ^ (if_creator xs)
      in
      (node_name node)^":\n" ^ (if_creator succ_texts) ^ "\n"
    in
  let procedure_body = var_declarations^(go entry []) in 
  let procedure = "procedure main() \n" ^ modifies_statement ^ "{\n" ^ procedure_body ^ "}\n" in

  array_initialization ^ procedure