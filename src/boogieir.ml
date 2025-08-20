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

type boogie_instr = 
| Assign of boogie_var * boogie_term 
| AAssign of boogie_avar * boogie_avar 
| AWrite of boogie_avar * boogie_term * boogie_term 
| Assume of boogie_formula 
| Assert of boogie_formula 
| IteAssign of boogie_var * boogie_formula * boogie_term * boogie_term 
| Rotate of (boogie_avar * boogie_avar) list
| Return of boogie_term
| Error

module BGNode = struct 
  type t = int * Llvmutil.LlvmNode.t * symbolicheap * boogie_instr list
  let hash = Hashtbl.hash 

  let compare (a, b, _, _) (c, d, _, _) = if b < d then -1 else if b > d then 1 else if a < c then -1 else if a > c then 1 else 0

  let equal (a, b, _, _) (c, d, _, _) = (b = d) && (a = c)
end

module BGEdge = struct
  type t = boogie_formula option * boogie_instr list option
  let hash = Hashtbl.hash
  let compare a b = if hash a < hash b then -1 else if hash a > hash b then 1 else 0 
  let equal a b = (hash a = hash b)
  let default = None, None
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
  BGraph.fold_vertex (fun (_, _, _, ops) acc -> List.fold_left (fun acc op -> 
    match op with 
    | AAssign (a1, a2) -> BoAvarSet.add a1 (BoAvarSet.add a2 acc)
    | AWrite (a, t1, t2) -> BoAvarSet.add a (fold_bt t1 (fold_bt t2 acc))
    | Assign (_, t) -> fold_bt t acc
    | Assume _ -> acc
    | Assert _ -> acc
    | IteAssign (_, _, t1, t2) -> fold_bt t1 (fold_bt t2 acc)
    | Rotate ls -> List.fold_left (fun acc (from, towards) -> BoAvarSet.add from (BoAvarSet.add towards acc)) acc ls
    | Return _ -> acc
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
  BGraph.fold_vertex (fun (_, _, _, ops) acc -> List.fold_left (fun acc op -> 
    match op with 
    | Assign (v, t) -> fold_bt t (BoVarSet.add v acc)
    | AWrite (_, t1, t2) -> fold_bt t1 (fold_bt t2 acc)
    | AAssign _ -> acc
    | Assume _ -> acc
    | Assert _ -> acc
    | IteAssign (v, _, t1, t2) -> fold_bt t1 (fold_bt t2 (BoVarSet.add v acc))
    | Rotate _ -> acc
    | Return t -> fold_bt t acc
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

let local_array_uses (g : BGraph.t) : boogie_avar list = 
  (BGraph.fold_vertex (fun (_, _, _, ops) acc -> 
    List.fold_left (fun acc instr -> 
      match instr with 
      | Rotate ls -> List.fold_left (fun acc (from, _) -> BoAvarSet.add from acc) acc ls
      | _ -> acc
      ) acc ops) g BoAvarSet.empty
  |> BoAvarSet.elements) @ 
  (BGraph.fold_edges_e (fun (_, (_, rotation), _) acc -> 
    match rotation with 
    | None -> acc
    | Some instrs -> 
      List.fold_left (fun acc instr -> 
        match instr with 
        | Rotate ls -> List.fold_left (fun acc (from, _) -> BoAvarSet.add from acc) acc ls
        | _ -> acc
      ) acc instrs
    ) g BoAvarSet.empty |> BoAvarSet.elements)

let boogie_instr_text (boogie_instr : boogie_instr) : string = 
  match boogie_instr with 
  | Assign (v, t) -> boogie_var_name v^" := "^(bt_text t)^";\n"
  | AAssign (a1, a2) -> (boogie_avar_name a1) ^" := "^(boogie_avar_name a2)^";\n"
  | AWrite (a, t1, t2) -> (boogie_avar_name a)^"["^(bt_text t1)^"] := "^(bt_text t2)^";\n"
  | Assume f -> "assume "^bf_text f^";\n"
  | Assert f -> "assert "^bf_text f^";\n"
  | IteAssign (v, f, t1, t2) -> 
    let v_text = boogie_var_name v in 
    let t1_text = bt_text t1 in 
    let t2_text = bt_text t2 in 
    let f_text = bf_text f in 
    "if ("^f_text^") {\n"^v_text^" := "^t1_text^";\n} else {\n"^v_text^" := "^t2_text^";\n}\n"
  | Rotate ls -> (
      let local_store = (List.map (fun (from, _) -> (boogie_avar_local_name from)^" := "^(boogie_avar_name from)^";\n") ls |> String.concat "") in 
      let rotate = (List.map (fun (from, towards) -> (boogie_avar_name towards)^" := "^(boogie_avar_local_name from)^";\n") ls |> String.concat "") in 
      local_store ^ rotate
  )
  | Return v -> "retval := "^bt_text v^";\n"
  | Error -> "error;\n"


let code_of_boogie_graph (entry : BGNode.t) (g : BGraph.t) (params : boogie_var list) (array_params : boogie_avar list): string = 
  let returns_statement = "returns ("^(List.fold_left (fun acc a -> (boogie_avar_name a^" : [int]int") :: acc) ["retval : int"] array_params |> String.concat ", ")^")\n" in 
  let params = params @ (List.map boogie_length_of_boogie_avar array_params) in

  let avars = get_avars g |>  List.filter (fun e -> not ( List.mem e array_params))in
  let vars = get_vars g |> List.filter (fun e -> not ( List.mem e params)) in 
  let local_array_uses = local_array_uses g in
  let var_declarations = List.fold_left (fun acc v -> "var "^(boogie_var_name v)^" : int;\n" ^ acc) "" vars 
      ^ (List.fold_left (fun acc a -> 
        "var "^(boogie_avar_name a)^" : [int]int;\n"^
        acc ) "" avars) 
      ^ (List.map (fun a -> "var "^(boogie_avar_local_name a)^" : [int]int;\n") local_array_uses |> String.concat "") in
  
  let (node_name : BGNode.t -> string) = (fun (i, j, _, _) -> "codelabel"^(string_of_int i)^"_"^(Llvmutil.LlvmNode.name j)) in

  let rec go (node : BGNode.t) seen = 
    if List.mem (node_name node) seen then "goto "^(node_name node)^";\n" else 
      let seen = (node_name node) :: seen in 
      let succs = BGraph.succ_e g node in 
      let succ_texts = List.map (fun (_, (cond, rotation), tgt) -> 
        let rotation_text = 
          match rotation with 
          | Some instrs -> String.concat "" (List.map boogie_instr_text instrs)
          | None -> ""
        in 
        match cond with 
        | None -> rotation_text ^ go tgt seen 
        | Some f -> 
          let succ_text = go tgt seen in 
          "if ("^(bf_text f)^") {\n" ^ rotation_text ^ succ_text ^ "}"
      ) succs in 
      let (_, _, _, instrs) = node in
      let node_text = String.concat "" (List.map boogie_instr_text instrs) in 
      (node_name node)^":\n" ^ node_text ^ (String.concat " else " succ_texts) ^ "\n" 
    in
  let array_initialization = (List.fold_left (fun acc a -> 
    boogie_avar_name a^" := "^(boogie_avar_input_name a)^";\n"^acc) "" array_params) in
  let procedure_body = var_declarations^array_initialization^(go entry []) in 
  let parameter_string = String.concat ", " ((List.map (fun p -> (boogie_var_name p) ^ " : int") params) @ 
  (List.fold_left (fun acc a -> ((boogie_avar_input_name a) ^ " : [int]int") :: acc ) [] array_params)) in

  let procedure = "procedure main("^parameter_string^") \n" ^ returns_statement ^ "{\n" ^ procedure_body ^ "}\n" in

  procedure

let generate_new_bvar (graph : BGraph.t) : Variable.bvar = 
  generate_new_bvar (get_avars graph)