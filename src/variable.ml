open Srkmin

type var = string
type pvar = string
type bvar = int 

type boogie_var = string
type boogie_avar = int

let boogie_var_of_var (v: var) : boogie_var = v
let boogie_var_of_pvar (p: pvar) : boogie_var = p
let boogie_avar_of_bvar (b : bvar) : boogie_avar = b
let boogie_length_of_boogie_avar (b : boogie_avar) : boogie_var = "length"^string_of_int b

let pvar_name (p : pvar) : string = p
let boogie_var_name (b : boogie_var) : string = String.map (fun c -> if c = '%' then 'q' else c) b
let boogie_avar_name (b : boogie_avar) : string = "a"^string_of_int b
let boogie_avar_local_name (b : boogie_avar) : string = "a"^string_of_int b^"_local"


let var_to_svar = (Memo.memo (fun v -> Syntax.mk_const Global.srk (Syntax.mk_symbol Global.srk ~name:(boogie_var_name v) `TyInt)))
let pvarblock_to_svar = (Memo.memo (fun p -> Syntax.mk_const Global.srk (Syntax.mk_symbol Global.srk ~name:("block("^(boogie_var_name p)^")") `TyInt)))
let pvaroffset_to_svar = (Memo.memo (fun p -> Syntax.mk_const Global.srk (Syntax.mk_symbol Global.srk ~name:("offset("^(boogie_var_name p)^")") `TyInt)))
let bvar_to_svar (b : bvar) : 'a Syntax.arith_term = Syntax.mk_int Global.srk b

let new_var ?(v : string = "") () : var = v
let new_pvar ?(v : string = "") () : pvar = v
let new_bvar ?(v : int =0) (): bvar = v