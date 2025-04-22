open Srkmin

type var = string
type pvar = string
type bvar = int 

type boogie_var = string
type boogie_avar = string

let boogie_var_of_var (v: var) : boogie_var = v
let boogie_avar_of_pvar (p: pvar) : boogie_avar = p
let boogie_var_of_pvar (p: pvar) : boogie_var = p

let var_to_svar = (Memo.memo (fun v -> Syntax.mk_const Global.srk (Syntax.mk_symbol Global.srk ~name:v `TyInt)))
let pvarblock_to_svar = (Memo.memo (fun p -> Syntax.mk_const Global.srk (Syntax.mk_symbol Global.srk ~name:("block("^p^")") `TyInt)))
let pvaroffset_to_svar = (Memo.memo (fun p -> Syntax.mk_const Global.srk (Syntax.mk_symbol Global.srk ~name:("offset("^p^")") `TyInt)))
let bvar_to_svar (b : bvar) : 'a Syntax.arith_term = Syntax.mk_int Global.srk b

let new_var ?(v : string = "") () : var = v
let new_pvar ?(v : string = "") () : pvar = v
let new_bvar ?(v : int =0) (): bvar = v