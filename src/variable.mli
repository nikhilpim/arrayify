

type var
type pvar
type bvar
type all_var = Variable of var | Pointer of pvar | Block of bvar

type boogie_var
type boogie_avar

val boogie_var_of_var : var -> boogie_var
val boogie_var_of_pvar : pvar -> boogie_var
val boogie_avar_of_bvar : bvar -> boogie_avar
val boogie_length_of_boogie_avar : boogie_avar -> boogie_var 

val pvar_name : pvar -> string
val boogie_var_name : boogie_var -> string
val boogie_avar_name : boogie_avar -> string
val boogie_avar_local_name : boogie_avar -> string

val var_to_svar : var -> Global.Ctx.t Srkmin.Syntax.arith_term
val pvarblock_to_svar : pvar -> Global.Ctx.t Srkmin.Syntax.arith_term
val pvaroffset_to_svar : pvar -> Global.Ctx.t Srkmin.Syntax.arith_term
val bvar_to_svar : bvar -> Global.Ctx.t Srkmin.Syntax.arith_term

val new_var : ?v:string -> unit -> var
val new_pvar : ?v:string -> unit -> pvar
val new_bvar : ?v:int -> unit -> bvar

val prime_var : var -> var
val prime_pvar : pvar -> pvar