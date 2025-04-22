

type var
type pvar
type bvar

type boogie_var
type boogie_avar

val boogie_var_of_var : var -> boogie_var
val boogie_avar_of_pvar : pvar -> boogie_avar
val boogie_var_of_pvar : pvar -> boogie_var

val var_to_svar : var -> Global.Ctx.t Srkmin.Syntax.arith_term
val pvarblock_to_svar : pvar -> Global.Ctx.t Srkmin.Syntax.arith_term
val pvaroffset_to_svar : pvar -> Global.Ctx.t Srkmin.Syntax.arith_term
val bvar_to_svar : bvar -> Global.Ctx.t Srkmin.Syntax.arith_term

val new_var : ?v:string -> unit -> var
val new_pvar : ?v:string -> unit -> pvar
val new_bvar : ?v:int -> unit -> bvar