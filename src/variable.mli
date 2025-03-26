

type var
type pvar
type bvar

type boogie_var
type boogie_avar

val boogie_var_of_var : var -> boogie_var
val boogie_avar_of_pvar : pvar -> boogie_avar
val boogie_var_of_pvar : pvar -> boogie_var