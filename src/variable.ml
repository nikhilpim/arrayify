
type var = string
type pvar = string
type bvar = string 

type boogie_var = string
type boogie_avar = string

let boogie_var_of_var (v: var) : boogie_var = v
let boogie_avar_of_pvar (p: pvar) : boogie_avar = p
let boogie_var_of_pvar (p: pvar) : boogie_var = p