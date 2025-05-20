module Ctx = Srkmin.Syntax.MakeContext ()
let srk = Ctx.context
let solver = Srkmin.SrkZ3.Solver.make srk
let max_malloc = ref (-1)