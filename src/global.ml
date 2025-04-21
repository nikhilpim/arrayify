module Ctx = Srkmin.Syntax.MakeContext ()
let ctx = Ctx.context
let solver = Srkmin.SrkZ3.Solver.make ctx