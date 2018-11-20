#!/usr/bin/env julia

using RCall
using SpecialFunctions

@rlibrary FAdist

Alpha = 5
Beta = -0.5
Gamma = 1.82

@rput Alpha Beta Gamma

R"p <- FAdist::pweibull3(6, scale=Alpha, thres=Beta, shape=Gamma)"

@rget p
println(p)
