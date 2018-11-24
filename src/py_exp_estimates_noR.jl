#!/usr/bin/env julia

# Reference:
# Miller, S. J. (2005). A Derivation of the Pythagorean Won-Loss Formula in Baseball.
#   ArXiv:Math/0509698. Retrieved from http://arxiv.org/abs/math/0509698


# Julia modules
using CSV
using Printf            # @printf
using DataFrames
using Distributions
using LinearAlgebra     # dot()
using SpecialFunctions  # gamma()


season = 2004
data_dir = "/Users/sanch/Dropbox/f2018/sta631/Project/data/"
cd(data_dir * string(season))

# globals (don't change these)
n_games = 162
β = -0.5          # threshold, location, shift Beta (β)

# knobs to twiddle
p_digits = 3         # precision digits
γ_step = 0.001
init_γ = 1.50
stop_γ = 2.00


# α_RS (runs scored), α_RA (runs allowed) [Alpha]
function alpharuns(runs, γ)
   return (runs - β) / gamma(1 + 1/γ)
end

# least squares sum
function ls_sum(runs, game_d, γ)
   sum_R = 0
   run_v = collect(keys(game_d))

   α = alpharuns(runs, γ)
   dist = Weibull(γ, α)

   for rc in run_v
      area_R = cdf(dist, (rc - β) - β) - cdf(dist, (rc - β) + β)
      sum_R += (game_d[rc] - n_games * area_R)^2
   end

   return sum_R
end

# least squares estimate
function ls_est(RS, RA, score_dict, allow_dict)
   gammav = Vector{Float64}()
   ssv = Vector{Float64}()

   # shape (γ)
   γ = init_γ
   total_sum = 0

   while γ <= stop_γ
      push!(gammav, round(γ, digits=p_digits))
      sum_RS = ls_sum(RS, score_dict, γ)
      sum_RA = ls_sum(RA, allow_dict, γ)
      total_sum = sum_RS + sum_RA
      push!(ssv, total_sum)
      γ += γ_step
   end
   ls_est = gammav[findmin(ssv)[2]]

   return ls_est
end

# max. likelihood product
function mle_prod(runs, game_d, γ)
   prod_R = 0
   run_v = collect(keys(game_d))

   α = alpharuns(runs, γ)
   dist = Weibull(γ, α)

   for rc in run_v
      area_R = cdf(dist, (rc - β) - β) - cdf(dist, (rc - β) + β)
      prod_R += log(area_R^game_d[rc])
   end

   return prod_R
end

# max. likelihood estimate
function mle_est(RS, RA, score_dict, allow_dict)
   gammav = Vector{Float64}()
   ssv = Vector{Float64}()

   # shape Gamma (γ)
   γ = init_γ
   total_prod = 0

   while γ <= stop_γ
      push!(gammav, round(γ, digits=p_digits))
      prod_RS = mle_prod(RS, score_dict, γ)
      prod_RA = mle_prod(RA, allow_dict, γ)
      total_prod = log(prod_RS * prod_RA)
      push!(ssv, total_prod)
      γ += γ_step
   end
   mle_est = gammav[findmin(ssv)[2]]

   return mle_est
end


# main function
function main()
   # AL / NL
   teams = ["BOS", "NYY", "BAL", "TBD", "TOR",           # east
            "MIN", "CHW", "CLE", "DET", "KCR",           # central
            "ANA", "OAK", "TEX", "SEA",                  # west
            "ATL", "PHI", "FLA", "NYM", "WSN",           # east: WSN is MTL
            "STL", "HOU", "CHC", "CIN", "PIT", "MIL",    # central
            "LAD", "SFG", "SDP", "COL", "ARI"]           # west

   # estimate vectors
   ls_v = Vector{Float64}()
   mle_v = Vector{Float64}()

   for team in teams
      rs_dataset = team * "_rs.csv"
      ra_dataset = team * "_ra.csv"

      score_summary_df = CSV.read(rs_dataset)
      rsv = score_summary_df[:Runs]
      rsg = score_summary_df[:Games]

      allow_summary_df = CSV.read(ra_dataset)
      rav = allow_summary_df[:Runs]
      rag = allow_summary_df[:Games]

      score_dict = Dict(rsv[i] => rsg[i] for i = 1:length(rsv))
      allow_dict = Dict(rav[i] => rag[i] for i = 1:length(rav))

      RS = dot(rsv, rsg) / n_games
      RA = dot(rav, rag) / n_games

      push!(ls_v, ls_est(RS, RA, score_dict, allow_dict))
      push!(mle_v, mle_est(RS, RA, score_dict, allow_dict))
   end

   return_df = DataFrame([teams, ls_v, mle_v], [:Team, :LS, :MLE])
   println(return_df, "\n")
   CSV.write("2004_py_exp_estimates.csv", return_df)
end


println("--------------------------------------------")
println(" 2004 MLB Pythagorean expectation estimates ")
println("--------------------------------------------")
@time main()
println()
