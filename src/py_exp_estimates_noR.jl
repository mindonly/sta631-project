#!/usr/bin/env julia

# Reference:
# Miller, S. J. (2005). A Derivation of the Pythagorean Won-Loss Formula in Baseball.
#   ArXiv:Math/0509698. Retrieved from http://arxiv.org/abs/math/0509698


# Julia modules
using CSV
using DataFrames
using Distributions
using LinearAlgebra     # dot()
using SpecialFunctions  # gamma()


# globals (don't change these)
const n_games = 162
const β = -0.5             # threshold, location, shift (β) [Beta]

# knobs to twiddle
const init_γ = 1.50
const stop_γ = 2.00
const p_digits = 3         # precision digits
γ_step = 0.001


# working directory (datapath)
function setdatapath(year)
   data_dir = "/Users/sanch/Dropbox/f2018/sta631/Project/data/"
   cd(data_dir * string(year))

   return string(year)
end

# α_RS (runs scored), α_RA (runs allowed) [Alpha]
alpharuns(runs, γ) = (runs - β) / gamma(1 + 1/γ)

# least squares sum
function ls_sum(runs, game_d, γ)
   α = alpharuns(runs, γ)
   dist = Weibull(γ, α)
   run_v = collect(keys(game_d))

   _sum = 0
   for rc in run_v
      bin_area = cdf(dist, (rc - β) - β) - cdf(dist, (rc - β) + β)
      _sum += (game_d[rc] - n_games * bin_area)^2
   end

   return _sum
end

# least squares estimate
function ls_est(RS, RA, score_dict, allow_dict)
   gammav = Vector{Float64}()    # gamma vector
   ssv = Vector{Float64}()       # sum-of-squares vector

   # shape (γ) [Gamma]
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

   return gammav[findmin(ssv)[2]]            # minimize sum-of-squares
end

# max. likelihood (log) product
function mle_prod(runs, game_d, γ)
   α = alpharuns(runs, γ)
   dist = Weibull(γ, α)
   run_v = collect(keys(game_d))

   _prod = 0
   for rc in run_v
      bin_area = cdf(dist, (rc - β) - β) - cdf(dist, (rc - β) + β)
      _prod += log(bin_area^game_d[rc])
   end

   return _prod                              # log(ab) = log(a) + log(b)
end

# max. likelihood estimate
function mle_est(RS, RA, score_dict, allow_dict)
   gammav = Vector{Float64}()    # gamma vector
   ssv = Vector{Float64}()       # sum-of-squares vector

   # shape (γ) [Gamma]
   γ = init_γ

   total_prod = 0
   while γ <= stop_γ
      push!(gammav, round(γ, digits=p_digits))
      prod_RS = mle_prod(RS, score_dict, γ)
      prod_RA = mle_prod(RA, allow_dict, γ)
      total_prod = prod_RS + prod_RA         # log(p_RS * p_RA) = log(p_RS) + log(p_RA)
      push!(ssv, total_prod)
      γ += γ_step
   end

   return gammav[findmax(ssv)[2]]            # max(LL) = min(-(LL))
                                             # where LL <- log-likelihood
end


# main function
function main()
   season = setdatapath(2004)

   # AL / NL
   teams = ["BOS", "NYY", "BAL", "TBD", "TOR",           # east
            "MIN", "CHW", "CLE", "DET", "KCR",           # central
            "ANA", "OAK", "TEX", "SEA",                  # west
            "ATL", "PHI", "FLA", "NYM", "WSN",           # east: WSN is Montreal
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

   println("   ==============================================")
   println("   * $season MLB Pythagorean expectation estimates *")
   println("   ==============================================\n")

   return_df = DataFrame([teams, ls_v, mle_v], [:Team, :LS, :MLE])
   println(return_df, "\n")

   outfile = season * "_py_exp_estimates.csv"
   CSV.write(outfile, return_df)
end


@time main()
println()
