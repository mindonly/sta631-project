#!/usr/bin/env julia

# Reference:
# Miller, S. J. (2005). A Derivation of the Pythagorean Won-Loss Formula in Baseball.
#   ArXiv:Math/0509698. Retrieved from http://arxiv.org/abs/math/0509698


# Julia modules
using CSV
using RCall
using Printf            # @printf
using DataFrames
using LinearAlgebra     # dot()
using SpecialFunctions  # gamma()
# R libraries
@rlibrary FAdist


season = 2004
data_dir = "/Users/sanch/Dropbox/f2018/sta631/Project/data/"
cd(data_dir * string(season))

# AL / NL
teams = ["\nAL\n",
         "BOS", "NYY", "BAL", "TBD", "TOR", "",          # east
         "MIN", "CHW", "CLE", "DET", "KCR", "",          # central
         "ANA", "OAK", "TEX", "SEA",                     # west
         "\nNL\n",
         "ATL", "PHI", "FLA", "NYM", "WSN", "",          # east: WSN is MTL
         "STL", "HOU", "CHC", "CIN", "PIT", "MIL", "",   # central
         "LAD", "SFG", "SDP", "COL", "ARI"]              # west

n_games = 162

for team in teams
   if team == "\nAL\n" || team == "\nNL\n" || team == ""
      println("$team--")
      continue
   end

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

   # threshold, location, shift (Beta)
   Beta = -0.5

   gammav = Vector()
   ssv = Vector()

   # shape (Gamma)
   Gamma = 1.60

   total_sum = 0
   while Gamma <= 2.05
      push!(gammav, round(Gamma, digits=2))

      sum_RS = 0
      sum_RA = 0

      for rc in rsv
         # scale (Alpha)
         Alpha_RS = (RS - Beta) / gamma(1 + 1/Gamma)
         @rput rc Alpha_RS Beta Gamma
         R"area_RS <- FAdist::pweibull3(rc - Beta, scale=Alpha_RS, thres=Beta, shape=Gamma) -
                      FAdist::pweibull3(rc + Beta, scale=Alpha_RS, thres=Beta, shape=Gamma)"
         @rget area_RS
         sum_RS += (score_dict[rc] - n_games * area_RS)^2
      end
      for rc in rav
         # scale (Alpha)
         Alpha_RA = (RA - Beta) / gamma(1 + 1/Gamma)
         @rput rc Alpha_RA Beta Gamma
         R"area_RA <- FAdist::pweibull3(rc - Beta, scale=Alpha_RA, thres=Beta, shape=Gamma) -
                      FAdist::pweibull3(rc + Beta, scale=Alpha_RA, thres=Beta, shape=Gamma)"
         @rget area_RA
         sum_RA += (allow_dict[rc] - n_games * area_RA)^2
      end

      total_sum = sum_RS + sum_RA
      push!(ssv, total_sum)
      # @show Gamma, total_sum
      Gamma += 0.01
   end

   @printf "%s %.2f\n" team gammav[findmin(ssv)[2]]
end
