library(readr)
library(FAdist)
library(hashmap)


season <- 2004
# AL east, central, west
AL_teams <- c("BOS", "NYY", "BAL", "TBD", "TOR", 
              "MIN", "CHW", "CLE", "DET", "KCR", 
              "ANA", "OAK", "TEX", "SEA")
# NL east, central, west
# WSN is MTL
NL_teams <- c("ATL", "PHI", "FLA", "NYM", "WSN",
              "STL", "HOU", "CHC", "CIN", "PIT", "MIL",
              "LAD", "SFG", "SDP", "COL", "ARI")

for (team in NL_teams) {
    setwd(paste0("~/Dropbox/f2018/sta631/Project/data/", season))
    
    rs_dataset <- paste0(team, "_rs.csv")
    ra_dataset <- paste0(team, "_ra.csv")
    
    score_summary_df <- read_csv(rs_dataset, col_types="iiiid")
    score_seq <- rep(score_summary_df$Runs, score_summary_df$Games)
    score_hash <- hashmap(score_summary_df$Runs, score_summary_df$Games)
    
    allow_summary_df <- read_csv(ra_dataset, col_types="iiiid")
    allow_seq <- rep(allow_summary_df$Runs, allow_summary_df$Games)
    allow_hash <- hashmap(allow_summary_df$Runs, allow_summary_df$Games)
    
    # run_stat: RS or RA
    RS = mean(score_seq)
    RA = mean(allow_seq)
    
    n_games = 162
    
    # threshold, location, shift (Beta)
    Beta = -0.5
    
    sum = 0
    
    gamma_vector = vector()
    ss_vector = vector()
    
    # shape (Gamma)
    Gamma = 1.60
    while (Gamma <= 2.05) {
        gamma_vector <- append(gamma_vector, Gamma)
        sum_RS = 0
        sum_RA = 0
        for (rc in score_summary_df$Runs) {
            # scale (Alpha)
            Alpha_RS = (RS - Beta) / (gamma(1 + 1/Gamma))
            area_RS = pweibull3(rc - Beta, scale=Alpha_RS, thres=Beta, shape=Gamma) - 
                      pweibull3(rc + Beta, scale=Alpha_RS, thres=Beta, shape=Gamma)
            sum_RS = sum_RS + (score_hash[[rc]] - n_games * area_RS)^2
        }
        for (rc in allow_summary_df$Runs) {
            # scale (Alpha)
            Alpha_RA = (RA - Beta) / (gamma(1 + 1/Gamma))
            area_RA = pweibull3(rc - Beta, scale=Alpha_RA, thres=Beta, shape=Gamma) -
                      pweibull3(rc + Beta, scale=Alpha_RA, thres=Beta, shape=Gamma)
            sum_RA = sum_RA + (allow_hash[[rc]] - n_games * area_RA)^2
        }
        sum = sum_RS + sum_RA
        ss_vector <- append(ss_vector, sum)
        # print(c(Gamma, sum))
        Gamma = Gamma + 0.01
    }
    
    min_idx <- which.min(ss_vector)
    print(c(team, gamma_vector[min_idx]))
}