library(readr)
library(FAdist)
library(Lahman)
library(cowplot)
library(ggplot2)
library(hashmap)

data("Teams")

# Reference:
# Miller, S. J. (2005). A Derivation of the Pythagorean Won-Loss Formula in Baseball. 
#   ArXiv:Math/0509698. Retrieved from http://arxiv.org/abs/math/0509698

# FUNCTIONS
fit_weibull <- function(team, stat="rs", method="ls") {
    team_name <- teams_df$name[which(teams_df$franchID == team)]
    
    # threshold, location, shift (Beta)
    Beta = -0.5
    
    # shape (Gamma)
    if (method == "mle") { 
        Gamma = mle_hash[[team]]
    } else if (method == "ls") {
        Gamma = ls_hash[[team]]
    }
   
    if (stat == "rs") { 
        dataset <- paste0(team, "_rs.csv")
        x_lab <- "runs scored"
    } else if (stat == "ra") {
        dataset <- paste0(team, "_ra.csv")
        x_lab <- "runs allowed"
    }
    scoring_summary_df <- read_csv(dataset, col_types="iiiid")
    score_seq <- rep(scoring_summary_df$Runs, scoring_summary_df$Games)
    score_seq_df <- data.frame(score_seq)
    
    # run_stat: RS or RA
    run_stat = mean(score_seq)
    
    # scale (Alpha)
    Alpha = (run_stat - Beta) / (gamma(1 + 1/Gamma))
    
    # MLB season: 162 games
    n_games = 162
    bw = 1
    
    p <- ggplot(score_seq_df, aes(score_seq)) +
        ggtitle(paste("2004", team_name)) +
        geom_histogram(binwidth = bw, col="blue", fill="grey") +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
        xlim(-0.5, NA) +
        xlab(x_lab) +
        ylab("games") +
        stat_function(
            fun = function(x)
                
                # 3-paramater Weibull distribution: Alpha, Beta, Gamma
                { n_games * dweibull3(x, scale=Alpha, thres=Beta, shape=Gamma) },
                
            color='red',
            size=0.65
        )
    
    return(p)
}

twofit <- function(team1, team2) {
    p1 <- fit_weibull(team1, "rs")
    p2 <- fit_weibull(team1, "ra")
    p3 <- fit_weibull(team2, "rs")
    p4 <- fit_weibull(team2, "ra")
    
    plot_grid(p1, p3, p2, p4, nrow=2, ncol=2)
}
# END FUNCTIONS

season <- 2004
setwd(paste0("~/Dropbox/f2018/sta631/Project/data/", season))
teams_df <- subset(Teams, yearID == season)

# AL east, central, west
# NL east, central, west
teams <- c("BOS", "NYY", "BAL", "TBD", "TOR", 
           "MIN", "CHW", "CLE", "DET", "KCR", 
           "ANA", "OAK", "TEX", "SEA",
           "ATL", "PHI", "FLA", "NYM", "WSN",
           "STL", "HOU", "CHC", "CIN", "PIT", "MIL",
           "LAD", "SFG", "SDP", "COL", "ARI")
# least squares estimates
ls_est  <- c(1.80, 1.77, 1.63, 1.82, 2.01,
             1.80, 1.71, 1.81, 1.76, 1.80,
             1.68, 1.79, 1.88, 1.76,
             1.70, 1.92, 1.65, 1.60, 1.60,
             1.83, 1.74, 1.67, 1.84, 1.61, 1.60, 
             1.80, 1.93, 1.80, 1.84, 1.75)
ls_hash  <- hashmap(teams, ls_est)

# max. likelihood estimates
# AL_mle <- c(1.82, 1.78, 1.66, 1.83, 1.97,
#             1.79, 1.73, 1.79, 1.78, 1.76,
#             1.71, 1.76, 1.90, 1.78)
# AL_mle_hash <- hashmap(AL_teams, AL_mle)

# AL
twofit("BOS", "NYY")
twofit("BAL", "TBD")
twofit("TOR", "MIN")
twofit("CHW", "CLE")
twofit("DET", "KCR")
twofit("ANA", "OAK")
twofit("TEX", "SEA")
# NL
twofit("ATL", "PHI")
twofit("FLA", "NYM")
twofit("WSN", "STL")    # WSN is MTL
twofit("HOU", "CHC")
twofit("CIN", "PIT")
twofit("MIL", "LAD")
twofit("SFG", "SDP")
twofit("COL", "ARI")

#
# TODO
# main(season_yr, league, method)
# season_yr: integer
# league: string ("AL" or "NL")
# method: string ("mle" or "ls")
#
