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
fit_weibull <- function(team, stat="rs", method="mle") {
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
        ggtitle(paste("2004", team_name, paste0("   [", method, "]"))) +
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
estimates_df <- read_csv("2004_py_exp_estimates.csv", col_types="cdd")

# AL east, central, west
# NL east, central, west
# WSN is MTL
teams   <- c("BOS", "NYY", "BAL", "TBD", "TOR", 
             "MIN", "CHW", "CLE", "DET", "KCR", 
             "ANA", "OAK", "TEX", "SEA",
             "ATL", "PHI", "FLA", "NYM", "WSN",
             "STL", "HOU", "CHC", "CIN", "PIT", "MIL",
             "LAD", "SFG", "SDP", "COL", "ARI")
ls_est <- estimates_df$LS
ls_hash  <- hashmap(teams, ls_est)

mle_est <- estimates_df$MLE
mle_hash <- hashmap(teams, mle_est)

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
