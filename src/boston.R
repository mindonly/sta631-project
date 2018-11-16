library(readr)
library(FAdist)
library(ggplot2)

setwd("~/Dropbox/f2018/sta631/Project/data/2004")

# NL_E_vec <- c("ATL", "PHI", "FLA", "NYM", "MTL")
# NL_C_vec <- c("STL", "HOU", "CHC", "CIN", "PIT", "MIL")
# NL_W_vec <- c("LAD", "SFG", "SDP", "COL", "ARI")

bos_rs <- read_csv("Dropbox/f2018/sta631/Project/data/2004/bos_rs.csv")
bos_ra <- read_csv("Dropbox/f2018/sta631/Project/data/2004/bos_ra.csv")

rs <- rep(bos_rs$Runs, bos_rs$Games)
ra <- rep(bos_ra$Runs, bos_ra$Games)


team_weibull_fits <- function(team) {
    
    # 'stat_function' parameters
    games = 162
    shape = 1.82
    bw = 1
    
    df_rs <- data.frame(rs)
    p1 <- ggplot(df_rs, aes(rs)) +
        xlim(c(-0.5, 20)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
        geom_histogram(binwidth = bw) +
        stat_function(
            fun = function(x) 
                games * dweibull3(x, shape=shape, scale=mean(rs), thres=-0.5), color='blue'
        )
    
    df_ra <- data.frame(ra)
    p2 <- ggplot(df_ra, aes(ra)) +
        xlim(c(-0.5, 20)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
        geom_histogram(binwidth = bw) +
        stat_function(
            fun = function(x) 
                games * dweibull3(x, shape=shape, scale=mean(ra), thres=-0.5), color='blue'
        )
    
    print(p1)
    print(p2)
}