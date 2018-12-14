library(car)
library(pls)
library(glmnet)

# BEGIN DATA INPUT #
#
    # reproducibility
set.seed(42)
    # Lahman's baseball databank
setwd("~/Dropbox/f2018/sta631/Project/baseballdatabank-master/core/")
    # intentionally using base not 'readr' to input CSV; HBP and SF columns mis-handled by 'readr'
teams <- read.csv("Teams.csv")
    # drop any winless teams: e.g. 1872 NAT & 1873 MAR
    # keeping them in breaks lm(logWratio) call below
teams <- subset(teams, W != 0)
    # ALL DATA SINCE 1871
teams_1871 <- subset(teams)[ , c(01, 04, 07, 09, 10, 15:41)]
train_1871 <- sample(1:nrow(teams_1871), nrow(teams_1871) * 0.6667)
test_1871 <- (-train_1871)
    # DATA SINCE 1970
teams_1970 <- subset(teams, yearID > 1969)[ , c(01, 04, 07, 09, 10, 15:41)]
train_1970 <- sample(1:nrow(teams_1970), nrow(teams_1970) * 0.6667)
test_1970 <- (-train_1970)
#
# END DATA INPUT #


# BEGIN ALL DATA ANALYSIS #
#
# RUN DIFF ONLY








# LINEAR
    # derive run-diff, win-pct, and singles (X1B) columns
teams_1871$RD <- with(teams_1871, R - RA)
teams_1871$Wpct <- with(teams_1871, W / (W + L))
teams_1871$X1B <- with(teams_1871, H - (X2B + X3B + HR))

# pythagorean expectation #
    # linear fit for win-pct on run-diff
lin.fit <- lm(Wpct ~ RD, data=teams_1871)
    # derive pythagorean win.pct column
teams_1871$pytWpct <- with(teams_1871, R^2 / (R^2 + RA^2))
    # log win ratio (pyth. exp. exponent)
teams_1871$logWratio <- log(teams_1871$W / teams_1871$L)
teams_1871$logRratio <- log(teams_1871$R / teams_1871$RA)
pyt_1871.fit <- lm(logWratio ~ 0 + logRratio, data=teams_1871)
(pyt_1871.fit)

    # drop non-predictors
    # select option, by row:
    # derived: run-diff, win-pct
    # batting/scoring
    # pitching
    # fielding
    # NOTE: SO, CS, SB removed due to lack of complete cases
    # also removed R, RA, AB, H, and IPouts
slim_1871 <- subset(teams_1871, 
                    select = c(RD, Wpct, 
                               X1B, X2B, X3B, HR, BB,
                               ER, ERA, CG, SHO, SV, HA, HRA, BBA, SOA, 
                               E, DP, FP))

slim_1871_noRD <- subset(teams_1871, 
                    select = c(Wpct, 
                               X1B, X2B, X3B, HR, BB,
                               ER, ERA, CG, SHO, SV, HA, HRA, BBA, SOA, 
                               E, DP, FP))

    # set up model matrix
x <- model.matrix(Wpct ~ ., slim_1871)[, -1]
y <- slim_1871$Wpct
y.test <- y[test_1871]

# PCR
# 1871 PCR with RD #
    # full dataset
pcr.fit.full <- pcr(Wpct ~ ., data=slim_1871, scale=TRUE, validation="CV")
summary(pcr.fit.full)
validationplot(pcr.fit.full, val.type="MSEP")
    # train/test
pcr.fit.train <- pcr(Wpct ~ ., data=slim_1871, subset=train_1871, scale=TRUE, validation="CV")
summary(pcr.fit.train)
validationplot(pcr.fit.train, val.type="MSEP")
    # fit on full dataset, get MSE
pcr.pred <- predict(pcr.fit.train, x[test_1871, ], ncomp=4)
mean((pcr.pred - y.test)^2)
    # loading matrices
loadings(pcr.fit.full)[, 1:6]
abs(loadings(pcr.fit.full)[, 1:6]) > 0.45
loadings(pcr.fit.train)[, 1:6]
abs(loadings(pcr.fit.train)[, 1:6]) > 0.45

    # set up model matrix (no RD)
x <- model.matrix(Wpct ~ ., slim_1871_noRD)[, -1]
y <- slim_1871_noRD$Wpct
y.test <- y[test_1871]

# 1871 PCR without RD #
pcr.fit.full <- pcr(Wpct ~ ., data=slim_1871_noRD, scale=TRUE, validation="CV")
summary(pcr.fit.full)
validationplot(pcr.fit.full, val.type="MSEP")
    # train/test
pcr.fit.train <- pcr(Wpct ~ ., data=slim_1871_noRD, subset=train_1871, scale=TRUE, validation="CV")
summary(pcr.fit.train)
validationplot(pcr.fit.train, val.type="MSEP")
    # fit on full dataset, get MSE
pcr.pred <- predict(pcr.fit.train, x[test_1871, ], ncomp=14)
mean((pcr.pred - y.test)^2)
    # loading matrices
loadings(pcr.fit.full)[, 1:6]
abs(loadings(pcr.fit.full)[, 1:6]) > 0.45
loadings(pcr.fit.train)[, 1:6]
abs(loadings(pcr.fit.train)[, 1:6]) > 0.45


# PLS
#
# END ALL DATA ANALYSIS #


# BEGIN SINCE 1970 DATA ANALYSIS #
#
# LINEAR

    # derive run-diff, win-pct, and singles (X1B) columns
teams_1970$RD <- with(teams_1970, R - RA)
teams_1970$Wpct <- with(teams_1970, W / (W + L))
teams_1970$X1B <- with(teams_1970, H - (X2B + X3B + HR))
    # deriv on-base percent (OBP), slugging percent (SLG), and OPS (on-base + slugging)
    # only possible for data since 1970 due to HBP and SF
teams_1970$OBP <- with(teams_1970, (H + BB + HBP) / (AB + BB + HBP +SF) )
#teams_1970$SLG <- with(teams_1970, ( (H - (X2B + X3B + HR) + (2*X2B) + (3*X3B) + (4*HR)) ) / AB)
teams_1970$SLG <- with(teams_1970, (X1B + (2*X2B) + (3*X3B) + (4*HR)) / AB)
teams_1970$OPS <- with(teams_1970, OBP + SLG)

# pythagorean expectation #
    # linear fit for win-pct on run-diff
lin.fit <- lm(Wpct ~ RD, data=teams_1970)
    # derive pythagorean win.pct column
teams_1970$pytWpct <- with(teams_1970, R^2 / (R^2 + RA^2))
    # log win ratio (pyth. exp. exponent)
teams_1970$logWratio <- log(teams_1970$W / teams_1970$L)
teams_1970$logRratio <- log(teams_1970$R / teams_1970$RA)
pyt_1970.fit <- lm(logWratio ~ 0 + logRratio, data=teams_1970)
(pyt_1970.fit)

# best subset
# forward selection
# backwared selection
# ridge regression
# lasso
# elastic net
# PCR
# PLS
#
# END SINCE 1970 DATA ANALYSIS #
