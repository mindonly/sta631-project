library(pls)

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

    # SUBSET DATA SINCE 1970
teams_1970 <- subset(teams, yearID > 1969)[ , c(01, 04, 07, 09, 10, 15:41)]
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
   # train/test
train_1970 <- sample(1:nrow(teams_1970), nrow(teams_1970) * 0.6667)
test_1970 <- (-train_1970)
#
# END DATA INPUT #


# PCR and PLS #
#
    # drop non-predictors
    # subset select option, by row below:
    # 1. derived: run-diff, win-pct
    # 2. batting/scoring
    # 3. pitching
    # 4. fielding
    # also removed R, RA, H, OPS

# (with RD)
slim_1970 <- subset(teams_1970, 
                    select = c(RD, Wpct, 
                               AB, X1B, X2B, X3B, HR, SF, BB, HBP, SO, SB, CS, OBP, SLG,
                               ER, ERA, CG, SHO, SV, HA, HRA, BBA, SOA, IPouts, 
                               E, DP, FP))
# (without RD)
slim_1970_noRD <- subset(teams_1970, 
                    select = c(Wpct, 
                               AB, X1B, X2B, X3B, HR, SF, BB, HBP, SO, SB, CS, OBP, SLG,
                               ER, ERA, CG, SHO, SV, HA, HRA, BBA, SOA, IPouts, 
                               E, DP, FP))

# PCR
#
# 1970 PCR with RD #
    # set up model matrix
x <- model.matrix(Wpct ~ ., slim_1970)[, -1]
y <- slim_1970$Wpct
y.test <- y[test_1970]
    # full dataset
pcr.fit.full <- pcr(Wpct ~ ., data=slim_1970, scale=TRUE, validation="CV")
summary(pcr.fit.full)
validationplot(pcr.fit.full, val.type="MSEP")
    # train/test
pcr.fit.train <- pcr(Wpct ~ ., data=slim_1970, subset=train_1970, scale=TRUE, validation="CV")
summary(pcr.fit.train)
validationplot(pcr.fit.train, val.type="MSEP", main="PCR with RD")
    # fit on full dataset, get RMSE
pcr.pred <- predict(pcr.fit.train, x[test_1970, ], ncomp=7)
sqrt(mean((pcr.pred - y.test)^2))
    # loading matrices
loadings(pcr.fit.full)[, 1:9]
abs(loadings(pcr.fit.full)[, 1:9]) > 0.30
loadings(pcr.fit.train)[, 1:9]
abs(loadings(pcr.fit.train)[, 1:9]) > 0.30

# 1970 PCR without RD
    # set up model matrix (no RD)
x <- model.matrix(Wpct ~ ., slim_1970_noRD)[, -1]
y <- slim_1970_noRD$Wpct
y.test <- y[test_1970]
   # full dataset
pcr.fit.full.noRD <- pcr(Wpct ~ ., data=slim_1970_noRD, scale=TRUE, validation="CV")
summary(pcr.fit.full.noRD)
validationplot(pcr.fit.full.noRD, val.type="MSEP")
    # train/test
pcr.fit.train.noRD <- pcr(Wpct ~ ., data=slim_1970_noRD, subset=train_1970, scale=TRUE, validation="CV")
summary(pcr.fit.train.noRD)
validationplot(pcr.fit.train.noRD, val.type="MSEP")
    # fit on full dataset, get MSE
pcr.pred <- predict(pcr.fit.train.noRD, x[test_1970, ], ncomp=26)
sqrt(mean((pcr.pred - y.test)^2))
    # loading matrices
loadings(pcr.fit.full.noRD)[, 1:6]
abs(loadings(pcr.fit.full.noRD)[, 1:6]) > 0.30
loadings(pcr.fit.train.noRD)[, 1:6]
abs(loadings(pcr.fit.train.noRD)[, 1:6]) > 0.30


# PLS
#
# 1970 PLS with RD #
   # set up model matrix
x <- model.matrix(Wpct ~ ., slim_1970)[, -1]
y <- slim_1970$Wpct
y.test <- y[test_1970]
   # full dataset
pls.fit.full <- plsr(Wpct ~ ., data=slim_1970, scale=TRUE, validation="CV")
summary(pls.fit.full)
validationplot(pls.fit.full, val.type="MSEP")
   # train/test
pls.fit.train <- plsr(Wpct ~ ., data=slim_1970, subset=train_1970, scale=TRUE, validation="CV")
summary(pls.fit.train)
validationplot(pls.fit.train, val.type="MSEP", main="PLS with RD")
   # fit on full dataset, get RMSE
pls.pred <- predict(pls.fit.train, x[test_1970, ], ncomp=2)
sqrt(mean((pls.pred - y.test)^2))
   # loading matrices
loadings(pls.fit.full)[, 1:9]
abs(loadings(pls.fit.full)[, 1:9]) > 0.35
loadings(pls.fit.train)[, 1:9]
abs(loadings(pls.fit.train)[, 1:9]) > 0.35

# 1970 PLS without RD #
   # set up model matrix (no RD)
x <- model.matrix(Wpct ~ ., slim_1970_noRD)[, -1]
y <- slim_1970_noRD$Wpct
y.test <- y[test_1970]
   # full dataset
pls.fit.full.noRD <- plsr(Wpct ~ ., data=slim_1970_noRD, scale=TRUE, validation="CV")
summary(pls.fit.full.noRD)
validationplot(pls.fit.full.noRD, val.type="MSEP")
   # train/test
pls.fit.train.noRD <- plsr(Wpct ~ ., data=slim_1970_noRD, subset=train_1970, scale=TRUE, validation="CV")
summary(pls.fit.train.noRD)
validationplot(pls.fit.train.noRD, val.type="MSEP")
   # fit on full dataset, get RMSE
pls.pred <- predict(pls.fit.train.noRD, x[test_1970, ], ncomp=2)
sqrt(mean((pls.pred - y.test)^2))
   # loading matrices
loadings(pls.fit.full.noRD)[, 1:6]
abs(loadings(pls.fit.full.noRD)[, 1:6]) > 0.35
loadings(pls.fit.train.noRD)[, 1:6]
abs(loadings(pls.fit.train.noRD)[, 1:6]) > 0.35

#
# END PCR and PLS #
