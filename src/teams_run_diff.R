

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
   # derive run-diff, win-pct, and singles (X1B) columns
teams_1871$RD <- with(teams_1871, R - RA)
teams_1871$Wpct <- with(teams_1871, W / (W + L))
teams_1871$X1B <- with(teams_1871, H - (X2B + X3B + HR))
   # train/test
train_1871 <- sample(1:nrow(teams_1871), nrow(teams_1871) * 0.6667)
test_1871 <- (-train_1871)
# DATA SINCE 1970
teams_1970 <- subset(teams, yearID > 1969)[ , c(01, 04, 07, 09, 10, 15:41)]
   # derive run-diff, win-pct, and singles (X1B) columns
teams_1970$RD <- with(teams_1970, R - RA)
teams_1970$Wpct <- with(teams_1970, W / (W + L))
teams_1970$X1B <- with(teams_1970, H - (X2B + X3B + HR))
   # train/test
train_1970 <- sample(1:nrow(teams_1970), nrow(teams_1970) * 0.6667)
test_1970 <- (-train_1970)

   # response vectors
y_1871 = teams_1871$Wpct
y_1970 = teams_1970$Wpct
#
# END DATA INPUT #


# RUN DIFFERENTIAL

   # ALL DATA since 1871

   # plot and line of best fit for Wpct ~ RD (naive, full dataset)
plot(teams_1871$RD, teams_1871$Wpct,
     xlab="run differential",
     ylab="winning percentage",
     main="Win.Pct vs. Run.Diff, since 1871")
rd_1871_naive.fit <- lm(Wpct ~ RD, data=teams_1871)
summary(rd_1871_naive.fit)
abline(a=coef(rd_1871_naive.fit)[1], b=coef(rd_1871_naive.fit)[2], lwd=2, col="red")
   # predictions and residuals (naive, full dataset)
teams_1871$linWpct <- predict(rd_1871_naive.fit)
teams_1871$linResiduals <- residuals(rd_1871_naive.fit)
plot(teams_1871$RD, teams_1871$linResiduals,
     xlab="run differential",
     ylab="residual",
     xlim=c(-500, 500),
     ylim=c(-0.25, 0.25),
     main="Residuals vs. Run.Diff, Naive, since 1871")
abline(h=0, lty=4)
rd_1871_naive.linRMSE <- sqrt(mean(teams_1871$linResiduals^2))
rd_1871_naive.linRMSE
nrow(subset(teams_1871, abs(linResiduals) < rd_1871_naive.linRMSE)) / nrow(teams_1871)
nrow(subset(teams_1871, abs(linResiduals) < 2 * rd_1871_naive.linRMSE)) / nrow(teams_1871)

   # test/train linear fit on RD
rd_1871_train.fit <- lm(Wpct ~ RD, data=teams_1871[train_1871, ])
summary(rd_1871_train.fit)
rd_1871_test.pred <- predict(rd_1871_train.fit, newdata=teams_1871[test_1871, ])
rd_1871_test.linRMSE <- sqrt(mean((rd_1871_test.pred - y_1871[test_1871])^2))
rd_1871_test.linRMSE 
nrow(subset(teams_1871[test_1871, ], abs(linResiduals) < rd_1871_test.linRMSE)) / 
   nrow(teams_1871[test_1871, ])
nrow(subset(teams_1871[test_1871, ], abs(linResiduals) < 2 * rd_1871_test.linRMSE)) / 
   nrow(teams_1871[test_1871, ])




   # ONLY DATA since 1970

# plot and line of best fit for Wpct ~ RD
plot(teams_1970$RD, teams_1970$Wpct,
     xlab="run differential",
     ylab="winning percentage",
     main="Win.Pct vs. Run.Diff, since 1970")
rd_1970_naive.fit <- lm(Wpct ~ RD, data=teams_1970)
rd_1970_naive.fit
abline(a=coef(rd_1970_naive.fit)[1], b=coef(rd_1871_naive.fit)[2], lwd=2, col="red")
   # predictions and residuals
teams_1970$linWpct <- predict(rd_1970_naive.fit)
teams_1970$linResiduals <- residuals(rd_1970_naive.fit)
plot(teams_1970$RD, teams_1970$linResiduals,
     xlab="run differential",
     ylab="residual",
     xlim=c(-400, 400),
     ylim=c(-0.1, 0.1),
     main="Residuals vs. Run.Diff, Naive, since 1970")
abline(h=0, lty=4)
rd_1970_naive.linRMSE <- sqrt(mean(teams_1970$linResiduals^2))
rd_1970_naive.linRMSE
nrow(subset(teams_1970, abs(linResiduals) < rd_1970_naive.linRMSE)) / nrow(teams_1970)
nrow(subset(teams_1970, abs(linResiduals) < 2 * rd_1970_naive.linRMSE)) / nrow(teams_1970)

   # test/train linear fit on RD
rd_1970_train.fit <- lm(Wpct ~ RD, data=teams_1970[train_1970, ])
summary(rd_1970_train.fit)
rd_1970_test.pred <- predict(rd_1970_train.fit, newdata=teams_1970[test_1970, ])
rd_1970_test.linRMSE <- sqrt(mean((rd_1970_test.pred - y_1970[test_1970])^2))
rd_1970_test.linRMSE 
nrow(subset(teams_1970[test_1970, ], abs(linResiduals) < rd_1970_test.linRMSE)) / 
   nrow(teams_1970[test_1970, ])
nrow(subset(teams_1970[test_1970, ], abs(linResiduals) < 2 * rd_1970_test.linRMSE)) / 
   nrow(teams_1970[test_1970, ])

