rm(list=ls(all=TRUE))
library(tidyverse)
library(survey)
library(gt)
options(help_type = "html")
library(multcomp)

options(digits = 3)
options(scipen = 10^3)

setwd("C:/Users/54137/ICF/CDC ADIA Project - ICF Private Channel - ICF Private Channel/Study 1+3/Study 3/04_Analysis/")

out <- 'depress'

file_name <- paste0("./01_Data for Analysis/Expanded sample (updated, August)/data_with_weights_", out, "_cau.Rds")
dat <- readRDS(file_name)
table(dat$training.sample)
table(dat$asian)

dat <-  dat %>% filter(training.sample == 0)
dim(dat)

#========================================================================
summary(dat$new_w)
contrasts(dat$ace_ocs)

table(dat$ace_ocs, useNA = "ifany")
table(dat$ace_ocs, dat$female, useNA = "ifany")
table(dat$ace_ocs, dat$white == 0, useNA = "ifany")

dat$orace <- as.numeric(apply(dat[, c("asian_nhpi", "othrace")]          # "asian" excluded (8/15/2023)
    , 1, \(x) sum(x, na.rm = TRUE) >= 1))
dat$mrace <- as.numeric(apply(dat[, c("asian_nhpi", "othrace", "black", "white")]    # "asian" excluded (8/15/2023)
    , 1, \(x) sum(x, na.rm = TRUE) > 1))

sdw    <- svydesign(id = ~ id, weights = ~ new_w, data = dat)
sdw    <- subset(sdw, black == 1 | (white == 1 & mrace == 0 & hisp == 0))

mean(dat$y, na.rm = T)

#========================================================================
#========================================================================
### I. No covaraiters in the regression (adjustment only thorugh weighting)
#========================================================================
#========================================================================
#' @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' family gaussian for continuous outcomes
#' for binnary change to logistic
#' @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
fit0   <- svyglm(y ~ ace_ocs, design = sdw, family = gaussian)
coef(summary(fit0))[2:4, ]

grp   <- c("None", "ACE", "ACE + OCS", "OCS")
pred0 <- predict(fit0, newdata = data.frame(ace_ocs = grp),  type = c("response"))
data.frame(grp, pred0, confint(pred0))

#-----------------------------------------------------------
### A. - black
#-----------------------------------------------------------
#' is the difference between "ACE + OCS" Vs. "ACE alone"
#' worse for black?
#' if it is bad for both groups the diffrence is negative
#' if it is worse for black, the interacction is negative
fit0_m <- svyglm(y ~ ace_ocs * black
    , design = sdw, family = gaussian)
round(coef(summary(fit0_m)), 4)

est <- svyby(~y, ~ ace_ocs + black
    , sdw, svymean, na = TRUE, vartype = c("se", "ci"))
n   <- svyby(~y, ~ ace_ocs + black
    , sdw,  unwtd.count, na = TRUE)
merge(est, n[, -ncol(n)])
merge(est, n[, -ncol(n)]) %>%
  gt %>%
  tab_header(title = "Predicted values by black") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cau_pred_by_black.html"))

#' @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' one sided hypothesis outcome is a good thing
#' otherwise, revert, i.e.,  <=
#' @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

K <- c(
      "ace_ocs2  <= 0" #difference among nonblack
    , "ace_ocs2 + ace_ocs2:black <= 0" #difference among black
    , "ace_ocs2:black <= 0" # difference in difference (black vs. nonblack)
        )

ht_m <- glht(fit0_m, linfct = K)
summary(ht_m)
data.frame(contrasts =
    c("diff among nonblack"
    , "diff among black"
    , "did black vs. nonblack")
    , summary(ht_m)$test[3:6]) %>%
  gt %>%
  tab_header(title = "Test 'ACE + OCS' vs. 'ACE only' by race-ethnic group") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cau_test_by_black.html"))


#-----------------------------------------------------------
###' c. - black & Female
#-----------------------------------------------------------
fit0_mf <- svyglm(y ~ ace_ocs * black * female
    , design = sdw, family = gaussian)
round(coef(summary(fit0_mf)), 4)


est <- svyby(~y, ~ ace_ocs + black + female
    , sdw, svymean, na = TRUE, vartype = c("se", "ci"))
n   <- svyby(~y, ~ ace_ocs + black + female
    , sdw,  unwtd.count, na = TRUE)
merge(est, n[, -ncol(n)])
merge(est, n[, -ncol(n)]) %>%
  gt %>%
  tab_header(title = "Predicted values by black adn female") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cau_pred_by_black_female.html"))

K <- c(
    # (1) difference among nonblack male
     "ace_ocs2  <= 0"
    # (2) difference among black male
    , "ace_ocs2 + ace_ocs2:black <= 0"
    # (3) difference among nonblack female
    , "ace_ocs2 + ace_ocs2:female <= 0"
    # (4) difference among black female
    , "ace_ocs2 + ace_ocs2:female + ace_ocs2:black + ace_ocs2:black:female <= 0"
    # black female Vs. nonblack male (4 - 1)
    , "ace_ocs2:female + ace_ocs2:black + ace_ocs2:black:female <= 0"
        )
ht_mf <- glht(fit0_mf, linfct = K)
summary(ht_mf)
data.frame(contrasts =
    c("diff among nonblack male"
    , "diff among black male"
    , "diff among nonblack female"
    , "diff among black female"
    , "did 'black female' vs. 'nonblack male'")
    , summary(ht_mf)$test[3:6]) %>%
  gt %>%
  tab_header(title = "Test 'ACE + OCS' vs. 'ACE only' by black and female") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cau_test_by_black_female.html"))

#========================================================================
#========================================================================
### II. adding covariates to the regression ("doubly robust")
#========================================================================
#========================================================================
#' "adjusted" predictions,i.e.
#' average over all other covaraite values
#' for gaussian is that not make a difference
#' for nonlinear, like logit, 
#' average proportion over population characteirstics
#' is not  the same as average
#' predicted value for population characteristc hold at at the average value

adj_values <- \(svy_model, grid) {
    sdwr <- as.svrepdesign(svy_model$survey.design)
    rest <- withReplicates(sdwr, \(ww, df) {
            m <- glm(y ~ 1
                , weights = ww
                , family = family(svy_model)
                , data = df)
            m <- update(m, formula(svy_model))
            cdat <- m$model
            s <- !(colnames(cdat) %in% names(grid))
            theta <- sapply(1:nrow(grid), \(i) {
                new_df <- cbind(grid[i, , drop = FALSE], cdat[, s]
                    , row.names = NULL)
                pred <- predict(m, newdata = new_df, "response")
                mean(pred)
            })
            names(theta) <- sapply(1:nrow(grid)
                , \(i) paste0(grid[i,], collapse = ""))
            theta
        }
    )
    pw   <- svy_model$model$"(weights)" > 0
    cdat <- svy_model$model[pw, names(grid), drop = FALSE]
    raw_n <- as.data.frame(ftable(cdat))
    merge(data.frame(grid
    , Est = coef(rest)
    , SE = SE(rest)
    , ci_l = confint(rest)[, 1]
    , ci_u = confint(rest)[, 2]
    ), raw_n)
}

fit_dr <- svyglm(y ~ ace_ocs
                 + female + agegrp
                 + black + white + hisp + asian_nhpi + othrace +
                   + mhighgd_bin
                 + rural + mixur
                 # + asian # no longer available (8/15/2023)
                 , design = sdw, family = gaussian)

coef(summary(fit_dr))[2:4, ]

grid <- expand.grid(ace_ocs = grp)
adj_values(fit_dr, grid)

#-----------------------------------------------------------
### A. - black
#-----------------------------------------------------------
#' is the difference between "ACE + OCS" Vs. "ACE alone"
#' worse for black?
#' if it is bad for both groups the diffrence is negative
#' if it is worse for black, the interacction is negative
fit_dr_m <- svyglm(y ~ ace_ocs * black
    + female + agegrp
    #+ black + white + hisp + asian + asian_nhpi + othrace +
    + mhighgd_bin
    + rural + mixur
    , design = sdw, family = gaussian)
round(coef(summary(fit_dr_m)), 4)

grid <- expand.grid(ace_ocs = grp
    , black = c(0, 1))
adj_values_tb <- adj_values(fit_dr_m, grid)
adj_values_tb
adj_values_tb %>%
  gt %>%
  tab_header(title = "Adjusted predicted values (DR) by black") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cau_pred_by_black_dr.html"))

K <- c(
      "ace_ocs2  <= 0" #difference among nonblack
    , "ace_ocs2 + ace_ocs2:black <= 0" #difference among black
    , "ace_ocs2:black <= 0" # difference in difference (black vs. nonblack)
        )
ht_dr_m <- glht(fit_dr_m, linfct = K)
summary(ht_dr_m)
data.frame(contrasts =
    c("diff among nonblack"
    , "diff among black"
    , "did black vs. nonblack")
    , summary(ht_dr_m)$test[3:6]) %>%
  gt %>%
  tab_header(title = "Doubly Robust Test 'ACE + OCS' vs. 'ACE only' by black") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cau_test_by_black_dr.html"))

#-----------------------------------------------------------
###' c. - black & Female
#-----------------------------------------------------------
fit_dr_mf <- svyglm(y ~ ace_ocs * black * female
    #+ female
    + agegrp
    #+ black + white + hisp + asian + asian_nhpi + othrace +
    + mhighgd_bin
    + rural + mixur
    , design = sdw, family = gaussian)
round(coef(summary(fit_dr_mf)), 4)


grid <- expand.grid(ace_ocs = grp
    , female = c(0, 1)
    , black = c(0, 1)
    )

adj_values_tb <- adj_values(fit_dr_mf, grid)
adj_values_tb
adj_values_tb %>%
  gt %>%
  tab_header(title = "Adjusted predicted values (DR) by black and female") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cau_pred_by_black_female_dr.html"))

K <- c(
    # (1) difference among nonblack male
     "ace_ocs2  <= 0"
    # (2) difference among black male
    , "ace_ocs2 + ace_ocs2:black <= 0"
    # (3) difference among nonblack female
    , "ace_ocs2 + ace_ocs2:female <= 0"
    # (4) difference among black female
    , "ace_ocs2 + ace_ocs2:female + ace_ocs2:black + ace_ocs2:black:female <= 0"
    # black female Vs. nonblack male (4 - 1)
    , "ace_ocs2:female + ace_ocs2:black + ace_ocs2:black:female <= 0"
        )
ht_dr_mf <- glht(fit_dr_mf, linfct = K)
summary(ht_dr_mf)
data.frame(contrasts =
    c("diff among nonblack male"
    , "diff among black male"
    , "diff among nonblack female"
    , "diff among black female"
    , "did 'black female' vs. 'nonblack male'")
    , summary(ht_dr_mf)$test[3:6]) %>%
  gt %>%
  tab_header(title = "Doubly Robust Test 'ACE + OCS' vs. 'ACE only' by black and female") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cau_test_by_black_female_dr.html"))
