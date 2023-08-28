
## this tests the contrasts between racial minority and non-Hispanic White in the causal model

rm(list=ls(all=TRUE))
library(tidyverse)
library(survey)
library(gt)
options(help_type = "html")
library(multcomp)


options(digits = 3)
options(scipen = 10^3)

setwd("C:/Users/54137/ICF/CDC ADIA Project - ICF Private Channel - ICF Private Channel/Study 1+3/Study 3/04_Analysis/")

out <- 'evrvicr'

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

dat$orace <- as.numeric(apply(dat[, c("asian_nhpi", "othrace")]           # "asian" excluded (8/15/2023)
                              , 1, \(x) sum(x, na.rm = TRUE) >= 1))  
dat$mrace <- as.numeric(apply(dat[, c("asian_nhpi", "othrace", "black", "white")]    # "asian" excluded (8/15/2023)
                              , 1, \(x) sum(x, na.rm = TRUE) > 1))
mean(dat$mrace)

#! nonhispanic white only, rest
dat$minority <- as.numeric(!(dat$white == 1 & dat$mrace == 0 & dat$hisp == 0))
table(dat$minority, dat$black)
table(dat$minority, dat$hisp)

sdw    <- svydesign(id = ~ id, weights = ~ new_w, data = dat)

table(dat$y, useNA = "always")
#========================================================================
#========================================================================
### I. No covaraiters in the regression (adjustment only thorugh weighting)
#========================================================================
#========================================================================
#' @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' family gaussian for continuous outcomes
#' for binnary change to logistic
#' @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
fit0   <- svyglm(y ~ ace_ocs, design = sdw, family = quasibinomial)
summary(fit0)
coef(summary(fit0))[2:4, ]

grp   <- c("None", "ACE", "ACE + OCS", "OCS")
pred0 <- predict(fit0, newdata = data.frame(ace_ocs = grp), type = c("response"))
data.frame(grp, pred0, confint(pred0))

#-----------------------------------------------------------
### A. - Minority
#-----------------------------------------------------------
#' is the difference between "ACE + OCS" Vs. "ACE alone"
#' worse for non-White?
#' if it is bad for both groups the diffrence is negative
#' if it is worse for minority, the interacction is negative
fit0_m <- svyglm(y ~ ace_ocs * minority
                 , design = sdw, family = quasibinomial)
round(coef(summary(fit0_m)), 4)

est <- svyby(~y, ~ ace_ocs + minority
             , sdw, svymean, na = TRUE, vartype = c("se", "ci"))
n   <- svyby(~y, ~ ace_ocs + minority
             , sdw,  unwtd.count, na = TRUE)
merge(est, n[, -ncol(n)])
merge(est, n[, -ncol(n)]) %>%
  gt %>%
  tab_header(title = "Predicted values by minority") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/pred_by_miniority.html"))

#' @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' one sided hypothesis outcome is a good thing
#' otherwise, revert, i.e.,  <=
#' @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

K <- c(
  "ace_ocs2  <= 0" #difference among white
  , "ace_ocs2 + ace_ocs2:minority <= 0" #difference among minority
  , "ace_ocs2:minority <= 0" # difference in difference (minority vs. white)
)

ht_m <- glht(fit0_m, linfct = K)   # One-sided hypothesis specified in linfct
summary(ht_m)
data.frame(contrasts =
             c("diff among white"
               , "diff among minority"
               , "did minority vs. white")
           , summary(ht_m)$test[3:6]) %>%
  gt %>%
  tab_header(title = "Test 'ACE + OCS' vs. 'ACE only' by race-ethnic group") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/test_by_miniority.html"))


#-----------------------------------------------------------
### B.- Female
#-----------------------------------------------------------

fit0_f <- svyglm(y ~ ace_ocs * female
                 , design = sdw, family = quasibinomial)
round(coef(summary(fit0_f)), 4)


est <- svyby(~y, ~ ace_ocs + female, sdw
             , svymean, na = TRUE, vartype = c("se", "ci"))
n   <- svyby(~y, ~ ace_ocs + female, sdw,  unwtd.count, na = TRUE)
merge(est, n[, -ncol(n)])
merge(est, n[, -ncol(n)]) %>%
  gt %>%
  tab_header(title = "Predicted values by female") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/pred_by_female.html"))

K <- c(
  "ace_ocs2  <= 0" #difference among white
  , "ace_ocs2 + ace_ocs2:female <= 0" #difference among minority
  , "ace_ocs2:female <= 0" # difference in difference (minority vs. white)
)
ht_f <- glht(fit0_f, linfct = K)
summary(ht_f)
data.frame(contrasts =
             c("diff among male"
               , "diff among female"
               , "did female vs. male")
           , summary(ht_f)$test[3:6]) %>%
  gt %>%
  tab_header(title = "Test 'ACE + OCS' vs. 'ACE only' by female") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/test_by_female.html"))
#-----------------------------------------------------------
###' c. - Minority & Female
#-----------------------------------------------------------
fit0_mf <- svyglm(y ~ ace_ocs * minority * female
                  , design = sdw, family = quasibinomial)
round(coef(summary(fit0_mf)), 4)


est <- svyby(~y, ~ ace_ocs + minority + female
             , sdw, svymean, na = TRUE, vartype = c("se", "ci"))
n   <- svyby(~y, ~ ace_ocs + minority + female
             , sdw,  unwtd.count, na = TRUE)
merge(est, n[, -ncol(n)])
merge(est, n[, -ncol(n)]) %>%
  gt %>%
  tab_header(title = "Predicted values by minority adn female") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/pred_by_miniority_female.html"))

K <- c(
  # (1) difference among white male
  "ace_ocs2  <= 0"
  # (2) difference among minority male
  , "ace_ocs2 + ace_ocs2:minority <= 0"
  # (3) difference among white female
  , "ace_ocs2 + ace_ocs2:female <= 0"
  # (4) difference among minority female
  , "ace_ocs2 + ace_ocs2:female + ace_ocs2:minority + ace_ocs2:minority:female <= 0"
  # minority female Vs. white male (4 - 1)
  , "ace_ocs2:female + ace_ocs2:minority + ace_ocs2:minority:female <= 0"
)
ht_mf <- glht(fit0_mf, linfct = K)
summary(ht_mf)
data.frame(contrasts =
             c("diff among white male"
               , "diff among minority male"
               , "diff among white female"
               , "diff among minority female"
               , "did 'minority female' vs. 'white male'")
           , summary(ht_mf)$test[3:6]) %>%
  gt %>%
  tab_header(title = "Test 'ACE + OCS' vs. 'ACE only' by minority and female") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/test_by_minority_female.html"))

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
                 + black + white + hisp  + asian_nhpi + othrace +
                   + mhighgd_bin
                 + rural + mixur
                 #+ asian   # no longer available (8/15/2023)
                 , design = sdw, family = quasibinomial)

coef(summary(fit_dr))[2:4, ]

grid <- expand.grid(ace_ocs = grp)
adj_values(fit_dr, grid)

#-----------------------------------------------------------
### A. - Minority
#-----------------------------------------------------------
#' is the difference between "ACE + OCS" Vs. "ACE alone"
#' worse for non-White?
#' if it is bad for both groups the diffrence is negative
#' if it is worse for minority, the interacction is negative
fit_dr_m <- svyglm(y ~ ace_ocs * minority
                   + female + agegrp
                   #+ black + white + hisp + asian + asian_nhpi + othrace +
                   + mhighgd_bin
                   + rural + mixur
                   , design = sdw, family = quasibinomial)
round(coef(summary(fit_dr_m)), 4)

grid <- expand.grid(ace_ocs = grp
                    , minority = c(0, 1))
adj_values_tb <- adj_values(fit_dr_m, grid)
adj_values_tb
adj_values_tb %>%
  gt %>%
  tab_header(title = "Adjusted predicted values (DR) by minority") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/pred_by_miniority_dr.html"))

K <- c(
  "ace_ocs2  <= 0" #difference among white
  , "ace_ocs2 + ace_ocs2:minority <= 0" #difference among minority
  , "ace_ocs2:minority <= 0" # difference in difference (minority vs. white)
)
ht_dr_m <- glht(fit_dr_m, linfct = K)
summary(ht_dr_m)
data.frame(contrasts =
             c("diff among white"
               , "diff among minority"
               , "did minority vs. white")
           , summary(ht_dr_m)$test[3:6]) %>%
  gt %>%
  tab_header(title = "Doubly Robust Test 'ACE + OCS' vs. 'ACE only' by minority") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/test_by_miniority_dr.html"))

#-----------------------------------------------------------
### B.- Female
#-----------------------------------------------------------

fit_dr_f <- svyglm(y ~ ace_ocs * female
                   #+ female
                   + agegrp
                   + black + white + hisp  + asian_nhpi + othrace +
                     + mhighgd_bin
                   + rural + mixur
                   #+ asian # no longer available (8/15/2023)
                   , design = sdw, family = quasibinomial)
round(coef(summary(fit_dr_f)), 4)

grid <- expand.grid(ace_ocs = grp
                    , female = c(0, 1))
adj_values_tb <- adj_values(fit_dr_f, grid)
adj_values_tb
adj_values_tb %>%
  gt %>%
  tab_header(title = "Adjusted predicted values (DR) by female") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/pred_by_female_dr.html"))

K <- c(
  "ace_ocs2  <= 0" #difference among white
  , "ace_ocs2 + ace_ocs2:female <= 0" #difference among minority
  , "ace_ocs2:female <= 0" # difference in difference (minority vs. white)
)
ht_dr_f <- glht(fit_dr_f, linfct = K)
summary(ht_dr_f)
data.frame(contrasts =
             c("diff among male"
               , "diff among female"
               , "did female vs. male")
           , summary(ht_dr_f)$test[3:6]) %>%
  gt %>%
  tab_header(title = "Doubly Robust Test 'ACE + OCS' vs. 'ACE only' by female") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/test_by_female_dr.html"))
#-----------------------------------------------------------
###' c. - Minority & Female
#-----------------------------------------------------------
fit_dr_mf <- svyglm(y ~ ace_ocs * minority * female
                    #+ female
                    + agegrp
                    #+ black + white + hisp + asian + asian_nhpi + othrace +
                    + mhighgd_bin
                    + rural + mixur
                    , design = sdw, family = quasibinomial)
round(coef(summary(fit_dr_mf)), 4)


grid <- expand.grid(ace_ocs = grp
                    , female = c(0, 1)
                    , minority = c(0, 1)
)
adj_values_tb <- adj_values(fit_dr_mf, grid)
adj_values_tb
adj_values_tb %>%
  gt %>%
  tab_header(title = "Adjusted predicted values (DR) by minority and female") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/pred_by_minority_female_dr.html"))

K <- c(
  # (1) difference among white male
  "ace_ocs2  <= 0"
  # (2) difference among minority male
  , "ace_ocs2 + ace_ocs2:minority <= 0"
  # (3) difference among white female
  , "ace_ocs2 + ace_ocs2:female <= 0"
  # (4) difference among minority female
  , "ace_ocs2 + ace_ocs2:female + ace_ocs2:minority + ace_ocs2:minority:female <= 0"
  # minority female Vs. white male (4 - 1)
  , "ace_ocs2:female + ace_ocs2:minority + ace_ocs2:minority:female <= 0"
)
ht_dr_mf <- glht(fit_dr_mf, linfct = K)
summary(ht_dr_mf)
data.frame(contrasts =
             c("diff among white male"
               , "diff among minority male"
               , "diff among white female"
               , "diff among minority female"
               , "did 'minority female' vs. 'white male'")
           , summary(ht_dr_mf)$test[3:6]) %>%
  gt %>%
  tab_header(title = "Doubly Robust Test 'ACE + OCS' vs. 'ACE only' by minority and female") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/test_by_minority_female_dr.html"))
