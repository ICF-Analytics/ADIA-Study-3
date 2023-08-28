
## this tests the contrasts between Hispanic and non-Hispanic White in the classical model

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

file_name <- paste0("./01_Data for Analysis/Expanded sample (updated, August)/data_with_weights_", out, "_cls.Rds")
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
table(dat$white, dat$black, useNA = "ifany")

dat$orace <- as.numeric(apply(dat[, c("asian_nhpi", "othrace")]            # asian excluded (8/15/2023)
    , 1, \(x) sum(x, na.rm = TRUE) >= 1))
dat$mrace <- as.numeric(apply(dat[, c("asian_nhpi", "othrace", "black", "white")]       # asian excluded (8/15/2023)
    , 1, \(x) sum(x, na.rm = TRUE) > 1))

sdw    <- svydesign(id = ~ id, weights = ~ new_w, data = dat)
sdw    <- subset(sdw, hisp == 1 | (white == 1 & mrace == 0))

mean(dat$y, na.rm = T)
#========================================================================
#========================================================================
### I. No covariaters in the regression (adjustment only thorough weighting)
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
### A. - hisp
#-----------------------------------------------------------
#' is the difference between "ACE + OCS" Vs. "ACE alone"
#' worse for Hispanics?
#' if it is bad for both groups the diffrence is negative
#' if it is worse for hisp, the interacction is negative
fit0_m <- svyglm(y ~ ace_ocs * hisp
    , design = sdw, family = gaussian)
round(coef(summary(fit0_m)), 4)

est <- svyby(~y, ~ ace_ocs + hisp
    , sdw, svymean, na = TRUE, vartype = c("se", "ci"))
n   <- svyby(~hisp, ~ ace_ocs + hisp
    , sdw,  unwtd.count, na = TRUE)


merge(est, n[, -ncol(n)])
merge(est, n[, -ncol(n)]) %>%
  gt %>%
  tab_header(title = "Predicted values by hisp") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cls_pred_by_hisp.html"))

#' @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' one sided hypothesis outcome is a good thing
#' otherwise, revert, i.e.,  <=
#' @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

K <- c(
      "ace_ocs2  <= 0" #difference among nonhisp
    , "ace_ocs2 + ace_ocs2:hisp <= 0" #difference among hisp
    , "ace_ocs2:hisp <= 0" # difference in difference (hisp vs. nonhisp)
        )

ht_m <- glht(fit0_m, linfct = K)
summary(ht_m)
data.frame(contrasts =
    c("diff among nonhisp"
    , "diff among hisp"
    , "did hisp vs. nonhisp")
    , summary(ht_m)$test[3:6]) %>%
  gt %>%
  tab_header(title = "Test 'ACE + OCS' vs. 'ACE only' by hisp") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cls_test_by_hisp.html"))


#-----------------------------------------------------------
###' c. - hisp & Female
#-----------------------------------------------------------
fit0_mf <- svyglm(y ~ ace_ocs * hisp * female
    , design = sdw, family = gaussian)
round(coef(summary(fit0_mf)), 4)


est <- svyby(~y, ~ ace_ocs + hisp + female
    , sdw, svymean, na = TRUE, vartype = c("se", "ci"))
n   <- svyby(~y, ~ ace_ocs + hisp + female
    , sdw,  unwtd.count, na = TRUE)
merge(est, n[, -ncol(n)])
merge(est, n[, -ncol(n)]) %>%
  gt %>%
  tab_header(title = "Predicted values by hisp adn female") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cls_pred_by_hisp_female.html"))

K <- c(
    # (1) difference among nonhisp male
     "ace_ocs2  <= 0"
    # (2) difference among hisp male
    , "ace_ocs2 + ace_ocs2:hisp <= 0"
    # (3) difference among nonhisp female
    , "ace_ocs2 + ace_ocs2:female <= 0"
    # (4) difference among hisp female
    , "ace_ocs2 + ace_ocs2:female + ace_ocs2:hisp + ace_ocs2:hisp:female <= 0"
    # hisp female Vs. nonhisp male (4 - 1)
    , "ace_ocs2:female + ace_ocs2:hisp + ace_ocs2:hisp:female <= 0"
        )
ht_mf <- glht(fit0_mf, linfct = K)
summary(ht_mf)
data.frame(contrasts =
    c("diff among nonhisp male"
    , "diff among hisp male"
    , "diff among nonhisp female"
    , "diff among hisp female"
    , "did 'hisp female' vs. 'nonhisp male'")
    , summary(ht_mf)$test[3:6]) %>%
  gt %>%
  tab_header(title = "Test 'ACE + OCS' vs. 'ACE only' by hisp and female") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cls_test_by_hisp_female.html"))

#========================================================================
#========================================================================
#========================================================================
### II. adding covariates to the regression ("doubly robust")
#========================================================================
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
                 #+ asian  # no longer available (8/15/2023)
                 , design = sdw, family = gaussian)

coef(summary(fit_dr))[2:4, ]

grid <- expand.grid(ace_ocs = grp)
adj_values(fit_dr, grid)
#as.data.frame(ftable(fit_dr$model[, names(grid), drop = FALSE]))
#as.data.frame(ftable(fit_dr$model[fit_dr$model$"(weights)" > 0, names(grid), drop = FALSE]))




#-----------------------------------------------------------
### A. - hisp
#-----------------------------------------------------------
#' is the difference between "ACE + OCS" Vs. "ACE alone"
#' worse for hisp?
#' if it is bad for both groups the diffrence is negative
#' if it is worse for hisp, the interacction is negative
fit_dr_m <- svyglm(y ~ ace_ocs * hisp
    + female + agegrp
    #+ black + white + hisp + asian + asian_nhpi + othrace +
    + mhighgd_bin
    + rural + mixur
    , design = sdw, family = gaussian)
round(coef(summary(fit_dr_m)), 4)

grid <- expand.grid(ace_ocs = grp
    , hisp = c(0, 1))

dim(dat)
table(fit_dr_m$model[, names(grid), drop = FALSE])

summary(fit_dr_m$model[, names(grid), drop = FALSE])
as.data.frame(ftable(fit_dr_m$model[, names(grid), drop = FALSE]))
fit_dr_m$model


adj_values_tb <- adj_values(fit_dr_m, grid)
adj_values_tb
adj_values_tb %>%
  gt %>%
  tab_header(title = "Adjusted predicted values (DR) by Hispanic") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cls_pred_by_hisp_dr.html"))

K <- c(
      "ace_ocs2  <= 0" #difference among nonhisp
    , "ace_ocs2 + ace_ocs2:hisp <= 0" #difference among hisp
    , "ace_ocs2:hisp <= 0" # difference in difference (hisp vs. nonhisp)
        )
ht_dr_m <- glht(fit_dr_m, linfct = K)
summary(ht_dr_m)
data.frame(contrasts =
    c("diff among nonhisp"
    , "diff among hisp"
    , "did hisp vs. nonhisp")
    , summary(ht_dr_m)$test[3:6]) %>%
  gt %>%
  tab_header(title = "Doubly Robust Test 'ACE + OCS' vs. 'ACE only' by Hispanic") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cls_test_by_hisp_dr.html"))
#-----------------------------------------------------------
###' c. - hisp & Female
#-----------------------------------------------------------
fit_dr_mf <- svyglm(y ~ ace_ocs * hisp * female
    #+ female
    + agegrp
    #+ black + white + hisp + asian + asian_nhpi + othrace +
    + mhighgd_bin
    + rural + mixur
    , design = sdw, family = gaussian)
round(coef(summary(fit_dr_mf)), 4)


grid <- expand.grid(ace_ocs = grp
    , female = c(0, 1)
    , hisp = c(0, 1)
    )

adj_values_tb <- adj_values(fit_dr_mf, grid)
adj_values_tb
adj_values_tb %>%
  gt %>%
  tab_header(title = "Adjusted predicted values (DR) by hisp and female") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cls_pred_by_hisp_female_dr.html"))

K <- c(
    # (1) difference among nonhisp male
     "ace_ocs2  <= 0"
    # (2) difference among hisp male
    , "ace_ocs2 + ace_ocs2:hisp <= 0"
    # (3) difference among nonhisp female
    , "ace_ocs2 + ace_ocs2:female <= 0"
    # (4) difference among hisp female
    , "ace_ocs2 + ace_ocs2:female + ace_ocs2:hisp + ace_ocs2:hisp:female <= 0"
    # hisp female Vs. nonhisp male (4 - 1)
    , "ace_ocs2:female + ace_ocs2:hisp + ace_ocs2:hisp:female <= 0"
        )
ht_dr_mf <- glht(fit_dr_mf, linfct = K)
summary(ht_dr_mf)
data.frame(contrasts =
    c("diff among nonhisp male"
    , "diff among hisp male"
    , "diff among nonhisp female"
    , "diff among hisp female"
    , "did 'hisp female' vs. 'nonhisp male'")
    , summary(ht_dr_mf)$test[3:6]) %>%
  gt %>%
  tab_header(title = "Doubly Robust Test 'ACE + OCS' vs. 'ACE only' by hisp and female") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cls_test_by_hisp_female_dr.html"))
