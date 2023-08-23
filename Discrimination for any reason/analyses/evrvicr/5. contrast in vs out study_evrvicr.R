rm(list=ls(all=TRUE))
library(tidyverse)
library(survey)
library(gt)
options(help_type = "html")
library(multcomp)

options(digits = 3)
options(scipen = 10^3)

setwd("C:/Users/54137/ICF/CDC ADIA Project - ICF Private Channel - ICF Private Channel/Study 1+3/Study 3/04_Analysis/")

out <- "evrvicr"

#=======================================================================
###' 1.- recover inidividuals excluded from Study 3 sample
#=======================================================================
#' a. - Apply the same preprocessing
#' b.- create the same 4 groups
#' c.- compute balancing weigths

dat0 <- haven:::read_sas("./03_SAS Data Management Files/Data/finalvar_08112023.sas7bdat") # Race variables updated in this file
table(dat0$white)
table(dat0$asian)

table(dat0$discrim, useNA = "ifany")
dat0 <- subset(dat0, is.na(discrim) | discrim == 0)
dim(dat0)
#------------------------------------------------------------------------
### a.- preprocess
#------------------------------------------------------------------------
dat0 <-
  dat0 %>%
  mutate(
    id  = childid,
    yob = biryear_xrnd,
    sex = ygender_xrnd,
    mhhinco = mhhinco_adj,
    DISC = discrim_reason,
    female = sex - 1,
    ageo = 2018 - yob,
    agegrp = case_when(
      ageo <= 17 & ageo >= 0  ~ 1,
      ageo <= 24 & ageo >= 18 ~ 2,
      ageo <= 29 & ageo >= 25 ~ 3,
      ageo <= 34 & ageo >= 30 ~ 4,
      ageo <= 39 & ageo >= 35 ~ 5,
      ageo <= 50 & ageo >= 40 ~ 6),
    rural = as.numeric(urbnrur == 0),
    mixur = as.numeric(urbnrur == 2))


anyACE_T_list  <- c("incarce", "divorce",  "physabu",  "subsuse",  "mentill")
dat0$anyACE_T   <- apply(dat0[, anyACE_T_list], 1, sum, na.rm = TRUE) >= 1
dat0$anyACE_T   <- as.integer(dat0$anyACE_T)
table(dat0$anyACE_T, useNA = "ifany")

#------------------------------------------------------------------------
### b.-  create the same 4 groups
#------------------------------------------------------------------------
dat0 <-
  dat0 %>%
  mutate(ace_ocs =
           case_when(
             anyACE_T == 1 & !(bneedin == 1) ~ "ACE"
             , anyACE_T == 1 &  (bneedin == 1) ~ "ACE + OCS"
             , anyACE_T == 0 &  (bneedin == 1) ~ "OCS"
             , anyACE_T == 0 & !(bneedin == 1) ~ "None"
           )
         , ace_ocs = factor(ace_ocs, levels = c("None", "ACE", "ACE + OCS", "OCS"))
  )

table(dat0$ace_ocs, useNA = "ifany")


#------------------------------------------------------------------------
### c.- Find balancing weights so groups are similar
#------------------------------------------------------------------------
#Entropy Balancing, #Reference: #https://web.stanford.edu/~jhain/Paper/eb.pdf
source("./02_R Scripts/ebw.r")

z <- c("female", "agegrp", "white", 
       "hisp",
       "black", 
       # "asian",  # no longer available (8/15/2023)
       "asian_nhpi",
       "othrace", 
       "mhighgd_bin",
       "rural", 
       "mixur"
)

dat0$Z <- dat0[, z]
# In other to balance the missing pattern we need to:
# for categorical variables, create an NA category (addNA)
# for continuous, add indicator is.na and impute mean
dat0$C <-
  dat0$Z %>%
  mutate(across(
    where(is.factor)
    , addNA, ifany = TRUE))  %>%
  mutate(across(
    where(~ is.numeric(.x) && any(is.na(.x)) && n_distinct(.x) == 2)
    , ~ addNA(factor(.x)))) %>%
  mutate(across(
    where(~ is.numeric(.x) && any(is.na(.x)))
    , is.na, .names = "NA_{.col}")) %>%
  mutate(across(
    where(is.numeric)
    , ~ replace(., is.na(.), mean(., na.rm = TRUE))))  %>%
  model.matrix(~., .) %>%
  .[, -1]
colMeans(dat0$C)
#edit 2/2 upped rare attribute threshhold to 5%
# will not balance very rare attributes (les than 5%)
dat0$C <- dat0$C[, colMeans(dat0$C) > .05]
# NA black, etc. are repetead
dat0$C <- dat0$C[, !colnames(dat0$C) %in%
                   c("NA_whiteTRUE", "NA_blackTRUE",
                     "NA_hispTRUE",
                     "NA_asian_nhpiTRUE", "NA_othraceTRUE",
                     "NA_ruralTRUE", "NA_mixurTRUE")]
# note that NA_ruralTRUE is of size zero in the only ACE group
colMeans(dat0$C)


# we can target any fixed population
# here just overal mean, kids with average characteristics
tgt  <- colMeans(dat0$C)
cbind(
  sapply(split(dat0, dat0$ace_ocs), \(d) with(d, colMeans(d$C)))
  , tgt)

grp <- c("None", "ACE", "ACE + OCS", "OCS")
weights <-
  lapply(grp, \(g) {
    with(dat0[!is.na(dat0$ace_ocs) & dat0$ace_ocs == g, ],
         find_weights(id = id, covariates = C
                      , target.margins = tgt, base.weight = w)
    )
  }) %>%
  bind_rows



colnames(weights)[2] <- "new_w"
dat0  <- left_join(dat0, weights, by = "id")
tapply(dat0$new_w, dat0$ace_ocs, sum, na.rm = TRUE)
dat0$new_w[is.na(dat0$new_w)] <- 0

tapply(dat0$new_w, dat0$ace_ocs, sum)

tb_r <-
  with(dat0, data.frame(C, w, ace_ocs)) %>%
  subset(!is.na(ace_ocs)) %>%
  group_by(grp = ace_ocs) %>%
  summarize(across(all_of(colnames(dat0$C))
                   , ~ weighted.mean(., w)), n = n()) %>%
  pivot_longer(!grp, names_to = "var", values_to = "freq") %>%
  pivot_wider(names_from = "grp", values_from = "freq")

tb_w <-
  with(dat0, data.frame(C, new_w, ace_ocs)) %>%
  subset(!is.na(ace_ocs)) %>%
  group_by(grp = ace_ocs) %>%
  summarize(across(all_of(colnames(dat0$C))
                   , ~ weighted.mean(., new_w)), n = n()) %>%
  pivot_longer(!grp, names_to = "var", values_to = "freq") %>%
  pivot_wider(names_from = "grp", values_from = "freq")

left_join(tb_r, tb_w, by = "var") %>% data.frame()


#saving the output in nice format
left_join(tb_r, tb_w, by = "var", suffix = c(".raw", ".weighted")) %>%
  gt %>%
  tab_header(title = "Before and after weighting") %>%
  fmt_number(
    columns = everything() & !var,
    rows = nrow(tb_r),
    decimals = 0,
    use_seps = FALSE
  ) %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cau_raw_weighted.html"))

dat0$y <- unlist(dat0[, out])

#=======================================================================
### 2.- Quick comparisson  Study 3 sample vs rest for reference
#=======================================================================

file_name <- paste0("./01_Data for Analysis/Expanded sample (updated, August)/data_with_weights_", out, "_cau.Rds")
dat <- readRDS(file_name)
dat <-  dat %>% filter(training.sample == 0)
sdw    <- svydesign(id = ~ id, weights = ~ new_w, data = dat)
fit0   <- svyglm(y ~ ace_ocs, design = sdw, family = quasibinomial)
fit_dr <- svyglm(y ~ ace_ocs
                 + female + agegrp
                 + black + white + hisp + asian_nhpi + othrace +
                   + mhighgd_bin
                 + rural + mixur
                 #+ asian # No longer available (8/15/2023)
                 , design = sdw
                 , family = quasibinomial)

table(dat$white)
table(dat$asian)

#a.- only weights
sdw0    <- svydesign(id = ~ id, weights = ~ new_w, data = dat0)
fit00   <- svyglm(y ~ ace_ocs, design = sdw0, family = quasibinomial)
coef(summary(fit00))["ace_ocs2", ]
coef(summary(fit0))["ace_ocs2", ]


#b.- doubly-robust fit
fit_dr0 <- svyglm(y ~ ace_ocs
                  + female + agegrp
                  + black + white + hisp + asian_nhpi + othrace +
                    + mhighgd_bin
                  + rural + mixur
                  #+ asian # No longer available (8/15/2023)
                  , design = sdw0
                  , family = quasibinomial)
coef(summary(fit_dr0))["ace_ocs2", ]
coef(summary(fit_dr))["ace_ocs2", ]

#=======================================================================
###3.- Formal comparisson Study 3 sample vs rest
#=======================================================================
contrasts(dat0$ace_ocs) <- contrasts(dat$ace_ocs)

dat$d  <- 1
dat0$d <- 0
vars <- c(all.vars(formula(fit_dr0)), "new_w", "id", "d")
dat2 <- rbind(dat[, vars], dat0[, vars])
contrasts(dat2$ace_ocs) <- contrasts(dat$ace_ocs)
sdw2 <- svydesign(id = ~ id, weights = ~ new_w, data = dat2)


#---------------------------------------------------------------------
### a. using weights only
#---------------------------------------------------------------------
#' is the difference between "ACE + OCS" Vs. "ACE alone"
#' worse for population included in Study 3 compare with rest (Study 1)?

fit0_d <- svyglm(y ~ ace_ocs * d
                 , design = sdw2, family = quasibinomial)
coef(summary(fit0_d))["ace_ocs2", ]
round(coef(summary(fit0_d)), 4)

est <- svyby(~y, ~ ace_ocs + d
             , sdw2, svymean, na = TRUE, vartype = c("se", "ci"))
n   <- svyby(~y, ~ ace_ocs + d
             , sdw2,  unwtd.count, na = TRUE)
merge(est, n[, -ncol(n)])
merge(est, n[, -ncol(n)]) %>%
  gt %>%
  tab_header(title = "Predicted values by discrimination") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cau_pred_by_discrimination.html"))

#' @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' one sided hypothesis outcome is a good thing
#' otherwise, revert, i.e.,  <=
#' @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

K <- c(
  "ace_ocs2  <= 0" #difference among white
  , "ace_ocs2 + ace_ocs2:d <= 0" #difference among minority
  , "ace_ocs2:d <= 0" # difference in difference (minority vs. white)
)

ht_d <- glht(fit0_d, linfct = K)
summary(ht_d)
data.frame(contrasts =
             c("diff among individuals not included"
               , "diff among study 3 sample"
               , "did study 3 vs. rest")
           , summary(ht_d)$test[3:6]) %>%
  gt %>%
  tab_header(title = "Test 'ACE + OCS' vs. 'ACE only' by discrimination") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cau_test_by_discrimination.html"))


#---------------------------------------------------------------------
### b.  adding covariates to the regression ("doubly robust")
#---------------------------------------------------------------------
#' "adjusted" predictions,i.e.
#' average over all other covariate values
#' For gaussian, this does not make a difference
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

#-----------------------------------------------------------
### test
#-----------------------------------------------------------
fit_dr_d <- svyglm(y ~ ace_ocs * d
                   + female + agegrp
                   + black + white + hisp + asian_nhpi + othrace +
                      mhighgd_bin
                   + rural + mixur
                   #+ asian # no longer available (8/15/2023)
                   , design = sdw2, family = quasibinomial)
round(coef(summary(fit_dr_d)), 4)

grid <- expand.grid(ace_ocs = grp
                    , d = c(0, 1))
adj_values_tb <- adj_values(fit_dr_d, grid)
adj_values_tb
adj_values_tb %>%
  gt %>%
  tab_header(title = "Adjusted predicted values (DR) by discrimination") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cau_pred_by_discrimination_dr.html"))

K <- c(
  "ace_ocs2  <= 0" #difference among white
  , "ace_ocs2 + ace_ocs2:d <= 0" #difference among minority
  , "ace_ocs2:d <= 0" # difference in difference (minority vs. white)
)
ht_dr_d <- glht(fit_dr_d, linfct = K)
summary(ht_dr_d)
data.frame(contrasts =
             c("diff among individuals not included"
               , "diff among study 3 sample"
               , "did study 3 vs. rest")
           , summary(ht_dr_d)$test[3:6]) %>%
  gt %>%
  tab_header(title = "Doubly Robust Test 'ACE + OCS' vs. 'ACE only' by discrimination") %>%
  gtsave(paste0("./05_Regression Tree Output/03 Race updated (August)/", out ,"/cau_test_by_discrimination_dr.html"))

