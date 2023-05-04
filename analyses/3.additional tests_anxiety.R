library(tidyverse)
library(survey)
library(survival)
library(gt)

options(digits = 3)
options(scipen = 10^3)

#i <- 'RES'
i   <- 'discrim_reason'
out <- 'anxiety'

file_name <- paste0("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/data/NLS.tree", out, i, ".Rds")
dat <- readRDS(file_name)
dim(dat)

dat <-  dat %>% filter(training.sample == 0)
dim(dat)

#========================================================================
#classical anxiety
#========================================================================

#========================================================================
#a.- create relevant groups
#========================================================================
# the basis for the relevant group comes from CausalTree for this outcome
# but  could come from the classical tree results for other outcomes

#classical tree for anxiety
table(dat$physabu)
table(dat$mentill)
table(dat$bneedin)
table(dat$incarce)

table(dat$physabu)
table(dat$mentill)
table(dat$bneedin)
table(dat$commstr)

dat <-
  dat %>%
  mutate(ace_ocs =
           case_when(
             (==1 | ==1 ) ~ "ACE"
             , (==1 | ==1) &  (bneedin ==1) ~ "ACE + OCS"
             , !(==1 | ==1) &  (bneedin ==1) ~ "OCS"
             ,! (==1 | ==1) & !(bneedin ==1) ~ "None"
           )
         , ace_ocs = factor(ace_ocs)
         , ace_ocs = relevel(ace_ocs, ref = "None")
  )
table(dat$ace_ocs, useNA = "ifany")
table(dat$ace_ocs,dat$agegrp, useNA = "ifany")
#recode age as two categories because "none" and "OCS" groups do 4,5,6
#creating new group as 1,2, vs. 3, 4, 5, 6
#age grp-0-17 18-24; 25-29; 30-34; 35-39; 40-50
dat$agegrpcov[dat$agegrp ==1]  <- 1
dat$agegrpcov[dat$agegrp ==2] <- 1
dat$agegrpcov[dat$agegrp ==3] <- 2
dat$agegrpcov[dat$agegrp ==4] <- 2
dat$agegrpcov[dat$agegrp ==5] <- 2
dat$agegrpcov[dat$agegrp ==6] <- 2
table(dat$agegrpcov, useNA = "ifany")
table(dat$ace_ocs,dat$agegrpcov, useNA = "ifany")

#========================================================================
### b. Find balancing weights so groups are similar
#========================================================================
#Entropy Balancing
#Reference: #https://web.stanford.edu/~jhain/Paper/eb.pdf
source("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/R/ebw.r")

# the matrix of covariates C was already created in step 1
# the following code can be skipped if no modifications are requiered
# covariates/confounding

z <- c("female", "agegrp", "white")

#, "agegrp", "white")
#, "agegrp", 
#      "black", "white", "hisp", "asian", "asian_nhpi", "othrace",
#      "mhighgd_bin",
#      "rural", "mixur",
#      "mhhinco") with the sample sizes in the classical tree we are using our "emergency" list of covariates
# Ye: I replaced income with income adjusted in preprocess 01/18/2023

dat$Z <- dat[, z]
# In orther to balance the missing pattern we need to:
# for categorical variables, create an NA category (addNA)
# this should inlcude binary varaibles not declared as such
# for continuous, add indicator is.na and impute mean
dat$C <-
  dat$Z %>%
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
colMeans(dat$C)
#edit 2/2 upped rare attribute threshhold to 5%
# will not balance very rare attributes (les than 5%)
dat$C <- dat$C[, colMeans(dat$C) > .05]
# NA black, etc. are repetead
dat$C <- dat$C[, !colnames(dat$C) %in%
                 c("NA_whiteTRUE","NA_blackTRUE", "NA_hispTRUE", "NA_asianTRUE",
                   "NA_asian_nhpiTRUE", "NA_othraceTRUE", 
                   "NA_mixurTRUE")]
colMeans(dat$C)

# we can target any fixed population
# here just overal mean, kids with average characteristics
tgt  <- colMeans(dat$C)
cbind(
  sapply(split(dat, dat$ace_ocs), \(D) with(D, colMeans(D$C)))
  , tgt)

grp <- c("None", "ACE", "ACE + OCS", "OCS")
weights <-
  lapply(grp, \(g) {
    with(dat[!is.na(dat$ace_ocs) & dat$ace_ocs == g, ],
         ebw(id = id, covariates = C, target.margins = tgt, base.weight = w)
    )
  }) %>%
  bind_rows

colnames(weights)[2] <- "new_w"
dat  <- left_join(dat, weights, by = "id")
dat$new_w[is.na(dat$new_w)] <- 0


tb_r <-
  with(dat, data.frame(C, w, new_w, ace_ocs)) %>%
  subset(!is.na(ace_ocs)) %>%
  group_by(grp = ace_ocs) %>%
  summarize(across(all_of(colnames(dat$C))
                   , ~ weighted.mean(., w)), n = n()) %>%
  pivot_longer(!grp, names_to = "var", values_to = "freq") %>%
  pivot_wider(names_from = "grp", values_from = "freq")

tb_w <-
  with(dat, data.frame(C, w, new_w, ace_ocs)) %>%
  subset(!is.na(ace_ocs)) %>%
  group_by(grp = ace_ocs) %>%
  summarize(across(all_of(colnames(dat$C))
                   , ~ weighted.mean(., new_w)), n = n()) %>%
  pivot_longer(!grp, names_to = "var", values_to = "freq") %>%
  pivot_wider(names_from = "grp", values_from = "freq")

left_join(tb_r, tb_w, by = "var") %>% data.frame()

#saving the output in nice format
left_join(tb_r, tb_w, by = "var", suffix = c(".raw", ".weighted")) %>%
  gt %>%
  tab_header(title = "Before and after weighting") %>%
  gtsave(paste0("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/output/raw_weighted", out, i, ".html"))


#========================================================================
### c.- Run analysis
#========================================================================
### custom contrasts
#contrasts(dat$ace_ocs) <- contr.treatment(4)
contrasts(dat$ace_ocs)
mat <- matrix(ncol = 3, byrow = TRUE, data = c(
  0,  0, 0,
  1, -1, 0,
  0,  1, 0,
  0,  0, 1)
)
contrasts(dat$ace_ocs) <- MASS::ginv(t(mat))
# 3 contrast:
# 1. ACE vs. none
# 2. ACE + OCE vs. ACE
# 3. OCS vs. none

###declare design
sdw    <- svydesign(id = ~ id, weights = ~ new_w, data = dat)

#-------------------------------------------------------------------
###c.1.-  basic fit
#-------------------------------------------------------------------
fit0   <- svyglm(y ~ ace_ocs, design = sdw, family = gaussian)
coef(summary(fit0))[2:4, ]

#saving the output in nice format
tests <- c("ACE Vs. Neither", "ACE + OCS Vs. ACE only", "OCS Vs. Neither")
data.frame(Contrast = tests,
           coef(summary(fit0))[2:4, ]) %>%
  gt %>%
  tab_header(title = "Test") %>%
  gtsave(paste0("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/output/test0", out, i, ".html"))


pred0 <- predict(fit0, newdata = data.frame(ace_ocs = grp))
cbind(pred0, confint(pred0))

#saving the output in nice format
data.frame(Group = grp, pred0, confint(pred0)) %>%
  gt %>%
  tab_header(title = "Predicted values") %>%
  gtsave(paste0("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/output/pred0", out, i, ".html"))


#-------------------------------------------------------------------
###c.2.- doubly-robust fit
#-------------------------------------------------------------------
fit_dr <- svyglm(y ~ ace_ocs
                 + female + agegrpcov
                 + black + white + hisp + asian + asian_nhpi + othrace +
                   + mhighgd_bin
                 + rural + mixur
                 #+ mhhinco removing income as covariate 02/18/2023
                 , design = sdw)

coef(summary(fit_dr))[2:4, ]


#saving the output in nice format
data.frame(Contrast = tests,
           coef(summary(fit_dr))[2:4, ]) %>%
  gt %>%
  tab_header(title = "Test (Doubly robust)") %>%
  gtsave(paste0("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/output/test_dr", out, i, ".html"))


pred_dr <- predict(fit_dr,
                   newdata = data.frame(ace_ocs = grp,
                                        female = 1,
                                        agegrpcov = mean(dat$agegrp, na.rm = TRUE),
                                        black = 0,
                                        white = 0,
                                        hisp = 1,
                                        asian = 0,
                                        asian_nhpi = 0,
                                        othrace = 0,
                                        mhighgd_bin = 0,
                                        rural = 0,
                                        mixur = 0
                                        #,
                                        #mhhinco = mean(dat$mhhinco, na.rm = TRUE) removing income as covariate
                   )
)

data.frame(Group = grp, pred_dr, confint(pred_dr))

#saving the output in nice format
data.frame(Group = grp, pred_dr, confint(pred_dr)) %>%
  gt %>%
  tab_header(title = "Predicted values (Doubly robust)") %>%
  gtsave(paste0("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/output/pred_dr", out, i, ".html"))

#========================================================================
#causal anxiety
#========================================================================
#========================================================================
#a.- create relevant groups
#========================================================================

# the basis for the relevant group comes from CausalTree for this outcome
# but  could come from the classical tree results for other outcomes
#causal tree groups 
dat <-
  dat %>%
  mutate(ace_ocs =
           case_when(
             anyACE_T == 1 & !(loveaff == 1) ~ "ACE"
             , anyACE_T == 1 &  (loveaff == 1) ~ "ACE + OCS"
             , anyACE_T == 0 &  (loveaff == 1) ~ "OCS"
             , anyACE_T == 0 & !(loveaff == 1) ~ "None"
           )
         , ace_ocs = factor(ace_ocs)
         , ace_ocs = relevel(ace_ocs, ref = "None")
  )

table(dat$ace_ocs, useNA = "ifany")

#========================================================================
### b. Find balancing weights so groups are similar
#========================================================================
#Entropy Balancing
#Reference: #https://web.stanford.edu/~jhain/Paper/eb.pdf
source("R/ebw.r")

# the matrix of covariates C was already created in step 1
# the following code can be skipped if no modifications are requiered
# covariates/confounding
z <- c("female", "agegrp", "white", "hisp", "black", "asian", "asian_nhpi", "othrace", "mhighgd_bin",
       "rural", "mixur"
       #,
       #"mhhinco"
)

# Ye: I replaced income with income adjusted in preprocess 01/18/2023
dat$Z <- dat[, z]
# In orther to balance the missing pattern we need to:
# for categorical variables, create an NA category (addNA)
# this should inlcude binary varaibles not declared as such
# for continuous, add indicator is.na and impute mean
dat$C <-
  dat$Z %>%
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
colMeans(dat$C)
#edit 2/2 upped rare attribute threshhold to 5%
# will not balance very rare attributes (les than 5%)
dat$C <- dat$C[, colMeans(dat$C) > .05]
# NA black, etc. are repetead
dat$C <- dat$C[, !colnames(dat$C) %in%
                 c("NA_whiteTRUE","NA_blackTRUE", "NA_hispTRUE", "NA_asianTRUE",
                   "NA_asian_nhpiTRUE", "NA_othraceTRUE", 
                   "NA_mixurTRUE")]
colMeans(dat$C)


# we can target any fixed population
# here just overal mean, kids with average characteristics
tgt  <- colMeans(dat$C)
cbind(
  sapply(split(dat, dat$ace_ocs), \(D) with(D, colMeans(D$C)))
  , tgt)

grp <- c("None", "ACE", "ACE + OCS", "OCS")
weights <-
  lapply(grp, \(g) {
    with(dat[!is.na(dat$ace_ocs) & dat$ace_ocs == g, ],
         ebw(id = id, covariates = C, target.margins = tgt, base.weight = w)
    )
  }) %>%
  bind_rows

colnames(weights)[2] <- "new_w"
dat  <- left_join(dat, weights, by = "id")
dat$new_w[is.na(dat$new_w)] <- 0


tb_r <-
  with(dat, data.frame(C, w, new_w, ace_ocs)) %>%
  subset(!is.na(ace_ocs)) %>%
  group_by(grp = ace_ocs) %>%
  summarize(across(all_of(colnames(dat$C))
                   , ~ weighted.mean(., w)), n = n()) %>%
  pivot_longer(!grp, names_to = "var", values_to = "freq") %>%
  pivot_wider(names_from = "grp", values_from = "freq")

tb_w <-
  with(dat, data.frame(C, w, new_w, ace_ocs)) %>%
  subset(!is.na(ace_ocs)) %>%
  group_by(grp = ace_ocs) %>%
  summarize(across(all_of(colnames(dat$C))
                   , ~ weighted.mean(., new_w)), n = n()) %>%
  pivot_longer(!grp, names_to = "var", values_to = "freq") %>%
  pivot_wider(names_from = "grp", values_from = "freq")

left_join(tb_r, tb_w, by = "var") %>% data.frame()

#saving the output in nice format
left_join(tb_r, tb_w, by = "var", suffix = c(".raw", ".weighted")) %>%
  gt %>%
  tab_header(title = "Before and after weighting") %>%
  gtsave(paste0("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/output/raw_weighted", out, i, ".html"))



#========================================================================
### c.- Run analysis
#========================================================================
### custom contrasts
#contrasts(dat$ace_ocs) <- contr.treatment(4)
contrasts(dat$ace_ocs)
mat <- matrix(ncol = 3, byrow = TRUE, data = c(
  0,  0, 0,
  1, -1, 0,
  0,  1, 0,
  0,  0, 1)
)
contrasts(dat$ace_ocs) <- MASS::ginv(t(mat))
# 3 contrast:
# 1. ACE vs. none
# 2. ACE + OCE vs. ACE
# 3. OCS vs. none

###declare design
sdw    <- svydesign(id = ~ id, weights = ~ new_w, data = dat)

#-------------------------------------------------------------------
###c.1.-  basic fit
#-------------------------------------------------------------------
fit0   <- svyglm(y ~ ace_ocs, design = sdw, family = gaussian)
coef(summary(fit0))[2:4, ]

#saving the output in nice format
tests <- c("ACE Vs. Neither", "ACE + OCS Vs. ACE only", "OCS Vs. Neither")
data.frame(Contrast = tests,
           coef(summary(fit0))[2:4, ]) %>%
  gt %>%
  tab_header(title = "Test") %>%
  gtsave(paste0("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/output/test0", out, i, ".html"))


pred0 <- predict(fit0, newdata = data.frame(ace_ocs = grp))
cbind(pred0, confint(pred0))

#saving the output in nice format
data.frame(Group = grp, pred0, confint(pred0)) %>%
  gt %>%
  tab_header(title = "Predicted values") %>%
  gtsave(paste0("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/output/pred0", out, i, ".html"))


#-------------------------------------------------------------------
###c.2.- doubly-robust fit
#-------------------------------------------------------------------
fit_dr <- svyglm(y ~ ace_ocs
                 + female + agegrp
                 + black + white + hisp + asian + asian_nhpi + othrace +
                   + mhighgd_bin
                 + rural + mixur
                 #+ mhhinco removing income as covariate
                 , design = sdw)

coef(summary(fit_dr))[2:4, ]


#saving the output in nice format
data.frame(Contrast = tests,
           coef(summary(fit_dr))[2:4, ]) %>%
  gt %>%
  tab_header(title = "Test (Doubly robust)") %>%
  gtsave(paste0("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/output/test_dr", out, i, ".html"))


pred_dr <- predict(fit_dr,
                   newdata = data.frame(ace_ocs = grp,
                                        female = 1,
                                        agegrp = mean(dat$agegrp, na.rm = TRUE),
                                        black = 0,
                                        white = 0,
                                        hisp = 1,
                                        asian = 0,
                                        asian_nhpi = 0,
                                        othrace = 0,
                                        mhighgd_bin = 0,
                                        rural = 0,
                                        mixur = 0
                                        #,
                                        #mhhinco = mean(dat$mhhinco, na.rm = TRUE) removing income as covariate
                   )
)

data.frame(Group = grp, pred_dr, confint(pred_dr))

#saving the output in nice format
data.frame(Group = grp, pred_dr, confint(pred_dr)) %>%
  gt %>%
  tab_header(title = "Predicted values (Doubly robust)") %>%
  gtsave(paste0("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/output/pred_dr", out, i, ".html"))