rm(list=ls(all=TRUE))
library(tidyverse)
library(survey)
library(survival)
library(gt)
library(crosstable)

options(digits = 3)
options(scipen = 10^3)

i   <- 'discrim_reason'
out <- 'accinju'
setwd("C:/Users/54137/ICF/CDC ADIA Project - ICF Private Channel - ICF Private Channel/Study 1+3/Study 3/04_Analysis/") 

file_name <- paste0("./01_Data for Analysis/NLS.tree_", out, ".Rds")
dat <- readRDS(file_name)
dim(dat)
#summary(dat)

dat <-  dat %>% filter(training.sample == 0)
dim(dat)
#summary(dat)

#========================================================================
#causal accinju
#========================================================================
#========================================================================
#a.- create relevant groups
#========================================================================
# the basis for the relevant group comes from CausalTree for this outcome
# but  could come from the classical tree results for other outcomes
table(dat$node.cau)

#causal tree groups 
dat <-
  dat %>%
  mutate(ace_ocs =
           case_when(
               anyACE_T == 1 & !(loveaff == 1 | mmental == 1 | commstr == 1 | mphysab == 1 | msubstu == 1 | bneedin == 1 | ecstand == 1) ~ "ACE"
             , anyACE_T == 1 &  (loveaff == 1 | mmental == 1 | commstr == 1 | mphysab == 1 | msubstu == 1 | bneedin == 1 | ecstand == 1) ~ "ACE + OCS"
             , anyACE_T == 0 &  (loveaff == 1 | mmental == 1 | commstr == 1 | mphysab == 1 | msubstu == 1 | bneedin == 1 | ecstand == 1) ~ "OCS"
             , anyACE_T == 0 & !(loveaff == 1 | mmental == 1 | commstr == 1 | mphysab == 1 | msubstu == 1 | bneedin == 1 | ecstand == 1) ~ "None"
           )
         , ace_ocs = factor(ace_ocs 
                            , levels = c("ACE", "ACE + OCS", "None", "OCS")
                            ) # Ensure alphabetical order
         , ace_ocs = relevel(ace_ocs, ref = "None")
  )


table(dat$ace_ocs, useNA = "ifany")
levels(dat$ace_ocs) # Check if the categories are in alphabetical order except the reference group "None"

crosstable(dat, c(ace_ocs), by=mixur)

#dat[,'agegrp'] <- lapply(dat[,'agegrp'] , factor)
#crosstable(dat, c(ace_ocs), by=agegrp)

#========================================================================
### b. Find balancing weights so groups are similar
#========================================================================
#Entropy Balancing
#Reference: #https://web.stanford.edu/~jhain/Paper/eb.pdf
source("./02_R Scripts/ebw.r")

# the matrix of covariates C was already created in step 1
# the following code can be skipped if no modifications are requiered
# covariates/confounding

z <- c("female", "agegrp", "white", 
       "hisp",
       "black", 
       "asian", 
       "asian_nhpi",
       "othrace", 
       #"mhighgd_bin",
       #"rural", 
       "mixur"
)

#z <- c("female", "agegrp", "white")

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
# will not balance very rare attributes (less than 5%)
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
         #         ebw(id = id, covariates = C, target.margins = tgt, base.weight = w)
         find_weights(id = id, covariates = C, target.margins = tgt, base.weight = w)
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
  fmt_number(
    columns = everything(),
    rows = nrow(tb_r),
    decimals = 0,
    use_seps = FALSE
  ) %>%
  gtsave(paste0("./05_Regression Tree Output/", out, "/raw_weighted", out, i, ".html"))



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
table(dat$ace_ocs)

#-------------------------------------------------------------------
###c.1.-  basic fit
#-------------------------------------------------------------------
fit0   <- svyglm(y ~ ace_ocs, design = sdw, family = quasibinomial)
summary(fit0)
coef(summary(fit0))[2:4, ]

#saving the output in nice format
tests <- c("ACE Vs. Neither", "ACE + OCS Vs. ACE only", "OCS Vs. Neither")
data.frame(Contrast = tests,
           coef(summary(fit0))[2:4, ]) %>%
  gt %>%
  tab_header(title = "Test") %>%
  fmt_number(
    columns = 2:5,
    rows = everything(),
    decimals = 3,
    use_seps = FALSE
  ) %>%
  #gtsave(paste0("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/output/test0", out, i, ".html"))
  gtsave(paste0("./05_Regression Tree Output/", out, "/test0", out, i, ".html"))


pred0 <- predict(fit0, newdata = data.frame(ace_ocs = grp),type = c("response"))
cbind(pred0, confint(pred0))

freq <- data.frame(table(dat$ace_ocs)) %>% rename(Group = Var1, N = Freq)


#saving the output in nice format
left_join(freq, data.frame(Group = grp, pred0, confint(pred0)), by = "Group") %>%
  rename(Estimate = response) %>%
  gt %>%
  tab_header(title = "Predicted values") %>%
  fmt_number(
    columns = 3:6,
    rows = everything(),
    decimals = 3,
    use_seps = FALSE
  ) %>%
  gtsave(paste0("./05_Regression Tree Output/", out, "/pred0", out, i, ".html"))
  #gtsave(paste0("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/output/pred0", out, i, ".html"))


#-------------------------------------------------------------------
###c.2.- doubly-robust fit
#-------------------------------------------------------------------
fit_dr <- svyglm(y ~ ace_ocs
                 + female + agegrp
                 + black + white + hisp + asian + asian_nhpi + othrace +
                   + mhighgd_bin
                 + rural + mixur
                 #+ mhhinco no longer including covariate 02/18/2023
                 , design = sdw, family =quasibinomial)

coef(summary(fit_dr))[2:4, ]


#saving the output in nice format
data.frame(Contrast = tests,
           coef(summary(fit_dr))[2:4, ]) %>%
  gt %>%
  tab_header(title = "Test (Doubly robust)") %>%
  fmt_number(
    columns = 2:5,
    rows = everything(),
    decimals = 3,
    use_seps = FALSE
  ) %>%
  gtsave(paste0("./05_Regression Tree Output/", out, "/test_dr", out, i, ".html"))
  #gtsave(paste0("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/output/test_dr", out, i, ".html"))


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
                                        #mhhinco = mean(dat$mhhinco, na.rm = TRUE) remove income as covariate
                   ),
                                        type = c("response")
                   )

data.frame(Group = grp, pred_dr, confint(pred_dr))

#saving the output in nice format
left_join(freq, data.frame(Group = grp, pred_dr, confint(pred_dr)), by = "Group") %>%
  rename(Estimate = response) %>%
  gt %>%
  tab_header(title = "Predicted values (Doubly robust)") %>%
  fmt_number(
    columns = 3:6,
    rows = everything(),
    decimals = 3,
    use_seps = FALSE
  ) %>%
  gtsave(paste0("./05_Regression Tree Output/", out, "/pred_dr", out, i, ".html"))
  #gtsave(paste0("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/output/pred_dr", out, i, ".html"))
