
library(devtools) 
library(tidyverse)
library(rpart)
library(partykit)
library(rpart.plot)
library(survey)
library(causalTree)
library(ebal)

options(digits = 3)
options(scipen = 10^3)
options(warn = 0)

getwd()
setwd('C:/Users/55231/OneDrive - ICF/Desktop/ADIA/analyses/evrvicr') 

i   <- 'discrim_reason'
out <- 'evrvicr'

org_dat <- readRDS("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/data/finalvar_03232023.Rds")
dim(org_dat)

############################################################################################## added by KJ Liao for Study 3, 3/27/2023
####### subset observation with etinicity/race/discrim_sexualorient discrimination############ 
##############################################################################################

dat <- subset(org_dat, discrim_ethn==1 | discrim_race==1 | discrim_raceethn==1 | discrim_sexualorient==1)
#dat <- org_dat
dim(dat) # 841 observations without missing discrim variables, 7946 observations including missing values 
#check<-dat[,c('discrim_ethn','discrim_race','discrim_raceethn','discrim_sexualorient')]
#print(check, n=100)

table(dat$DISC, useNA = "ifany")
dat$y <- unlist(dat[, out])

#===============================================================================
### 1.- 'Classical' regression tree (expousure & covariates are all lump together as predictors)
#===============================================================================
summary(dat)

### 0.- variables & roles
# exposures
x <- c('commstr', 'ecstand', 'bneedin', 'mloveaf', 'mphysab', 'msubstu', 'mmental',
       'loveaff', 'incarce', 'divorce', 'physabu', 'subsuse', 'mentill')

# covariates/confounding
z <- c('female', 'agegrp', 'black', 'white', 'hisp', 'asian', 'asian_nhpi', 'othrace', 'mhighgd_bin',
       'rural', 'mixur')

dat$Z <- dat[, z]
dat$X <- dat[, x]
summary(dat$Z)
summary(dat$X)

df0 <-   dat %>%   filter(training.sample == 1)

#a.- grow large tree
set.seed(0203)
tree <- with(df0, rpart(y ~ ., data = cbind(y, Z, X), cp = 0, xval = 5, weights = w, method = 'anova'))
plotcp(tree, col = "red")

#b.- prune based on complexity
opcp  <- tree$cptable[, "CP"][which.min(tree$cptable[, "xerror"])]
ptree <- prune(tree, cp = opcp)
#rpart.plot(ptree, roundint = FALSE)
prp(ptree, type = 4, # left and right split labels (see Figure 2)
    clip.right.labs = FALSE, # full right split labels
    extra = 101, # show nbr of obs and percentages (see Figure 3)
    under = TRUE, # position extra info _under_ the boxes
    under.cex = 1, # size of text under the boxes (default is .8)
    fallen.leaves = TRUE, # put leaves at the bottom of plot
    box.palette = "GnYlRd", # color of the boxes
    branch = .3, # branch lines with narrow shoulders and down slopes
    round = 0, # no rounding of node corners i.e. use rectangles
    leaf.round = 9, # round leaf nodes (for leaves, this supersedes the round arg)
    #prefix = "Depression Score\n", # prepend this string to the node labels
    main = "Violent Crime Victimization - Classical for Study 3", # main title
    cex.main = 1.5, # use big text for main title
    branch.col = "gray", # color of branch lines
    branch.lwd = 2, # line width of branch lines
    roundint=FALSE) 


png(
  paste0("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/output/", out, "/tree.plot.unconditional.", i, "_Study 3.png"),
  width = 480 * 4, heigh = 480 * 4, res = 300)
#rpart.plot(ptree, roundint = FALSE)
prp(ptree, type = 4, # left and right split labels (see Figure 2)
    clip.right.labs = FALSE, # full right split labels
    extra = 101, # show nbr of obs and percentages (see Figure 3)
    under = TRUE, # position extra info _under_ the boxes
    under.cex = 1, # size of text under the boxes (default is .8)
    fallen.leaves = TRUE, # put leaves at the bottom of plot
    box.palette = "GnYlRd", # color of the boxes
    branch = .3, # branch lines with narrow shoulders and down slopes
    round = 0, # no rounding of node corners i.e. use rectangles
    leaf.round = 9, # round leaf nodes (for leaves, this supersedes the round arg)
    #prefix = "Depression Score\n", # prepend this string to the node labels
    main = "Violent Crime Victimization - Classical for Study 3", # main title
    cex.main = 1.5, # use big text for main title
    branch.col = "gray", # color of branch lines
    branch.lwd = 2, # line width of branch lines
    roundint=FALSE) 
dev.off()
#adding node labels
rules <- partykit:::.list.rules.party(as.party(ptree))
dat$node.cls  <- factor(predict(as.party(ptree), type = "node", newdata = dat), labels = rules)
table(dat$node.cls)

#saveRDS(dat, file = paste("data/NLS.tree.unconditional.w.notres.Rds")

#===============================================================================
### 2.- Regression tree 'conditional' on covariates
#===============================================================================
# expousures and covaraites are treated differently
# we explore expousures after adjusting for covariates
#Reference:  Stanfill et al. (2019) https://doi.org/10.1177/1179597219858954

#Ye: The procedure can only adjust by a simplified version of Z
#e.g., 3 categories of income
# include only main covariates!

dat$strata <- NULL
dat <-
  dat %>%
  mutate(strata = paste0(
    female, white, cut(yob, 3),
    rural, cut(mhhinco, 3) #not using conditional tree so no need to comment out income 
  )) %>%
  group_by(strata) %>%
  mutate(n = n(), strata = if_else(n > 10, strata, "misc"))
table(dat$strata)

dat$W <-
  dat %>% 
  group_by(strata) %>%
  summarize(across(c(all_of(x)), ~ .x - weighted.mean(.x, w, na.rm = TRUE))) %>%
  ungroup %>%
  select(-1)

df0 <- dat %>% filter(training.sample == 1)

#a.- grow large tree
tree <- with(df0, rpart(y ~ ., data = cbind(y, W), cp = 0, xval = 5, weights = w))
plotcp(tree, col = "red")

#b.- prune based on complexity
opcp <- tree$cptable[, "CP"][which.min(tree$cptable[, "xerror"])]
ptree.cond <- prune(tree, cp = opcp)
rpart.plot(ptree.cond, roundint = FALSE)
# plot(as.party(ptree.cond))

png(
  paste0("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/output/", out, "/tree.plot.conditional", i, "_Study 3.png"),
  width = 480 * 4, heigh = 480 * 4, res = 300)
rpart.plot(ptree.cond, roundint = FALSE)
dev.off()

dat$node.cnd <- factor(
  predict(as.party(ptree.cond),
          type = "node", newdata = dat)
)
table(dat$node.cnd)

#===============================================================================
### 3.- A casual tree
#===============================================================================
### 01/17/23 Causal tree needs complete info on ACE_T
#one solution is to collpase certainly unexposed with expousure unknown
dat$anyACE_T[is.na(dat$anyACE_T)] <- 0
table(dat$anyACE_T)
#an alternative solution would be imputation
#anyACE_T_list  <- c('incarce', 'divorce',  'physabu',  'subsuse',  'mentill')
#table(dat$anyACE_T, apply(is.na(dat[, anyACE_T_list]), 1, sum))
#for example 0 if only 1 out of 5 missing

### a. Find balancing weights among those with and without any ACEs
#Entropy Balancing
#Reference: #https://web.stanford.edu/~jhain/Paper/eb.pdf
source("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/R/ebw.r")

# In orther to balance the missing pattern we need to:
# for categorical variables, create an NA category (addNA)
# this should inlcude binary varaibles not declared as such
# for continuous, add indicator is.na and impute mean
dat$C <-
  dat$Z %>%
  mutate(across(where(is.factor), addNA, ifany = TRUE))  %>%
  mutate(across(where(~ is.numeric(.x) && any(is.na(.x)) && n_distinct(.x)==2), ~ addNA(factor(.x)))) %>%
  mutate(across(where(~ is.numeric(.x) && any(is.na(.x))), is.na,.names = 'NA_{.col}')) %>%
  mutate(across(where(is.numeric),~ replace(.,is.na(.),mean(.,na.rm=T))))  %>%
  model.matrix(~.,.) %>%
  .[, -1]
colMeans(dat$C)
# will not balance very rare attributes (les than 1%)
dat$C <- dat$C[, colMeans(dat$C) > .01]
# NA black, etc. are repetead
dat$C <- dat$C[, !colnames(dat$C) %in%
                 c("NA_whiteTRUE", "NA_hispTRUE", "NA_asianTRUE",
                   "NA_asian_nhpiTRUE", "NA_othraceTRUE", 
                   "NA_mixurTRUE")]
colMeans(dat$C)


tgt  <- colMeans(dat$C); tgt
sapply(split(dat, dat$anyACE_T), \(D) with(D, colMeans(D$C)))

ebw1 <- with(dat[dat$anyACE_T == 1, ],
             ebw(id = id,
                 covariates = C,
                 target.margins = tgt,
                 base.weight = w)
)
ebw0 <- with(dat[dat$anyACE_T == 0, ],
             ebw(id = id,
                 covariates = C,
                 target.margins = tgt,
                 base.weight = w)
)

dat$wb <- NULL
dat  <- left_join(dat, rbind(ebw0, ebw1), by = "id")

summary(with(dat, data.frame(C, anyACE_T, w, wb)))


with(dat, data.frame(C, w, wb)) %>%
  group_by(dat$anyACE_T) %>%
  summarize(across(all_of(colnames(dat$C)), list(
    ~ weighted.mean(., w),
    ~ weighted.mean(., wb))
  ))

###expanded ACE
ace.e <-  c('commstr', 'ecstand', 'bneedin', 'mloveaf','DISC', 
            'mphysab', 'msubstu', 'mmental','loveaff')

summary(dat[, ace.e])
dat$ACE.E <- dat[, ace.e]

###case weights
#dat$cw[dat$anyACE_T == 1] <- dat$wb * sum(dat$anyACE_T)
#dat$cw[dat$anyACE_T == 0] <- dat$wb * sum(!dat$anyACE_T)
#tapply(dat$wb, dat$anyACE_T, sum)
#tapply(dat$cw, dat$anyACE_T, sum)

#updated weight code from Lucas 1/24/2023
dat$cw <- NA
dat$cw[dat$anyACE_T == 1] <- dat$wb[dat$anyACE_T == 1] * sum(dat$anyACE_T)
dat$cw[dat$anyACE_T == 0] <- dat$wb[dat$anyACE_T == 0] * sum(!dat$anyACE_T)
tapply(dat$wb, dat$anyACE_T, sum)
tapply(dat$cw, dat$anyACE_T, sum)

# replace normalized weights (that add up to 1 in each arm)
# with weigths that add up to sample size in eqach arm
# For rtree the scale of the  weights is of no consequence
# casual tree uses sample size of each arm
# in the computation of criteria for splitting


### . find effect modifiers
#Reference https://doi.org/10.48550/arXiv.1504.01132
df0 <-  dat %>%  filter(training.sample == 1)

###Causal Trees (CT)
#followed protocol to first try "CT" then "fit" then "TOT"
set.seed(0203)
tree_causal <- with(df0, causalTree(y ~ .,
                                    data = cbind(y, ACE.E),
                                    treatment = anyACE_T,
                                    weights = cw,
                                    split.Rule = "fit",
                                    cv.option  = "fit",
                                    split.Honest = FALSE,
                                    cv.Honest = FALSE
)
)
plotcp(tree_causal, col = "red")
tree_causal$cptable

opcp <- tree_causal$cptable[, 1][which.min(tree_causal$cptable[, 4])]
ptree_causal <- prune(tree_causal, cp = opcp)
#rpart.plot(ptree_causal, roundint = FALSE)
prp(ptree_causal, type = 4, # left and right split labels (see Figure 2)
    clip.right.labs = FALSE, # full right split labels
    extra = 101, # show nbr of obs and percentages (see Figure 3)
    under = TRUE, # position extra info _under_ the boxes
    under.cex = 1, # size of text under the boxes (default is .8)
    fallen.leaves = TRUE, # put leaves at the bottom of plot
    box.palette = "GnYlRd", # color of the boxes
    branch = .3, # branch lines with narrow shoulders and down slopes
    round = 0, # no rounding of node corners i.e. use rectangles
    leaf.round = 9, # round leaf nodes (for leaves, this supersedes the round arg)
    #prefix = "Depression Score\n", # prepend this string to the node labels
    main = "Violent Crime Victimization - Causal for Study 3", # main title
    cex.main = 1.5, # use big text for main title
    branch.col = "gray", # color of branch lines
    branch.lwd = 2, # line width of branch lines
    roundint=FALSE) 

png(
  paste0("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/output/", out, "/tree.plot.causal", i, "_Study 3.png"),
  width = 480 * 4, heigh = 480 * 4, res = 300)
rpart.plot(ptree_causal, roundint = FALSE)
prp(ptree_causal, type = 4, # left and right split labels (see Figure 2)
    clip.right.labs = FALSE, # full right split labels
    extra = 101, # show nbr of obs and percentages (see Figure 3)
    under = TRUE, # position extra info _under_ the boxes
    under.cex = 1, # size of text under the boxes (default is .8)
    fallen.leaves = TRUE, # put leaves at the bottom of plot
    box.palette = "GnYlRd", # color of the boxes
    branch = .3, # branch lines with narrow shoulders and down slopes
    round = 0, # no rounding of node corners i.e. use rectangles
    leaf.round = 9, # round leaf nodes (for leaves, this supersedes the round arg)
    #prefix = "Depression Score\n", # prepend this string to the node labels
    main = "Violent Crime Victimization - Causal for Study 3", # main title
    cex.main = 1.5, # use big text for main title
    branch.col = "gray", # color of branch lines
    branch.lwd = 2, # line width of branch lines
    roundint=FALSE) 
dev.off()

#adding node labels
rules <- partykit:::.list.rules.party(as.party(ptree_causal))
dat$node.cau  <- factor(
  predict(partykit:::as.party(ptree_causal),
          type = "node", newdata = dat), labels = rules)
table(dat$node.cau)

#===========================================================================================

file_name <- paste0("C:/Users/55231/OneDrive - ICF/Desktop/ADIA/data/NLS.tree", out, i, ".Rds")
saveRDS(dat, file = file_name)