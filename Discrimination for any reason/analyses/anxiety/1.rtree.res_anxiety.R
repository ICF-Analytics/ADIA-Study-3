rm(list=ls(all=TRUE))
library(devtools) 
library(tidyverse)
library(rpart)
library(partykit)
library(rpart.plot)
library(survey)
library(causalTree)  # If not working, try install_github("susanathey/causalTree") - Added 3/30/2023
library(ebal)
library(dplyr)

options(digits = 3)
options(scipen = 10^3)
options(warn = 0)
getwd()
#setwd('C:/Users/55231/OneDrive - ICF/Desktop/ADIA/analyses/anxiety') 

user <- 54137
setwd(paste0("C:/Users/", user, "/ICF/CDC ADIA Project - ICF Private Channel - ICF Private Channel/Study 1+3/Study 3/04_Analysis/")) 

#i <- 'RES'
i   <- 'discrim_reason'
out <- 'anxiety'


####### read Study dataset output from SAS ######
dat <- readRDS("./01_Data for Analysis/data_for_regTree_05302023.Rds") # 6281 observations (Study 3 only)
dim(dat)

#removing the switching of sensitivity discrim variable as we are now just using reason_discrim
#dat$DISC <- unlist(dat[, paste0("sensitivity", i)])

table(dat$DISC, useNA = "ifany")
dat$y <- unlist(dat[, out])

#to see yes/no on graphs run the code below
#dat$female <- factor(dat$female, labels=c("no", "yes"))
#dat$incarce <- factor(dat$incarce, labels=c("no", "yes"))
#dat$loveaff <- factor(dat$loveaff, labels=c("no", "yes"))
#dat$mentill <- factor(dat$mentill, labels=c("no", "yes"))
#dat$physabu <- factor(dat$physabu, labels=c("no", "yes"))
#dat$bneedin <- factor(dat$bneedin, labels=c("no", "yes"))

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
       'rural', 'mixur'
       #,
       #'mhhinco' removing income as covariate 02/18/2023
) # Ye: I replaced income with income adjusted in preprocess 01/18/2023

dat$Z <- dat[, z]
dat$X <- dat[, x]
summary(dat$Z)
summary(dat$X)

df0 <-   dat %>%   filter(training.sample == 1)

#a.- grow large tree
set.seed(0203)
tree <- with(df0, rpart(y ~ .,
                        data = cbind(y, Z, X), cp = 0, xval = 5, weights = w))
plotcp(tree, col = "red")
tree$cptable

#b.- prune based on complexity
opcp  <- tree$cptable[, "CP"][which.min(tree$cptable[, "xerror"])]
ptree <- prune(tree, cp = opcp)
rpart.plot(ptree, roundint = FALSE)

split.fun <- function(x, labs, digits, varlen, faclen)  # YK added for manual edits the splits labels (4/5/2023)
{
  labs <- gsub("mentill < 0.5", "HH members with \nmental illness = No", labs)
  labs <- gsub("mentill >= 0.5", "HH members with \nmental illness = Yes", labs)
}
prp(ptree, type = 4, # left and right split labels (see Figure 2)
    clip.right.labs = FALSE, # full right split labels
    extra = 101, # show nbr of obs and percentages (see Figure 3)
    under = TRUE, # position extra info _under_ the boxes
    under.cex = 1.2, # size of text under the boxes (default is .8)
    fallen.leaves = TRUE, # put leaves at the bottom of plot
    box.palette = "GnYlRd", # color of the boxes
    branch = .3, # branch lines with narrow shoulders and down slopes
    round = 0, # no rounding of node corners i.e. use rectangles
    leaf.round = 9, # round leaf nodes (for leaves, this supersedes the round arg)
    main = "Anxiety - Classical for Study 3", # main title
    cex.main = 1.2, # use big text for main title
    branch.col = "gray", # color of branch lines
    branch.lwd = 2, # line width of branch lines
    roundint=FALSE,
    split.fun = split.fun
) 
png(
  paste0("./05_Regression Tree Output/", out, "/tree.plot.unconditional.", i, "_Study 3.png"),
  width = 480 * 6, heigh = 480 * 4, res = 500)
#rpart.plot(ptree, roundint = FALSE)
prp(ptree, type = 4, # left and right split labels (see Figure 2)
    clip.right.labs = FALSE, # full right split labels
    extra = 101, # show nbr of obs and percentages (see Figure 3)
    under = TRUE, # position extra info _under_ the boxes
    under.cex = 1.2, # size of text under the boxes (default is .8)
    fallen.leaves = TRUE, # put leaves at the bottom of plot
    box.palette = "GnYlRd", # color of the boxes
    branch = .3, # branch lines with narrow shoulders and down slopes
    round = 0, # no rounding of node corners i.e. use rectangles
    leaf.round = 9, # round leaf nodes (for leaves, this supersedes the round arg)
    main = "Anxiety - Classical for Study 3", # main title
    cex.main = 1.2, # use big text for main title
    branch.col = "gray", # color of branch lines
    branch.lwd = 2, # line width of branch lines
    roundint=FALSE,
    split.fun = split.fun
) 
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
    rural, cut(mhhinco, 3) #since we are not running this tree, income code does not need to be edited
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
tree <- with(df0, rpart(y ~ ., 
                        data = cbind(y, W), cp = 0, xval = 5, weights = w))
plotcp(tree, col = "red")

#b.- prune based on complexity
opcp <- tree$cptable[, "CP"][which.min(tree$cptable[, "xerror"])]
ptree.cond <- prune(tree, cp = opcp)
rpart.plot(ptree.cond, roundint = FALSE)
# plot(as.party(ptree.cond))

png(
  paste0("./05_Regression Tree Output/", out, "/tree.plot.conditional.", i, "_Study 3.png"),
  width = 480 * 6, heigh = 480 * 4, res = 500)
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
source("./02_R Scripts/ebw.r")

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
ace.e <-  c('commstr', 'ecstand', 'bneedin', 'mloveaf', 
            'mphysab', 'msubstu', 'mmental',
            'loveaff')

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
#options "fit"(2) "CT"(1) "TOT"(3)
###Causal Trees (CT)
set.seed(0204)
tree_causal <- with(df0, causalTree(y ~ .,
                                    data = cbind(y, ACE.E),
                                    treatment = anyACE_T,
                                    weights = cw,
                                    split.Rule = "TOT",
                                    cv.option  = "TOT",
                                    split.Honest = FALSE,
                                    cv.Honest = FALSE
)
)
plotcp(tree_causal, col = "red")
tree_causal$cptable

opcp <- tree_causal$cptable[, 1][which.min(tree_causal$cptable[, 4])]
ptree_causal <- prune(tree_causal, cp = opcp)

#rpart.plot(ptree_causal, roundint = FALSE)
split.fun <- function(x, labs, digits, varlen, faclen)  # YK added for manual edits the splits labels (4/5/2023)
{
  labs <- gsub("loveaff < 0.5",  "Lack of parental \nlove and affection = No", labs)
  labs <- gsub("loveaff >= 0.5", "Lack of parental \nlove and affection = Yes", labs)
  labs <- gsub("mphysab < 0.5",  "Mother’s ACE- \nPhysical abuse = No", labs)
  labs <- gsub("mphysab >= 0.5", "Mother’s ACE- \nPhysical abuse = Yes", labs)
  labs <- gsub("commstr < 0.5",  "Community \nstressors = No", labs)
  labs <- gsub("commstr >= 0.5", "Community \nstressors = Yes", labs)
  labs <- gsub("msubstu < 0.5",  "Mother's ACE- HH members \nwith substance use \nproblems = No", labs)
  labs <- gsub("msubstu >= 0.5", "Mother's ACE- HH members \nwith substance use \nproblems = Yes", labs)
  labs <- gsub("bneedin < 0.5",  "Basic needs \ninstability = No", labs)
  labs <- gsub("bneedin >= 0.5", "Basic needs \ninstability = Yes", labs)
  labs <- gsub("ecstand < 0.5",  "Economic \nstanding = No", labs)
  labs <- gsub("ecstand >= 0.5", "Economic \nstanding = Yes", labs)
  labs <- gsub("mloveaf < 0.5",  "Mother’s ACE- Lack of \nparental love and affection \n= No \n ", labs)
  labs <- gsub("mloveaf >= 0.5", "Mother’s ACE- Lack of \nparental love and affection \n= Yes \n ", labs)
}
prp(ptree_causal, type = 4, # left and right split labels (see Figure 2)
    clip.right.labs = FALSE, # full right split labels
    extra = 101, # show nbr of obs and percentages (see Figure 3)
    under = TRUE, # position extra info _under_ the boxes
    under.cex = 1.0, # size of text under the boxes (default is .8)
    fallen.leaves = TRUE, # put leaves at the bottom of plot
    box.palette = "GnYlRd", # color of the boxes
    branch = .5, # branch lines with narrow shoulders and down slopes
    round = 0, # no rounding of node corners i.e. use rectangles
    leaf.round = 9, # round leaf nodes (for leaves, this supersedes the round arg)
    main = "Anxiety - Causal for Study 3", # main title
    cex.main = 1.2, # use big text for main title
    branch.col = "gray", # color of branch lines
    branch.lwd = 2, # line width of branch lines
    roundint=FALSE,
    cex = 0.7,
    split.fun = split.fun
) 
png(
  paste0("./05_Regression Tree Output/", out, "/tree.plot.causal", i, "_Study 3.png"),
  width = 480 * 5, heigh = 480 * 5, res = 500)
#rpart.plot(ptree_causal, roundint = FALSE)
prp(ptree_causal, type = 4, # left and right split labels (see Figure 2)
    clip.right.labs = FALSE, # full right split labels
    extra = 101, # show nbr of obs and percentages (see Figure 3)
    under = TRUE, # position extra info _under_ the boxes
    under.cex = 1.0, # size of text under the boxes (default is .8)
    fallen.leaves = TRUE, # put leaves at the bottom of plot
    box.palette = "GnYlRd", # color of the boxes
    branch = .5, # branch lines with narrow shoulders and down slopes
    round = 0, # no rounding of node corners i.e. use rectangles
    leaf.round = 9, # round leaf nodes (for leaves, this supersedes the round arg)
    main = "Anxiety - Causal for Study 3", # main title
    cex.main = 1.2, # use big text for main title
    branch.col = "gray", # color of branch lines
    branch.lwd = 2, # line width of branch lines
    roundint=FALSE,
    cex = 0.7,
    split.fun = split.fun
) 
dev.off()

#adding node labels
rules <- partykit:::.list.rules.party(as.party(ptree_causal))
dat$node.cau  <- factor(
  predict(partykit:::as.party(ptree_causal),
          type = "node", newdata = dat), labels = rules)
table(dat$node.cau)
table(dat$loveaff, useNA = "ifany")
dat$loveaff <- as.numeric(dat$loveaff)
table(dat$loveaff, useNA = "ifany")
dat$loveaff <- dat$loveaff - 1
table(dat$loveaff, useNA = "ifany")
#===========================================================================================
#saveRDS(dat, file = "C:/Users/55484/OneDrive - ICF/Documents/ADIA/RegressionTreeGraphics/data/NLS.tree.causal.w.Rds")
file_name <- paste0("./01_Data for Analysis/NLS.tree_", out, ".Rds")
saveRDS(dat, file = file_name)
