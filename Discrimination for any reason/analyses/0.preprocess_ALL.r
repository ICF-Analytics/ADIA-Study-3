
rm(list=ls(all=TRUE))
library(tidyverse)
library(haven)

getwd()
#setwd('C:/Users/55231/OneDrive - ICF/Desktop/ADIA') 

#(using 01.30, most recent flat data file)
user <- 54137
#dat <- read.csv('C:/Users/55231/OneDrive - ICF/Desktop/ADIA/data/finalvar_03232023.csv')
setwd(paste0("C:/Users/", user, "/ICF/CDC ADIA Project - ICF Private Channel - ICF Private Channel/Study 1+3/Study 3/04_Analysis/")) 
dat <- read_sas("./03_SAS Data Management Files/Data/finalvar_03232023.sas7bdat")
summary(dat)
summary(dat$w)
summary(dat$bneedin)
summary(dat$mloveaf)


#Subset data to Study 3 population (5/15/2023)
#dat <- subset(dat, discrim_reason==1) # This is respondents who reported any discrimination experience based on race, ethnicity, or sexual orientation
#table(dat$discrim_reason, useNA = "always")

dat <- subset(dat, discrim==1)
table(dat$discrim, useNA = "always")


### 1.- Cleaning, recoding
dat$id  <- dat$childid
dat$yob <- dat$biryear_xrnd
dat$sex <- dat$ygender_xrnd

dat$sex <- dat$ygender_xrnd

#replacing income with adjusted income
dat$mhhinco <-  dat$mhhinco_adj

#sensitivity/discrimination indicator
#dat$DISC <- dat$sensitivityRES
#changing DISC to be the raw reason for discrimination variable 02/18/2023 as sensitivity analysis was null
dat$DISC <- dat$discrim_reason
table(dat$sensitivityNOTRES, useNA = "ifany")
table(dat$sensitivityRES, useNA = "ifany")

#discrimination indicator tables
table(dat$discrim_reason, useNA = "ifany")
table(dat$sensitivityRES, useNA = "ifany")
table(dat$sensitivityNOTRES, useNA = "ifany")
table(dat$discrim_reason, dat$sensitivityRES)
table(dat$discrim_reason, dat$sensitivityNOTRES)

mean(is.na(dat$sensitivityRES))
mean(is.na(dat$sensitivityNOTRES))

dat$female <- dat$sex - 1

dat$anyrace <- as.integer(apply(
  dat[, c('white','black','asian_nhpi','asian','othrace')], 1,
  sum, na.rm = TRUE) >= 2)

dat$race[dat$white ==1] <- 2
dat$race[dat$black ==1] <- 3
dat$race[dat$asian_nhpi ==1] <- 4
dat$race[dat$asian ==1] <- 5
dat$race[dat$othrace == 1] <- 6
dat$race[dat$hisp ==1] <- 1
dat$race[dat$anyrace == 1] <- 7
#dat$race[is.na(dat$race)]<-9

table(dat$race, useNA = "always")
table(dat$asian, dat$anyrace)

##factors
dat$race <- factor(dat$race)

head(dat)
view(dat[,c('white','black','asian_nhpi','asian','othrace', 'hisp', 'race')])

###Cohort
dat$ageo <- 2018 - dat$yob
#Ye: this is cohort with anotehr name

#age grp-0-17 18-24; 25-29; 30-34; 35-39; 40-50
dat$agegrp[dat$ageo <= 17 & dat$ageo >= 0]  <- 1
dat$agegrp[dat$ageo <= 24 & dat$ageo >= 18] <- 2
dat$agegrp[dat$ageo <= 29 & dat$ageo >= 25] <- 3
dat$agegrp[dat$ageo <= 34 & dat$ageo >= 30] <- 4
dat$agegrp[dat$ageo <= 39 & dat$ageo >= 35] <- 5
dat$agegrp[dat$ageo <= 50 & dat$ageo >= 40] <- 6
table(dat$agegrp)
#view(dat[, c('ageo', 'agegrp')])

summary(dat$ageo, na.rm = T)
sd(dat$ageo, na.rm = T)

#urban rural
table(dat$urbnrur)
dat$rural <- as.numeric(dat$urbnrur == 0)
dat$mixur <- as.numeric(dat$urbnrur == 2)

#===============================================================================

#----------------------------------------------------------------------------
### any Exposure
#----------------------------------------------------------------------------
anyACE_list  <- c('commstr',  'ecstand',  'bneedin',  'mloveaf',  'mphysab',
                  'msubstu',  'mmental',  'loveaff',  
                  'incarce',  'divorce',  'physabu',  'subsuse',  'mentill'
                  #, 'DISC', 'discrim', 'discrim_reason', 'discrim_ethn','discrim_race', 'discrim_raceethn','discrim_sexualorient'
                  )

#any_ACE[!any_ACE %in% colnames(dat)]
apply(is.na(dat[, anyACE_list]), 2, mean)

dat$anyACE <- as.integer(apply(
  dat[, anyACE_list], 1, sum, na.rm = TRUE) >= 1)
table(dat$anyACE, useNA = 'ifany')

# was your intention to set anyACE to NA (instead of 0) when any individual variable is missing?
# in that case we could use this
dat$anyACE[dat$anyACE == 0 & !complete.cases(dat[, anyACE_list])] <- NA
table(dat$anyACE, useNA = 'ifany')

#----------------------------------------------------------------------------
### any traditional ACE
#----------------------------------------------------------------------------
anyACE_T_list  <- c('incarce', 'divorce',  'physabu',  'subsuse',  'mentill')
apply(is.na(dat[, anyACE_T_list]), 2, mean)

dat$anyACE_T <- as.integer(apply(
  dat[, anyACE_T_list], 1, sum, na.rm = TRUE) >= 1)
table(dat$anyACE_T, useNA = 'ifany')

dat$anyACE_T[dat$anyACE_T == 0 & !complete.cases(dat[, anyACE_T_list])] <- NA
table(dat$anyACE_T, useNA = 'ifany')

#----------------------------------------------------------------------------
#any expanded ACE
#----------------------------------------------------------------------------
anyACE_E_list  <- c('commstr',  'ecstand',  'bneedin',  'mloveaf',  'mphysab',
                    'msubstu',  'mmental',  # 'DISC',
                    'loveaff')
apply(is.na(dat[, anyACE_E_list]), 2, mean)

dat$anyACE_E <- as.integer(apply(
  dat[, anyACE_E_list], 1, sum, na.rm = TRUE) >= 1)
dat$anyACE_E[dat$anyACE_E == 0 & !complete.cases(dat[, anyACE_E_list])] <- NA
table(dat$anyACE_E, useNA = "ifany")

summary(dat)

#----------------------------------------------------------------------------
### 2.- define a training sample---------------------------------------------
#----------------------------------------------------------------------------
#simple random sample
#dat$ts  <- dat$id%in%sample(unique(dat$id), length(unique(dat$id))/2)

###stratfied random sample
#how to define strata? 
#stratifed random sample from sex, race, gender, ruralm, any traditional ace
set.seed(0203)
dat <-
  dat %>%
  mutate(strata = paste0(female,
                         race, cut(yob, 3), rural, anyACE_T)) %>%
  group_by(strata) %>%
  mutate(training.sample = 
           if_else(id %in% sample(unique(id), round(n_distinct(id) / 2)), 1, 0)
  ) %>%
  ungroup



n_distinct(dat$strata)
table(dat$strata, dat$training.sample)
table(dat$training.sample)
#ideally training sample is equal to validation sample


###01/17/23 
### Ye: by stratifying by traditional ACE
# we can run this process only once
# since it does not change with the definition of DISC

saveRDS(dat,"./01_Data for Analysis/data_for_regTree_05302023.Rds")
#saveRDS(dat,'data/finalvar_NOTRES.Rds')
#saveRDS(dat,'C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/finalvar_RES.Rds')