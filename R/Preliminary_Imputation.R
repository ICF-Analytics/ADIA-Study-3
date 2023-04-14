rm(list=ls(all=TRUE))

library(tidyverse)
library(haven)
#install.packages("InformationValue")
library(caret)
library(InformationValue)

getwd()

user <- 54137
setwd(paste0("C:/Users/", user, "/ICF/CDC ADIA Project - ICF Private Channel - ICF Private Channel/Study 1+3/Study 3/Analysis/")) 
dat_all <- read_sas("./SAS Data Management Files/Data/finalvar_03232023.sas7bdat")

table(dat_all$discrim, useNA = "always")
table(dat_all$discrim_reason, useNA = "always")

# Subset sample that indicates discrimination and perceived reasons for discrimination (both 0 or 1 in reason_discrim)
dat <- dat_all %>%
  filter(discrim == 1 & is.na(discrim_reason)==FALSE)
table(dat$discrim_reason, useNA = "always")
table(dat$discrim, useNA = "always")

# Define training sample and test sample
set.seed(3333)
dat <- dat %>%
  mutate(training.sample = 
           if_else(childid %in% sample(unique(childid), round(n_distinct(childid)/4)), 1, 0)
  )
table(dat$training.sample, useNA = "always")


# Recode predictors
predictors <- c("ygender_xrnd", "black", "white", "hisp", "asian", "asian_nhpi", "othrace", "mhighgd_bin",
                #"mhhinco_adj", 
                "urbnrur", "lastgrd_bin")
for (i in predictors){
print(i)
print(table(dat[[i]], useNA = "always"))
  }
for (i in predictors){
dat[[i]] <- as.factor(ifelse(is.na(dat[[i]])==FALSE, dat[[i]], 99))
}

dat <- dat %>%
  mutate(mhhinco_na = ifelse(is.na(dat$mhhinco_adj)==TRUE, 1, 0)) %>%
  mutate(mhhinco_adj_=ifelse(is.na(dat$mhhinco_adj)==TRUE, -1, mhhinco_adj)) 

check <- dat %>%
  filter(is.na(dat$mhhinco_adj)==TRUE) %>%
  select(mhhinco_adj, mhhinco_adj_, mhhinco_na)

table(dat$mhhinco_na)
table(dat$mhhinco_adj_)
sum(is.na(dat$mhhinco_adj_)) # Check is any missing in income data

# Fit simple logistic regression model
train <- filter(dat, training.sample == 1)
test <- filter(dat, training.sample == 0)

model <- glm(discrim_reason ~ ygender_xrnd +     # child gender
                              black +            # race - Black
                              white +            # race - White
                              hisp +             # Hispanic
                              asian +            # race - Asian
                              asian_nhpi +       # race - NH/PI
                              othrace +          # race - Other
                              mhighgd_bin +      # mother completing high school
                              mhhinco_adj_ +     # household income (NA replaced with -1)
                              mhhinco_na +       # dummy indicating NA in hh income
                              urbnrur +          # urban/rural/mixed
                              lastgrd_bin,       # child not completing high school
             data = train, family = binomial(link = "logit"))
summary(model)

# Predict reasons for discrimination on test sample
test$predicted <- predict(model, test, type="response")

# Get optimal cut-off
optimal <- optimalCutoff(test$discrim_reason, test$predicted)

sum(is.na(test$predicted)) # Check is any missing in predicted values
table(test$discrim_reason)

# Assess accuracy (confusion matrix)

# Confusion Matrix
confusionMatrix(actual=test$discrim_reason, predicted=test$predicted) # use 0.5 as cut-off if no threshold is assigned
# Sensitivity
sensitivity(test$discrim_reason, test$predicted)
# Specificity
specificity(test$discrim_reason, test$predicted)
# Misclassification Error
misClassError(test$discrim_reason, test$predicted)


