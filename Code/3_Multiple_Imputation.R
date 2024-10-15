# clear the environment
rm(list=ls())

# load libraries
library(survival)
library(visdat)  # visualize missing data
library(mice)    # multiple imputation

# set file.path
file.path <- "C:/Users/carol/OneDrive - Erasmus MC/Projects Tufts/Course - Predictive Models 2024/R tutorials/"

# load data with S object inside
load(paste0(file.path, "Data/data.Rdata"))

# select the outcome and candidate variables based on literature 
data <- select(data, time, status, 
         trt, age, sex, ascites, hepato, spiders, edema,
         bili, chol, albumin, copper, alk.phos, ast, trig, 
         platelet, protime, stage, center)

# binary and categorical variables should be factor variables
factor.var <- c("sex", "ascites", "hepato", "spiders", "stage", "center")
data[, factor.var] <- lapply(data[, factor.var], as.factor)

# visualize missingness
visdat::vis_miss(data)

# multiple imputation
m <- 5
imputed.data <- mice(data, m=m, seed=1, print=FALSE)

# visualize imputations
densityplot(imputed.data)

# save imputed data
save(data, m, imputed.data, S, S.10, horizon,
     file=paste0(file.path, "Data/imputed.data.Rdata"))
