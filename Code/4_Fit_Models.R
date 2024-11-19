# clear the environment
rm(list=ls())

# load libraries
library(survival)
library(rms)          # cph or lrm
library(Hmisc)      # fit.mult.impute

# set file.path
file.path <- "C:/Users/carol/OneDrive - Erasmus MC/Projects Tufts/Course - Predictive Models 2024/R tutorials/"

# load imputed data with S object
load(paste0(file.path, "Data/imputed.data.Rdata"))

# set data distributions
dd <- rms::datadist(data)
options(datadist="dd")

# fit full model with all candidate variables on each imputed data set
# model all conitnuous variables flexible to see if that is needed
full.non.linear.model <- Hmisc::fit.mult.impute(S.10 ~ rcs(age, 3) + sex + 
                                                  ascites + hepato + stage, 
                              cph,
                              fitargs=list(x=TRUE, y=TRUE, surv=TRUE),
                              xtrans=imputed.data,
                              data=data,
                              fit.reps=TRUE)
summary(full.non.linear.model)
# display chi-square tests for non-linearities
anova(full.non.linear.model)
# NOte: doesn't display significant non-linear effects for age, 
# but I'll illustrate that pol(age, 2) gives similar pattern

# display predictor effects
# rcs(age, 3) gives similar effect as pol(age, 2)
plot(Predict(full.non.linear.model))

# fit full model with all candidate variables on each imputed data set
form.full <- S.10 ~ pol(age, 2) + sex + ascites + hepato + stage
full.model <- Hmisc::fit.mult.impute(form.full, 
                              cph,
                              fitargs=list(x=TRUE, y=TRUE, surv=TRUE),
                              xtrans=imputed.data,
                              data=data,
                              fit.reps=TRUE)
# display predictor effects
summary(full.model)

# display chi-square tests for variable importance
anova(full.model)

# test proportional hazards assumption
survival::cox.zph(full.model)

# display predictor effects
plot(Predict(full.model))

# perform stepwise backward selection
# use k=log(nrow(data)) for BIC criterion, default is AIC
bw.model <- step(full.model,
                 direction="backward")
bw.model$formula
# this gives final model: S.10 ~ pol(age, 2) + ascites + hepato + stage

# alternatively using a fastbw function
fastbw(full.model, type="individual", rule="p", sls=0.05)
# this gives final model: age     ascites hepato  stage  

# fit backward selected model
form.bw <- S.10 ~ pol(age, 2) + ascites + hepato + stage
bw.model <- Hmisc::fit.mult.impute(form.bw, 
                            cph,
                            fitargs=list(x=TRUE, y=TRUE, surv=TRUE),
                            xtrans=imputed.data,
                            data=data,
                            fit.reps=TRUE)
# display predictor effects
summary(bw.model)

# present effect for age per decade
summary(bw.model, age=c(0, 10))

# display variable importance using chi-square test statistic
anova(bw.model)

# visualize predictor effects
plot(Predict(bw.model))

# nomogram
plot(rms::nomogram(bw.model, 
                   age=c(25, 50, 75))) # optional: specify the labels of variables

# save models
save(data, m, imputed.data, S, S.10, horizon,
     full.non.linear.model, form.full, full.model, form.bw, bw.model,
     file=paste0(file.path, "Data/models.Rdata"))