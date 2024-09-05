# clear the environment
rm(list=ls())

# load libraries
library(survival)
library(rms)      # fit.mult.impute

# set file.path
file.path <- "C:/Users/carol/OneDrive - Erasmus MC/Projects Tufts/Course - Predictive Models/R tutorials/"

# load imputed data with S object
load(paste0(file.path, "Data/imputed.data.Rdata"))

# set data distributions
dd <- datadist(data)
options(datadist="dd")

# fit full model with all candidate variables on each imputed data set
# model all conitnuous variables flexible to see if that is needed
full.non.linear.model <- fit.mult.impute(S.10 ~ rcs(age, 3) + sex + ascites + hepato + 
                                spiders + edema + rcs(bili, 3) + rcs(chol, 3) + 
                                rcs(albumin, 3) + rcs(copper, 3) + 
                                rcs(alk.phos, 3) + rcs(ast, 3) + trig + 
                                rcs(platelet, 3) + rcs(protime, 3) + stage, 
                              cph,
                              fitargs=list(x=TRUE, y=TRUE, surv=TRUE),
                              xtrans=imputed.data,
                              data=data,
                              fit.reps=TRUE)
# display chi-square tests for non-linearities
anova(full.non.linear.model)
# this displays only significant non-linear effects for age and bili

# display predictor effects
# rcs(bili, 3) gives similar effect than log(bili)
# rcs(age, 3) gives similar effect to pol(age, 2)
plot(Predict(full.non.linear.model))

# fit full model with all candidate variables on each imputed data set
form.full <- S.10 ~ pol(age, 2) + sex + ascites + hepato + spiders + edema + stage
full.model <- fit.mult.impute(form.full, 
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
cox.zph(full.model)

# display predictor effects
plot(Predict(full.model))

# perform stepwise backward selection
# bw.model <- step(full.model,
#                  data=data,
#                  direction="backward",
#                  k=log(nrow(data))) # BIC criterion, default is AIC
# bw.model$formula
# this gives final model: S.10 ~ pol(age, 2) + ascites + hepato + edema

# alternatively using a fastbw function
# fastbw(full.model, type="individual", rule="p", sls=0.05)
# this gives final model: age     ascites hepato  spiders edema 

# fit backward selected model
form.bw <- S.10 ~ pol(age, 2) + ascites + hepato + edema
bw.model <- fit.mult.impute(form.bw, 
                            cph,
                            fitargs=list(x=TRUE, y=TRUE, surv=TRUE),
                            xtrans=imputed.data,
                            data=data,
                            fit.reps=TRUE)
# display predictor effects
summary(bw.model)

# display variable importance using chi-square test statistic
anova(bw.model)

# visualize predictor effects
plot(Predict(bw.model))

# nomogram
rms::nomogram(bw.model)

# save models
save(data, m, imputed.data, S, S.10, horizon,
     full.non.linear.model, form.full, full.model, form.bw, bw.model,
     file=paste0(file.path, "Data/models.Rdata"))
