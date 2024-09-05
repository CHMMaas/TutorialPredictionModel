# clear the environment
rm(list=ls())

# load libraries
library(survival)
library(rms)             # fit.mult.impute, val.prob
# remotes::install_github("CHMMaas/PredictionTools")
library(PredictionTools) # documentation: https://github.com/CHMMaas/PredictionTools
library(dcurves)

# set file.path
file.path <- "C:/Users/carol/OneDrive - Erasmus MC/Projects Tufts/Course - Predictive Models/R tutorials/"

# load data and models 
load(paste0(file.path, "Data/shrinkage.Rdata"))

# set data distributions
dd <- datadist(data)
options(datadist="dd")

###
### INTERNAL-EXTERNAL CROSS-VALIDATION
###
png(file=paste0(file.path, "Results/Internal-external cross-validation.png"),
    width=4000, height=2000, res=300)
layout(matrix(1:2, ncol=2, byrow=TRUE))
models <- paste(names(bw.model$coefficients), collapse=", ")
shrinkage.factor.regions <- c(shrinkage.factor)
for (region in unique(data$center)){
  # fit model on data excluding region j
  S.10.not.j <- S.10[data$center!=region, ]
  full.form.not.j <- update(form.full, S.10.not.j ~ .)
  full.fit.regional.not.j <- fit.mult.impute(full.form.not.j, cph,
                                             n.impute=m,
                                             fitargs=list(x=TRUE, y=TRUE, surv=TRUE),
                                             xtrans=imputed.data,
                                             data=data,
                                             sub=data$center!=region,
                                             fit.reps=TRUE)
  
  # backward selected model on data excluding region j
  vars.bw.regional.not.j <- fastbw(full.fit.regional.not.j, 
                                   rule="p", sls=0.05)$names.kept
  models <- c(models, paste(vars.bw.regional.not.j, collapse=", "))
  bw.form.not.j <- eval(parse(text=paste0("S.10.not.j ~", 
                                          paste(vars.bw.regional.not.j, 
                                                collapse="+")))) 
  bw.fit.regional.not.j <- fit.mult.impute(bw.form.not.j, cph,
                                           n.impute=m,
                                           fitargs=list(x=TRUE, y=TRUE, surv=TRUE),
                                           xtrans=imputed.data,
                                           data=data,
                                           sub=data$center!=region,
                                           fit.reps=TRUE)
  
  # calculate shrinkage factor on regions that were left out
  p.region <- c()
  shrinkage.factor.not.j <- c()
  for (i in 1:m){
    # Modelling strategy: backward selection (BW) with BIC
    # In our case p-value<0.05 gives similar model than BW using BIC
    val.model.not.j <- validate(full.fit.regional.not.j$fits[[i]], 
                                set.seed(1), pr=FALSE,
                                B=200, bw=TRUE, rule="p", sls=0.05, 
                                type="individual")
    shrinkage.factor.not.j.imp.i <- val.model.not.j["Slope", "test"]
    shrinkage.factor.not.j <- c(shrinkage.factor.not.j, shrinkage.factor.not.j.imp.i)
    
    # prediction of final model for development regions
    region.not.j <- mice::complete(imputed.data, i)[mice::complete(imputed.data, i)$center!=region, ]
    lp.not.j <- predict(bw.fit.regional.not.j$fits[[i]],
                        newdata=region.not.j,
                        type="lp")
    lp.shrunk.not.j <- shrinkage.factor.not.j.imp.i*lp.not.j
    
    # baseline hazard for development regions
    S.10.not.j <- S.10[data$center!=region, ]
    offset.lp.not.j <- survival::coxph(S.10.not.j ~ offset(lp.shrunk.not.j))
    f.basehaz.not.j <- survival::basehaz(offset.lp.not.j)
    h0.shrunk.not.j <- f.basehaz.not.j$hazard[f.basehaz.not.j$time==max(f.basehaz.not.j$time[f.basehaz.not.j$time<=horizon])]
    
    # prediction of final model for region j
    region.j <- mice::complete(imputed.data, i)[mice::complete(imputed.data, i)$center==region, ]
    lp.region.j <- shrinkage.factor.not.j.imp.i*predict(bw.fit.regional.not.j$fits[[i]],
                                                        newdata=region.j,
                                                        type="lp")
    
    # make predictions on imputation i for region j
    p.region <- cbind(p.region, 
                      PredictionTools::fun.event(lp=lp.region.j, h0=h0.shrunk.not.j))
  }
  # store shrinkage factor
  shrinkage.factor.regions <- c(shrinkage.factor.regions, mean(shrinkage.factor.not.j))
  
  # plot calibration plot for region j
  S.10.j <- S.10[data$center==region, ]
  
  # make figure
  PredictionTools::val.surv.mi(p=p.region, y=S.10.j, time=horizon, g=3,
                               CI.metrics=TRUE, main=paste("Center", region),
                               show.metrics=c(rep(TRUE, 5), FALSE))
}
dev.off()