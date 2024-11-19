# clear the environment
rm(list=ls())

# load libraries
# library(survival)
library(rms)
# # remotes::install_github("CHMMaas/PredictionTools")
# library(PredictionTools) # documentation: https://github.com/CHMMaas/PredictionTools
# library(dcurves)

# set file.path
file.path <- "C:/Users/carol/OneDrive - Erasmus MC/Projects Tufts/Course - Predictive Models 2024/R tutorials/"

# load data and models 
load(paste0(file.path, "Data/shrinkage.Rdata"))

# set data distributions
dd <- rms::datadist(data)
options(datadist="dd")

# make calibration plots with performance metrics
model.df <- data.frame(model.short=c("full", "bw", "shrunk"),
                       model.title=c("Full model", 
                                     "Final model without shrinkage", 
                                     "Shrunk model"))
for (model in model.df$model.short){
  cat("Now making plot for", model, "model \n")
  
  # load full model or backward selected model
  if (model != "shrunk"){
    fitted.model <- eval(parse(text=paste0(model, ".model")))
    form.model <- eval(parse(text=paste0("form.", model)))
  }
  
  p <- c()
  h0 <- c()
  for (i in 1:m){
    if (model != "shrunk"){
      # linear predictor
      lp.i <- predict(fitted.model$fits[[i]],
                      newdata=mice::complete(imputed.data,i),
                      type="lp")
      
      # baseline hazard
      f.basehaz.i <- survival::basehaz(rms::cph(form.model, data=mice::complete(imputed.data, i), 
                                 x=TRUE, y=TRUE, se.fit=TRUE))
    } else if (model=="shrunk"){
      # linear predictor
      lp.i <- predict(bw.model$fits[[i]],
                      newdata=mice::complete(imputed.data,i),
                      type="lp")
      lp.shrunk.i <- shrinkage.factor*lp.i
      
      # baseline hazard
      offset.lp.i <- survival::coxph(S.10 ~ offset(lp.shrunk.i), 
                           data=mice::complete(imputed.data,i))
      f.basehaz.i <- survival::basehaz(offset.lp.i)
    }
    # obtain baseline hazard
    h0.i <- f.basehaz.i$hazard[f.basehaz.i$time==max(f.basehaz.i$time[f.basehaz.i$time<=horizon])]
    h0 <- c(h0, h0.i)
    
    # obtain predictions
    p <- cbind(p, PredictionTools::fun.event(lp=lp.i, h0=h0.i))
  }
  # save predictions
  p <- as.matrix(p)
  assign(paste0("p.", model), p)
  
  # to make calibration plots: replace predictions ==1 to 0.9999 because log(0)=-Inf
  p[p==1] <- 0.99999
  
  # calibration plots
  png(filename=paste0(file.path, "Results/", model, ".model.performance.png"),
      width=2000, height=2000, res=300)
  PredictionTools::val.surv.mi(p=p, y=S.10, time=horizon, g=3,
                               main=model.df[model.df$model.short==model, "model.title"], 
                               CI.metrics=TRUE, 
                               show.metrics=c(rep(TRUE, 5), FALSE))
  dev.off()
  
  assign(paste0("p.", model), p)
}
# check if models predict well on average
1-summary(survival::survfit(S.10~1, data=data), time=horizon)$surv
mean(rowMeans(p.full))
mean(rowMeans(p.bw))
mean(rowMeans(p.shrunk))

# decision curve analysis
ggplot2::ggsave(file=paste0(file.path, "Results/dca.png"),
       plot=dcurves::dca(y ~ Model, 
                time=horizon,
                data=data.frame(y=S.10,
                                Model=rowMeans(p.bw))) |> 
         plot(smooth = TRUE),
       width=5, height=5, dpi=300)
