# clear the environment
rm(list=ls())

# load libraries
library(survival)
library(rms)      # validate

# set file.path
file.path <- "C:/Users/carol/OneDrive - Erasmus MC/Projects Tufts/Course - Predictive Models 2024/R tutorials/"

# load data and models 
load(paste0(file.path, "Data/models.Rdata"))

# calculate uniform shrinkage factor across all imputations
slope <- c()
opt.C <- c()
for (i in 1:m){
  val.model <- rms::validate(full.model$fits[[i]], 
                        set.seed(1),
                        B=200, 
                        bw=TRUE,
                        rule="p",
                        sls=0.05,
                        type="individual")
  slope <- c(slope, val.model["Slope", "test"])
  opt.C <- c(opt.C, val.model["Dxy", "optimism"]/2)
}
# shrinkage factor
shrinkage.factor <- mean(slope)
cat("Shrinkage factor:", shrinkage.factor, "\n")

# shrunk coefficients
shrinkage.factor*bw.model$coefficients

# optimism-corrected C-index
optimism.C <- mean(opt.C)
original.C <- survival::concordance(bw.model)
cat("Optimism-corrected C-index:",
    round(original.C$concordance - optimism.C, 2), ", 95% CI:",
    round(original.C$concordance - qnorm(0.975)*sqrt(original.C$var) - optimism.C, 2), "-",
    round(original.C$concordance + qnorm(0.975)*sqrt(original.C$var) - optimism.C, 2), "\n")

# save shrinkage
save(data, m, imputed.data, S, S.10, horizon,
     full.non.linear.model, form.full, full.model, form.bw, bw.model,
     shrinkage.factor,
     file=paste0(file.path, "Data/shrinkage.Rdata"))
