# clear the environment
rm(list=ls())

# load libraries
library(survival)
library(survminer) # Kaplan-Meier curve
library(prodlim)   # median follow-up time

# set file.path
file.path <- "C:/Users/carol/OneDrive - Erasmus MC/Projects Tufts/Course - Predictive Models 2024/R tutorials/"

# load dummy data from survival package
data(pbc, package="survival")
data <- pbc

# add center variable for illustration
data$center <- rep(1:2, length.out=nrow(data))

# The following is only relevent if you're dealing with time-to-event data:
# generate the Surv object for the outcome
# transform time from days to years
# dichotomize the event (0/1/2 for censored, transplant, dead) 
S <- Surv(time=data$time/365, event=as.numeric(data$status==2))

# limit the outcome to your time horizon
horizon <- 10
S.10 <- S
S.10[S.10[, 1]>horizon, 2] <- 0
S.10[S.10[, 1]>horizon, 1] <- horizon

# make Kaplan-Meier curve
KM <- survival::survfit(S.10 ~ trt, data=data)

# display the Kaplan-Meier estimates at the horizon
summary(KM, time=horizon)

# make ggplot Kaplan-Meier curve
KM.plot <- survminer::ggsurvplot(KM, 
                                 data=data,
                                 risk.table=TRUE, 
                                 legend="none", 
                                 conf.int=TRUE)

# save Kaplan-Meier curve
ggsave(filename=paste0(file.path, "Results/KM.plot.png"),
       plot=ggarrange(KM.plot$plot, KM.plot$table, 
                      nrow=2, heights=c(3, 1)),
       width=7, height=7, dpi=300)

# median follow-up time
stats::quantile(prodlim::prodlim(prodlim::Hist(time/365, status==2)~1, 
                                 data=data, reverse=TRUE))

# save data elements
save(data, S, S.10, horizon,
     file=paste0(file.path, "Data/data.Rdata"))
