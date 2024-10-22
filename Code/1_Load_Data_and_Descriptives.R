# clear the environment
rm(list=ls())

# load libraries
library(survival)
library(dplyr)     # data manipulations
library(arsenal)   # Table 1
library(flextable) # save to Word

# set file.path
file.path <- "C:/Users/carol/OneDrive - Erasmus MC/Projects Tufts/Course - Predictive Models 2024/R tutorials/"

# load Excel data
# data <- readxl::read_excel("C:/.../file_data_name.xlsx")
# load SPSS file
# data <- foreign::read.spss(file="C:/.../file_data_name.sav", to.data.frame=TRUE, origin="01-01-1970")

# load dummy data from survival package
# data dictionary: https://cran.r-project.org/web/packages/survival/survival.pdf
data(pbc, package="survival")
data <- pbc

# add center variable for illustration
data$center <- rep(1:2, length.out=nrow(data))

# select the variables you want to display in your descriptives table
data <- dplyr::select(data, trt, age, sex, ascites, hepato, spiders, edema,
               bili, chol, albumin, copper, alk.phos, ast, trig, 
               platelet, protime, stage, center)

# binary and categorical variables should be factor variables
factor.var <- c("sex", "ascites", "hepato", "spiders", "stage", "center")
data[, factor.var] <- lapply(data[, factor.var], as.factor)

# write Table 1 to Word
table_one <- arsenal::tableby(center ~ .,
                              data=data,
                              total.pos="before",
                              digits=1,
                              test=FALSE,
                              total=TRUE,
                              numeric.test="kwt",
                              cat.test="chisq", 
                              cat.simplify=TRUE,
                              numeric.stats=c("meansd", "medianq1q3",
                                              "range", "Nmiss2"),
                              cat.stats=c("countpct", "Nmiss2"),
                              stats.labels=list(
                                meansd="Mean (SD)",
                                medianq1q3="Median (Q1, Q3)",
                                range="Min - Max",
                                Nmiss2="Missing"))
flextable::flextable(data.frame(summary(table_one, text=TRUE))) |>
  flextable::save_as_docx(path=paste0(file.path, "Results/Descriptives.docx"))