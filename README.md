# Tutorial in R: develop and internally validate a clinical prediction model

File 1_Load_Data_and_Descriptives.R can be used to load data and make a Table 1 to display the important characteristics of the patients in the data set.

File 2_Describe_Survival_Data.R is only relevant when dealing with time-to-event data, and illustrates how to make a survival probability curve (Kaplan-Meier).

File 3_Multiple_Imputation.R shows how to use Multivariate Imputation by Chained Equations (mice) to impute missing data.

File 4_Fit_Models.R illustrates how to investigate non-linearities of predictors, fitting a model using all imputed data sets, and perform backward stepwise variable selection to obtain a parsimonious model.

File 5_Uniform_Shrinkage_Optimism_Corrected_C_Index.R presents how to use bootstrapping to determine a uniform shrinkage factor and correct the C-index for optimism.

File 6_Model_Performance.R shows how to make a calibration plot with performance metrics.

File 7_Internal_External_Cross_Validation.R illustrates how to perform internal-external cross-validation, i.e., leaving one of the j centers out of development, execute the modeling strategy (backward selection and uniform shrinkage factor), and validate the new model on the jth center, repeating this until each center has been a validation set once.
