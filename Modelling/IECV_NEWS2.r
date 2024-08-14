setwd("/Users/jkv465/Desktop/Work_EWS/New_Data")

# Working directory for the rest should be found in the S-drive

#########################################
############# Packages ##################
#########################################

library(tidyverse) # For data analysis
library(tidymodels) # For modelling if needed
library(patchwork) # For plot merging
library(dcurves) # For decision curve analysis
library(ggsci) # More palettes
library(probably) # Calibration
library(rms) # Classic statistical modelling
library(doParallel) # Parallel processing
library(tidylog) # Observing pre-processing numbers
library(Publish) # Table 1 creation
library(writexl) # Excel conversion of data frames
library(arrow) # Read parquet files
library(riskRegression) # Risk modelling
library(metamisc) # Internal/External Cross-Validation
library(metafor) # Another meta-package

# First read the data

data_cv <- read_parquet("df_august.parquet")

# Specify formulas

f1 <- mort30D ~ Max_NEWS

# Evaluate AUC within an Internal External Cross-validation

f1_auc <- metapred(data_cv, 
                      perfFUN = "auc",
                      strata = "Hospital", 
                      formula = f1, scope = f1, 
                      family = binomial)

# Plot it

p1 <- metamisc::forest(f1_auc,study.digits = 2, slab = data_cv$Hospital,xlab = "",sort = 3) + 
  theme_gray(base_size = 12)


# Evaluate Brier Score within an Internal External Cross-validation

f1_brier <- metapred(data_cv, 
                        perfFUN = "mse",
                        strata = "Hospital", 
                        formula = f1, scope = f1, 
                        family = binomial)


p2 <- metamisc::forest(f1_brier,study.digits = 3, slab = data_cv$Hospital, xlab = "", sort = 3) + 
    theme_gray(base_size = 12)

p1/p2 # Merging the plots


# Fitting a meta-analysis regression to explore  I-squared and H-squared

# For AUC


current_30_auc <- data.frame(Hospitals = f1_auc$global.model$perf.all$auc$val.strata,
                            AUC = f1_auc$global.model$perf.all$auc$estimate,
                            AUC_SE = f1_auc$global.model$perf.all$auc$se,
                            AUC_LB = f1_auc$global.model$perf.all$auc$ci.lb,
                            AUC_UB = f1_auc$global.model$perf.all$auc$ci.ub)

current_result_30_auc <- rma(yi = AUC, sei = AUC_SE, data = current_30_auc, method = "REML",test="knha")


# For Brier Score

current_30_brier <- data.frame(Hospitals = f1_brier$global.model$perf.all$mse$val.strata,
                            Brier_Score = f1_brier$global.model$perf.all$mse$estimate,
                            Brier_Score_SE = f1_brier$global.model$perf.all$mse$se,
                            Brier_Score_LB = f1_brier$global.model$perf.all$mse$ci.lb,
                            Brier_Score_UB = f1_brier$global.model$perf.all$mse$ci.ub)

current_result_30_brier <- rma(yi = Brier_Score, sei = Brier_Score_SE, data = current_30_brier, method = "REML",test="knha")
