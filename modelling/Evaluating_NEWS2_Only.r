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


#######################################################


# Information
# 1. Age restricted to 105
# 2. IEWS and NEWS2-Light score accounting for age and sex implemented (see below)

# NEWS2-Light

# df <- df %>%
#   mutate(
#     Respiration_Score = case_when(
#       Respiration_Rate <= 8 ~ 3,
#       Respiration_Rate >= 9 & Respiration_Rate <= 11 ~ 1,
#       Respiration_Rate >= 12 & Respiration_Rate <= 20 ~ 0,
#       Respiration_Rate >= 21 & Respiration_Rate <= 24 ~ 2,
#       Respiration_Rate >= 25 ~ 3
#     ),
#     Saturation_Score = case_when(
#       Saturation <= 91 ~ 3,
#       Saturation >= 92 & Saturation <= 93 ~ 2,
#       Saturation >= 94 & Saturation <= 95 ~ 1,
#       Saturation >= 96 ~ 0,
#     ),
#     Oxygen_Supplement_Score = case_when(
#       Oxygen_Supplement == "Oxygen" ~ 2,
#       Oxygen_Supplement == "Air"  ~ 0,
#     ),
#     Pulse_Score = case_when(
#       Pulse <= 40 ~ 3,
#       Pulse >= 41 & Pulse <= 50 ~ 1,
#       Pulse >= 51 & Pulse <= 90 ~ 0,
#       Pulse >= 91 & Pulse <= 110 ~ 1,
#       Pulse >= 111 & Pulse <= 130 ~ 2,
#       Pulse >= 131 ~ 3,
#     ),
#     Consciousness_Score = case_when(
#       Consciousness == "A" ~ 0,
#       Consciousness == "VPU" ~ 3
#     ),
#     Total_Score = Respiration_Score + Saturation_Score + 
#       Pulse_Score + Oxygen_Supplement_Score +
#       Consciousness_Score
#   ) |> 
#   select(-Saturation_Score,-Respiration_Score,-Pulse_Score,-Oxygen_Supplement_Score,-Consciousness_Score)


# df <- df |> 
#   mutate(EWS_light = Total_Score) |> 
#   select(-Total_Score)


# # I-EWS with age and sex

# df <- df %>%
#   mutate(
#     Respiration_Score = case_when(
#       Respiration_Rate >= 0 & Respiration_Rate <= 20 ~ 0,
#       Respiration_Rate >= 21 & Respiration_Rate <= 24 ~ 2,
#       Respiration_Rate >= 25 ~ 3
#     ),
#     Saturation_Score = case_when(
#       Saturation <= 91 ~ 3,
#       Saturation >= 92 & Saturation <= 95 ~ 1,
#       Saturation >= 96 ~ 0,
#     ),
#     Oxygen_Supplement_Score = case_when(
#       Oxygen_Supplement == "Oxygen" ~ 1,
#       Oxygen_Supplement == "Air"  ~ 0,
#     ),
#     Pulse_Score = case_when(
#       Pulse <= 50 ~ 1,
#       Pulse >= 51 & Pulse <= 90 ~ 0,
#       Pulse >= 91 & Pulse <= 110 ~ 1,
#       Pulse >= 111 ~ 2
#     ),
#     Consciousness_Score = case_when(
#       Consciousness == "A" ~ 0,
#       Consciousness == "VPU" ~ 5
#     ),
#     Sex_Score = if_else(
#       Sex == "Male", 1,0
#     ),
#     Age_Score = case_when(
#       Age < 41 ~ 0,
#       Age >= 41 & Age <  51 ~ 1,
#       Age >= 51 & Age <  61 ~ 2,
#       Age >= 61 & Age <  66 ~ 3,
#       Age >= 66 & Age <  76 ~ 4,
#       Age >= 76 & Age <  81 ~ 5,
#       Age >= 81 & Age <  91 ~ 6,
#       Age >= 91 ~ 7
#     ),
#     Total_IScore = Respiration_Score + Saturation_Score + 
#       Pulse_Score + Oxygen_Supplement_Score +
#       Consciousness_Score + Sex_Score + Age_Score
#     ) |> 
#   select(-Saturation_Score,-Respiration_Score,
#          -Pulse_Score,-Oxygen_Supplement_Score,-Consciousness_Score,
#          -Sex_Score,-Age_Score)


# df <- df |> 
#   mutate(IEWS_Light = Total_IScore) |> 
#   select(-Total_IScore)


# 3. Initially restrict to max 20 measurements per person and per hospitalization to avoid over-representation
# 4. Hospitals added
# 5. Unique hospitalization number added, i.e CSN
# 6. Department name added
# 7. Imputation with median and mode for NAs (various other ways possible, no difference in results based on previous work)
# 8. Blood tests, interventions, diagnoses, intensive care unit data added
# 9. Early warning scores are summarised into max / mean per hospitalization and per patient, so every patient is repeated only based on the number of times they are hospitalized


# Open the initial dataset

data <- read_parquet("EWS_Final_Unique.parquet")

# Shift the position of the two first columns
# Here PT_ID is the personal identifier, and Identifier is a pseudo number that represents the unique PT_ID for each hospitalization

data <- data |> 
  relocate(PT_ID, .before = Identifier)

# Change some variables from characters to factors
# SKS_Category is diagnosis of the patient

data <- data |> 
  mutate_at(vars(ITA_Indicator,SKS_Category),as.factor)

# What we should do now is create a model that gives the propensity of having received an intervention (thats for comparing afterwards with new models)

model_weights = glm(Interventions ~ Age_Group + Sex + Hospital + 
                      Blood_Pressure.Sys + Temperature + Saturation + Pulse +
                      Oxygen_Supplement + Consciousness,data = data,family = "binomial")

# Compute the probability of receiving intervention

intervention_probs <- predictRisk(model_weights,data)

# Put them into the data (Inverse probability weighting)

data <- data |>
  mutate(preds = intervention_probs,
         weights = if_else(Interventions == 1, 1/preds,1/(1-preds)))

# Factorize interventions

data <- data |>
  mutate(Interventions = if_else(Interventions == 0, "No_Intervention","Intervention")) |>
  mutate_at(vars(Interventions),as.factor)

# Further pre-processing

data$Status30D <- relevel(data$Status30D,"Deceased")

# Refactoring the department categories (everything with less than 27486 counts gets lumped into the "Other" category)

data |> count(Department_Name)

data <- data |> 
  mutate(Department_Name_Fac = fct_lump_min(Department_Name,min =  27486))


# Categorizing previous hospitalization

data <- data |> 
  mutate(Previous_Hosp_Fac = case_when(Previous_Hosp == 0 ~ "Zero",
                                       Previous_Hosp == 1 ~ "One",
                                       Previous_Hosp == 2 ~ "Two",
                                       Previous_Hosp == 3 ~ "Three",
                                       Previous_Hosp == 4 ~ "Four",
                                       Previous_Hosp == 5 ~ "Five",
                                       .default = "6 or more")) |> 
  mutate_at(vars(Previous_Hosp_Fac),as.factor)


# Some statistics on the diagnoses

data |> count(SKS_Category,sort = T)

data <- data |> 
  mutate(SKS_Category = if_else(is.na(SKS_Category), "Unknown category",SKS_Category)) |> 
  mutate_at(vars(SKS_Category),as.factor)

# Defining Risk Groups based on EWS

data <- data |> 
  mutate(Risk_Groups_EWS = case_when(
    EWS_score >= 7 ~ "High",
    EWS_score >= 5 & EWS_score <= 6 ~ "Medium",
    (Respiration_Rate <= 8 | Respiration_Rate >=25) | (Saturation <= 91) | 
      (Pulse <= 40 | Pulse >= 131) | (Consciousness == "VPU") | (Temperature <= 35) | 
      (Blood_Pressure.Sys <= 90 | Blood_Pressure.Sys >= 220) ~ "Low-Medium",
    EWS_score >= 0 & EWS_score <= 4 ~ "Low")) |> 
  mutate(Risk_Groups_EWS = as.factor(Risk_Groups_EWS))

# Save the dataset just in case we work in Python

# write_parquet(data,"df_august.parquet")

#########################################################################
###################### Modelling ########################################
#########################################################################

data$imp_weights <- importance_weights(data$weights) # Not necessary for the evaluation of the current scoring system

# data <- data |> filter(Interventions == 0) For now we skip this

set.seed(234)

data_folds <- group_vfold_cv(data,group = Hospital)

# Now we will create our model 

model <- logistic_reg(mode = "classification",engine = "glm")

# Workflows now

# Current model

current_wf <- workflow() |> 
  add_formula(Status30D ~ Max_NEWS) |>
  add_model(model)


# Set up parallel processing
doParallel::registerDoParallel(cores = 6)

cntrl <- control_resamples(save_pred = T)


# Internal-External validation of the current EWS (checking demographic parity also)

current_fit <- fit_resamples(current_wf,resamples = data_folds,
                             metrics = metric_set(
                               roc_auc,
                               brier_class,
                               demographic_parity(Age_Group),
                               demographic_parity(Sex),
                               demographic_parity(Department_Name_Fac),
                               demographic_parity(Hospital),
                               demographic_parity(Risk_Groups_EWS),
                               demographic_parity(Previous_Hosp_Fac),
                               demographic_parity(SKS_Category),
                               demographic_parity(Interventions),
                               demographic_parity(ITA_Indicator)), 
                             ,control = cntrl)


# Collect metrics

current_fit |> collect_metrics() 


# Compute confidence interval of performance metrics

current_fit_ci <- fit_resamples(current_wf,resamples = data_folds,
                                metrics = metric_set(
                                  roc_auc,
                                  brier_class), 
                                control = cntrl)

doParallel::registerDoParallel(cores = 6)

set.seed(222)

confidence_bands <- int_pctl(current_fit_ci,times = 1000) 

confidence_bands


# Calibration curves

##############################################
########### Calibration Curves ###############
##############################################

# Get the overall calibration curves

cal_current <- current_fit |> 
  collect_predictions() |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_Deceased,include_rug = F) + 
  theme_gray(base_size = 12) + 
  labs(x = "Predictions from NEWS2 model")

cal_current



# Get the calibration curves for the different age groups

cal_current_age <- current_fit |> 
  collect_predictions() |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_Deceased,include_rug = F,.by = Age_Group) + 
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") + 
  labs(x = "Predictions from NEWS2 model")


cal_current_age


# Get the calibration curves for the different sexes

cal_current_sex <- current_fit |> 
  collect_predictions() |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_Deceased,include_rug = F,.by = Sex) + 
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") + 
  labs(x = "Predictions from NEWS2 model")

cal_current_sex


# Get the calibration curves for people receiving interventions and those who have not

cal_current_int <- current_fit |> 
  collect_predictions() |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_Deceased,include_rug = F,.by = Interventions) + 
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") + 
  labs(x = "Predictions from NEWS2 model")


cal_current_int


# Get the calibration curves for people in the ITA and those not

cal_current_ita <- current_fit |> 
  collect_predictions() |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_Deceased,include_rug = F,.by = ITA_Indicator) + 
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") + 
  labs(x = "Predictions from NEWS2 model")


cal_current_ita


# Looking deeper at the ITA category

# For youngsters

cal_current_ita_young <- current_fit |> 
  collect_predictions() |>
  filter(Age_Group == "18-65") |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_Deceased,include_rug = F,.by = ITA_Indicator) + 
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") + 
  labs(x = "Predictions from NEWS2 model")


cal_current_ita_young 


# For middle aged

cal_current_ita_middle <- current_fit |> 
  collect_predictions() |>
  filter(Age_Group == "66-80") |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_Deceased,include_rug = F,.by = ITA_Indicator) + 
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") + 
  labs(x = "Predictions from NEWS2 model")


cal_current_ita_middle



# Old category

cal_current_ita_old <- current_fit |> 
  collect_predictions() |>
  filter(Age_Group == "80+") |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_Deceased,include_rug = F,.by = ITA_Indicator) + 
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") + 
  labs(x = "Predictions from NEWS2 model")


cal_current_ita_old



# Get the calibration curves for the different hospital departments

cal_current_department <- current_fit |> 
  collect_predictions() |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_Deceased,include_rug = F,.by = Department_Name_Fac) + 
  theme_gray(base_size = 12) + 
  facet_wrap(vars(Department_Name_Fac),nrow = 5) + 
  theme(legend.position = "topleft") +
  labs(x = "Predictions from NEWS2 model")


cal_current_department


# Get the calibration curves for the different hospitals


cal_current_hosp <- current_fit |> 
  collect_predictions() |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_Deceased,include_rug = F,.by = Hospital) + 
  theme_gray(base_size = 12) + 
  facet_wrap(vars(Hospital),nrow = 5) + 
  theme(legend.position = "topleft") +
  labs(x = "Predictions from NEWS2 model")


cal_current_hosp


# Get the calibration curves for the different sks

cal_current_sks <- current_fit |> 
  collect_predictions() |>
  filter(SKS_Category %in% c("Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
                             "Factors influencing health status and contact with health services", 
                             "Injury, poisoning and certain other consequences of external causes",
                             "Diseases of the circulatory system",
                             "Diseases of the digestive system",
                             "Diseases of the musculoskeletal system and connective tissue",
                             "Diseases of the genitourinary system",
                             "Diseases of the respiratory system",
                             "Neoplasms",
                             "Certain infectious and parasitic diseases")) |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_Deceased,include_rug = F,.by = SKS_Category) + 
  theme_gray(base_size = 12) + 
  facet_wrap(vars(SKS_Category),labeller = label_wrap_gen(width = 20)) + 
  theme(legend.position = "topleft") +
  labs(x = "Predictions from NEWS2 model")


cal_current_sks


################################################################################
########################## Decision curve analysis #############################
################################################################################

# We need to somehow translate the predicted probabilities into clinical groups

pred_df <- current_fit |>
  collect_predictions() |>
  arrange(.row) |>
  mutate(mort30D = if_else(Status30D == "Deceased",1,0))


# Now let's see how the predicted probabilities translate to the groups

pred_df |>
  group_by(Risk_Groups_EWS) |>
  summarize(Mean_Probability = mean(.pred_Deceased), Q25 = quantile(.pred_Deceased,0.25)) |>
  arrange(Mean_Probability)

# Specify thresholds 

dca_thresholds <- seq(0.01, 0.60, by = 0.01)

# Grab predictions from current_fit 

preds_current <- current_fit |> 
  collect_predictions() |>
  arrange(.row) |>
  pull(.pred_Deceased)


# Put all of them in the dataframe

data <- data |>
  mutate(cv_pred_current = preds_current)

# Create the curves

dca_first <- dcurves::dca(
  data = data,
  formula = mort30D ~ cv_pred_current,
  thresholds = dca_thresholds,
  label = list(
    cv_pred_current = "NEWS2"
  )) |> plot(smooth = T)

dca_plot1 <- dca_first + 
  theme_grey(base_size = 12) + 
  theme(legend.position = "top") +
  geom_textvline(xintercept = 0.00626,label = "Low",linetype = 2) + 
  geom_textvline(xintercept = 0.0214,label = "Low-Medium", linetype = 2) + 
  geom_textvline(xintercept = 0.0487, label = "Medium", linetype = 2) + 
  geom_textvline(xintercept = 0.110, label = "High", linetype = 2) + 
  labs(y = "Net Benefit\nper 1000 patients", x = NULL) +
  scale_y_continuous(labels = scales::label_number(scale = 1000))


# For the youngest age group


dca_youngest <- dcurves::dca(
  data = data |> filter(Age_Group == "18-65"),
  formula = mort30D ~ cv_pred_current,
  thresholds = seq(0,0.2,0.01),
  label = list(
    cv_pred_current = "NEWS2"
  )) |> plot(smooth = T)

dca_plot2 <- dca_youngest + 
  theme_grey(base_size = 12) + 
  theme(legend.position = "top") +
  geom_textvline(xintercept = 0.00626,label = "Low",linetype = 2) + 
  geom_textvline(xintercept = 0.0214,label = "Low-Medium", linetype = 2) + 
  geom_textvline(xintercept = 0.0487, label = "Medium", linetype = 2) + 
  geom_textvline(xintercept = 0.110, label = "High", linetype = 2) + 
  labs(y = "Net Benefit\nper 1000 patients", x = NULL) +
  scale_y_continuous(labels = scales::label_number(scale = 1000))


# For the middle group

# Middle group

dca_middle <- dcurves::dca(
  data = data |> filter(Age_Group == "66-80"),
  formula = mort30D ~ cv_pred_current,
  thresholds = seq(0,0.2,0.01),
  label = list(
    cv_pred_current = "NEWS2"
  )) |> plot(smooth = T)


dca_plot3 <- dca_middle + 
  theme_grey(base_size = 12) + 
  theme(legend.position = "top") +
  geom_textvline(xintercept = 0.00626,label = "Low",linetype = 2) + 
  geom_textvline(xintercept = 0.0214,label = "Low-Medium", linetype = 2) + 
  geom_textvline(xintercept = 0.0487, label = "Medium", linetype = 2) + 
  geom_textvline(xintercept = 0.110, label = "High", linetype = 2) + 
  labs(y = NULL, x = NULL) +
  scale_y_continuous(labels = scales::label_number(scale = 1000))


# Oldest group

dca_old <- dcurves::dca(
  data = data |> filter(Age_Group == "80+"),
  formula = mort30D ~ cv_pred_current,
  thresholds = seq(0,0.2,0.01),
  label = list(
    cv_pred_current = "NEWS2"
  )) |> plot(smooth = T)


dca_plot3 <- dca_old + 
  theme_grey(base_size = 12) + 
  theme(legend.position = "top") +
  geom_textvline(xintercept = 0.00626,label = "Low",linetype = 2) + 
  geom_textvline(xintercept = 0.0214,label = "Low-Medium", linetype = 2) + 
  geom_textvline(xintercept = 0.0487, label = "Medium", linetype = 2) + 
  geom_textvline(xintercept = 0.110, label = "High", linetype = 2) + 
  labs(y = NULL, x = NULL) +
  scale_y_continuous(labels = scales::label_number(scale = 1000))


# Decision curve analysis on ITA and non ITA (Intensive Care)


dca_ita <- dcurves::dca(
  data = data |> filter(ITA_Indicator == "NO_ITA"),
  formula = mort30D ~ cv_pred_current,
  thresholds = seq(0,0.2,0.01),
  label = list(
    cv_pred_current = "NEWS2"
  )) |> plot(smooth = T)


dca_plot4 <- dca_ita + 
  theme_grey(base_size = 12) + 
  theme(legend.position = "top") +
  labs(y = "Net Benefit\nper 1000 patients", x = NULL) +
  scale_y_continuous(labels = scales::label_number(scale = 1000))



dca_ita2 <- dcurves::dca(
  data = data |> filter(ITA_Indicator == "ITA"),
  formula = mort30D ~ cv_pred_current,
  thresholds = seq(0,0.2,0.01),
  label = list(
    cv_pred_current = "NEWS2"
  )) |> plot(smooth = T)


dca_plot5 <- dca_ita2 + 
  theme_grey(base_size = 12) + 
  theme(legend.position = "top") +
  geom_textvline(xintercept = 0.00626,label = "Low",linetype = 2) + 
  geom_textvline(xintercept = 0.0214,label = "Low-Medium", linetype = 2) + 
  geom_textvline(xintercept = 0.0487, label = "Medium", linetype = 2) + 
  geom_textvline(xintercept = 0.110, label = "High", linetype = 2) + 
  labs(y = "Net Benefit\nper 1000 patients", x = NULL) +
  scale_y_continuous(labels = scales::label_number(scale = 1000))


# Decision curve analysis on Interventions vs non Interventions (More broad than ITA only)

dca_no_int <- dcurves::dca(
  data = data |> filter(Interventions == "No_Intervention"),
  formula = mort30D ~ cv_pred_current,
  thresholds = seq(0,0.2,0.01),
  label = list(
    cv_pred_current = "NEWS2"
  )) |> plot(smooth = T)


dca_plot6 <- dca_no_int + 
  theme_grey(base_size = 12) + 
  theme(legend.position = "top") +
  geom_textvline(xintercept = 0.00626,label = "Low",linetype = 2) + 
  geom_textvline(xintercept = 0.0214,label = "Low-Medium", linetype = 2) + 
  geom_textvline(xintercept = 0.0487, label = "Medium", linetype = 2) + 
  geom_textvline(xintercept = 0.110, label = "High", linetype = 2) + 
  labs(y = "Net Benefit\nper 1000 patients", x = NULL) +
  scale_y_continuous(labels = scales::label_number(scale = 1000))



dca_int <- dcurves::dca(
  data = data |> filter(Interventions == "Intervention"),
  formula = mort30D ~ cv_pred_current,
  thresholds = seq(0,0.2,0.01),
  label = list(
    cv_pred_current = "NEWS2"
  )) |> plot(smooth = T)


dca_plot7 <- dca_int + 
  theme_grey(base_size = 12) + 
  theme(legend.position = "top") +
  labs(y = "Net Benefit\nper 1000 patients", x = NULL) +
  geom_textvline(xintercept = 0.00626,label = "Low",linetype = 2) + 
  geom_textvline(xintercept = 0.0214,label = "Low-Medium", linetype = 2) + 
  geom_textvline(xintercept = 0.0487, label = "Medium", linetype = 2) + 
  geom_textvline(xintercept = 0.110, label = "High", linetype = 2) + 
  scale_y_continuous(labels = scales::label_number(scale = 1000))

# End of decision curve analysis

# Evaluate mortality proportions per Max EWS (for demonstration purposes)

data |>
  group_by(Max_NEWS) |>
  summarise(Mortality = mean(mort30D == 1)) |>
  ggplot(aes(x = Max_NEWS, y = Mortality)) + 
  geom_smooth()
