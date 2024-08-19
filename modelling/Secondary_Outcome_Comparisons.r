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

# Keep only the interventions people

data <- data |>
    filter(Interventions == "No_Intervention")

# Save the dataset just in case we work in Python

# write_parquet(data,"df_august.parquet")

# Create a new variable for sustained recovery

result_df <- data %>%
  group_by(PT_ID) %>%
  # Create the sustained recovery variable
  mutate(
    next_admission_date = lead(HOSP_DISCH_TIME),
    days_to_next_admission = as.numeric(next_admission_date - HOSP_DISCH_TIME),
    days_to_death = as.numeric(deathDate - HOSP_DISCH_TIME),
    sustained_recovery = case_when(
      # If there's no next admission and no death date, it's a sustained recovery
      is.na(next_admission_date) & is.na(deathDate) ~ 0,
      # If next admission is more than 30 days later and death is either NA or more than 30 days later
      (is.na(days_to_next_admission) | days_to_next_admission > 30) & 
        (is.na(days_to_death) | days_to_death > 30) ~ 0,
      # Otherwise, it's not a sustained recovery
      TRUE ~ 1
    )
  ) %>%
  # Remove the temporary columns we created
  select(-next_admission_date, -days_to_next_admission, -days_to_death) %>%
  ungroup()

data <- result_df

data <- data |>
  mutate(Status30D = sustained_recovery) |>
  mutate(Status30D = if_else(Status30D == 0, "Sustained_Recovery", "No_Recovery")) |>
  mutate_at(vars(Status30D),as.factor)

data$Status30D <- relevel(data$Status30D,"No_Recovery")

######################################
############# Modelling ##############
######################################

set.seed(234)

data_folds <- group_vfold_cv(data,group = Hospital)

# Now we will create our model 

model <- logistic_reg(mode = "classification",engine = "glm")

# A more sophisticated XGBoost model

xgb <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification")

# Workflows now

# Current model

current_wf <- workflow() |> 
  add_formula(Status30D ~ Max_NEWS) |>
  add_model(model)

# NEWS2-Light

light_wf <- workflow() |> 
  add_formula(Status30D ~ Max_NEWS_Light) |>
  add_model(model)

# IEWS

full_wf <- workflow() |> 
  add_formula(Status30D ~ Max_IEWS) |>
  add_model(model)

# XGBoost

xgb_wf <- workflow() |>
  add_formula(Status30D ~  Age + Sex + Respiration_Rate + Temperature + Saturation + Oxygen_Supplement + Blood_Pressure.Sys + Blood_Pressure.Dia + Consciousness + Previous_Hosp_Fac + 
              Hemoglobin + Leukocytes + Trombocytes + Kreatinin + ALAT + LDH + Albumin + CRP + Laktak_ab + Troponin + Laktat_vb) |>
  add_model(xgb)

xgb_wf
  
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

light_fit <- fit_resamples(light_wf,resamples = data_folds,
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


full_fit <- fit_resamples(full_wf,resamples = data_folds,
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

xgb_fit <- fit_resamples(xgb_wf,resamples = data_folds,
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


# Now let's compute the weighted metrics for all of them (AUC and Brier Score)

current_fit |> collect_metrics()

# Gather the weighted metrics

weighted_current <- current_fit |> 
    collect_predictions() |>
    arrange(.row) |>
    mutate(weights = data$weights,
           mort30D = if_else(Status30D == "No_Recovery",1,0)) |>
    group_by(id) |>
    summarise(AUC_Weighted = MetricsWeighted::AUC(mort30D, .pred_No_Recovery, w = weights),
              AUC_Unweighted = MetricsWeighted::AUC(mort30D, .pred_No_Recovery),
              Brier_Score_Weighted = MetricsWeighted::mse(mort30D,.pred_No_Recovery,w = weights),
              Brier_Score_Unweighted = MetricsWeighted::mse(mort30D,.pred_No_Recovery)) |>
    ungroup() |>
    summarise(Mean_AUC = mean(AUC_Weighted), Mean_Brier = mean(Brier_Score_Weighted), SD_AUC = sd(AUC_Weighted), SD_Brier = sd(Brier_Score_Weighted)) |>
    mutate(Lower_AUC = Mean_AUC - 1.96*(SD_AUC/sqrt(10)), Upper_AUC = Mean_AUC + 1.96*(SD_AUC/sqrt(10)), Lower_Brier = Mean_Brier - 1.96*(SD_Brier/sqrt(10)), Upper_Brier = Mean_Brier + 1.96*(SD_Brier/sqrt(10))) |>
    relocate(Lower_AUC,.after = Mean_AUC) |>
    relocate(Upper_AUC, .after = Lower_AUC) |>
    relocate(Lower_Brier, .after = Mean_Brier) |>
    relocate(Upper_Brier, .after = Lower_Brier)


weighted_light <- light_fit |> 
    collect_predictions() |>
    arrange(.row) |>
    mutate(weights = data$weights,
           mort30D = if_else(Status30D == "No_Recovery",1,0)) |>
    group_by(id) |>
    summarise(AUC_Weighted = MetricsWeighted::AUC(mort30D, .pred_No_Recovery, w = weights),
              AUC_Unweighted = MetricsWeighted::AUC(mort30D, .pred_No_Recovery),
              Brier_Score_Weighted = MetricsWeighted::mse(mort30D,.pred_No_Recovery,w = weights),
              Brier_Score_Unweighted = MetricsWeighted::mse(mort30D,.pred_No_Recovery)) |>
    ungroup() |>
    summarise(Mean_AUC = mean(AUC_Weighted), Mean_Brier = mean(Brier_Score_Weighted), SD_AUC = sd(AUC_Weighted), SD_Brier = sd(Brier_Score_Weighted)) |>
    mutate(Lower_AUC = Mean_AUC - 1.96*(SD_AUC/sqrt(10)), Upper_AUC = Mean_AUC + 1.96*(SD_AUC/sqrt(10)), Lower_Brier = Mean_Brier - 1.96*(SD_Brier/sqrt(10)), Upper_Brier = Mean_Brier + 1.96*(SD_Brier/sqrt(10))) |>
    relocate(Lower_AUC,.after = Mean_AUC) |>
    relocate(Upper_AUC, .after = Lower_AUC) |>
    relocate(Lower_Brier, .after = Mean_Brier) |>
    relocate(Upper_Brier, .after = Lower_Brier)


weighted_full <- full_fit |> 
    collect_predictions() |>
    arrange(.row) |>
    mutate(weights = data$weights,
           mort30D = if_else(Status30D == "No_Recovery",1,0)) |>
    group_by(id) |>
    summarise(AUC_Weighted = MetricsWeighted::AUC(mort30D, .pred_No_Recovery, w = weights),
              AUC_Unweighted = MetricsWeighted::AUC(mort30D, .pred_No_Recovery),
              Brier_Score_Weighted = MetricsWeighted::mse(mort30D,.pred_No_Recovery,w = weights),
              Brier_Score_Unweighted = MetricsWeighted::mse(mort30D,.pred_No_Recovery)) |>
    ungroup() |>
    summarise(Mean_AUC = mean(AUC_Weighted), Mean_Brier = mean(Brier_Score_Weighted), SD_AUC = sd(AUC_Weighted), SD_Brier = sd(Brier_Score_Weighted)) |>
    mutate(Lower_AUC = Mean_AUC - 1.96*(SD_AUC/sqrt(10)), Upper_AUC = Mean_AUC + 1.96*(SD_AUC/sqrt(10)), Lower_Brier = Mean_Brier - 1.96*(SD_Brier/sqrt(10)), Upper_Brier = Mean_Brier + 1.96*(SD_Brier/sqrt(10))) |>
    relocate(Lower_AUC,.after = Mean_AUC) |>
    relocate(Upper_AUC, .after = Lower_AUC) |>
    relocate(Lower_Brier, .after = Mean_Brier) |>
    relocate(Upper_Brier, .after = Lower_Brier)


weighted_xgb <- xgb_fit |> 
    collect_predictions() |>
    arrange(.row) |>
    mutate(weights = data$weights,
           mort30D = if_else(Status30D == "No_Recovery",1,0)) |>
    group_by(id) |>
    summarise(AUC_Weighted = MetricsWeighted::AUC(mort30D, .pred_No_Recovery, w = weights),
              AUC_Unweighted = MetricsWeighted::AUC(mort30D, .pred_No_Recovery),
              Brier_Score_Weighted = MetricsWeighted::mse(mort30D,.pred_No_Recovery,w = weights),
              Brier_Score_Unweighted = MetricsWeighted::mse(mort30D,.pred_No_Recovery)) |>
    ungroup() |>
    summarise(Mean_AUC = mean(AUC_Weighted), Mean_Brier = mean(Brier_Score_Weighted), SD_AUC = sd(AUC_Weighted), SD_Brier = sd(Brier_Score_Weighted)) |>
    mutate(Lower_AUC = Mean_AUC - 1.96*(SD_AUC/sqrt(10)), Upper_AUC = Mean_AUC + 1.96*(SD_AUC/sqrt(10)), Lower_Brier = Mean_Brier - 1.96*(SD_Brier/sqrt(10)), Upper_Brier = Mean_Brier + 1.96*(SD_Brier/sqrt(10))) |>
    relocate(Lower_AUC,.after = Mean_AUC) |>
    relocate(Upper_AUC, .after = Lower_AUC) |>
    relocate(Lower_Brier, .after = Mean_Brier) |>
    relocate(Upper_Brier, .after = Lower_Brier)


# Bind all of them together

weighted_metrics <- bind_rows(weighted_current,weighted_light, weighted_full, weighted_xgb) |>
    mutate(Model = c("NEWS2", "NEWS2-Light", "IEWS", "TREE-EWS")) |>
    relocate(Model,.before = Mean_AUC)


# Now let's get the calibrations

# Define number of bins
n_bins <- 20

######################################################
################ For current NEWS2 model #############
######################################################

# Bin the predicted probabilities
cals_current <- current_fit |>
    collect_predictions() |>
    arrange(.row) |>
    mutate(weights = data$weights) |>
    mutate(bin = cut(.pred_No_Recovery, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE)) |>
    mutate(mort30D = if_else(Status30D == "No_Recovery",1,0))


# Calculate weighted fraction of positives for each resample
resample_calibration_current <- cals_current |>
  group_by(id, bin) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop'
  )


# Calculate overall mean and confidence intervals for each bin
calibration_summary_current <- resample_calibration_current |>
  group_by(bin) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

# Overall calibration line

# Plot the calibration curve with confidence bands

current_overall_weighted <- ggplot(calibration_summary_current, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, ymin = lower_ci, ymax = upper_ci)) +
  see::geom_point2(stroke = 4) +
  geom_line() + 
  geom_errorbar() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12)


# Now we need to do it for different age groups

# Calculate overall mean and confidence intervals for each bin

resample_calibration_current_age <- cals_current |>
  group_by(id, bin, Age_Group) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')


calibration_summary_current_age <- resample_calibration_current_age |>
  group_by(bin,Age_Group) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

current_age_weighted <- ggplot(calibration_summary_current_age, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = Age_Group, colour = Age_Group)) +
  see::geom_point2(stroke = 4) +
  geom_line() + 
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(Age_Group)) + 
  ggokabeito::scale_colour_okabe_ito() +
  ggokabeito::scale_fill_okabe_ito() +
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "top")


# Now we need to do it for sexes

# Calculate overall mean and confidence intervals for each bin

resample_calibration_current_sex <- cals_current |>
  group_by(id, bin, Sex) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')

calibration_summary_current_sex <- resample_calibration_current_sex |>
  group_by(bin,Sex) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

current_sex_weighted <- ggplot(calibration_summary_current_sex, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = Sex, colour = Sex)) +
  see::geom_point2(stroke = 4) +
  geom_line() + 
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(Sex)) + 
  ggokabeito::scale_colour_okabe_ito() +
  ggokabeito::scale_fill_okabe_ito() +
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "top")


# Now we do it for ITA and No ITA

resample_calibration_current_ita <- cals_current |>
  group_by(id, bin, ITA_Indicator) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')


calibration_summary_current_ita <- resample_calibration_current_ita |>
  group_by(bin,ITA_Indicator) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

current_ita_weighted <- ggplot(calibration_summary_current_ita, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = ITA_Indicator, colour = ITA_Indicator)) +
  see::geom_point2(stroke = 4) +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(ITA_Indicator)) + 
  ggokabeito::scale_colour_okabe_ito() +
  ggokabeito::scale_fill_okabe_ito() +
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "top")


# Now we do it for Hospitals

resample_calibration_current_hosp <- cals_current |>
  group_by(id, bin, Hospital) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')


calibration_summary_current_hosp <- resample_calibration_current_hosp |>
  group_by(bin,Hospital) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

current_hosp_weighted <- ggplot(calibration_summary_current_hosp, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = Hospital, colour = Hospital)) +
  see::geom_point2(stroke = 4) +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(Hospital)) + 
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "top")

# No confidence bands for hospitals due to the fold testing we introduce


# For SKS Categories

resample_calibration_current_diagn <- cals_current |>
  group_by(id, bin, SKS_Category) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')


calibration_summary_current_diagn <- resample_calibration_current_diagn |>
  group_by(bin,SKS_Category) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

current_diagn_weighted <- ggplot(calibration_summary_current_diagn, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = SKS_Category, colour = SKS_Category)) +
  see::geom_point2(stroke = 4) +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(SKS_Category),labeller = label_wrap_gen(width = 40)) + 
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft")

##############################################
##############################################

######################################
############ NEWS2-Light #############
######################################


# Bin the predicted probabilities
cals_light <- light_fit |>
    collect_predictions() |>
    arrange(.row) |>
    mutate(weights = data$weights) |>
    mutate(bin = cut(.pred_No_Recovery, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE)) |>
    mutate(mort30D = if_else(Status30D == "No_Recovery",1,0))


# Calculate weighted fraction of positives for each resample
resample_calibration_light <- cals_light |>
  group_by(id, bin) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop'
  )


# Calculate overall mean and confidence intervals for each bin
calibration_summary_light <- resample_calibration_light |>
  group_by(bin) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

# Overall calibration line

# Plot the calibration curve with confidence bands

light_overall_weighted <- ggplot(calibration_summary_light, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, ymin = lower_ci, ymax = upper_ci)) +
  see::geom_point2(stroke = 4) +
  geom_line() + 
  geom_errorbar() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12)


# Now we need to do it for different age groups

# Calculate overall mean and confidence intervals for each bin

resample_calibration_light_age <- cals_light |>
  group_by(id, bin, Age_Group) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')


calibration_summary_light_age <- resample_calibration_light_age |>
  group_by(bin,Age_Group) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

light_age_weighted <- ggplot(calibration_summary_light_age, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = Age_Group, colour = Age_Group)) +
  see::geom_point2(stroke = 4) +
  geom_line() + 
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(Age_Group)) + 
  ggokabeito::scale_colour_okabe_ito() +
  ggokabeito::scale_fill_okabe_ito() +
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "top")


# Now we need to do it for sexes

# Calculate overall mean and confidence intervals for each bin

resample_calibration_light_sex <- cals_light |>
  group_by(id, bin, Sex) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')

calibration_summary_light_sex <- resample_calibration_light_sex |>
  group_by(bin,Sex) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

light_sex_weighted <- ggplot(calibration_summary_light_sex, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = Sex, colour = Sex)) +
  see::geom_point2(stroke = 4) +
  geom_line() + 
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(Sex)) + 
  ggokabeito::scale_colour_okabe_ito() +
  ggokabeito::scale_fill_okabe_ito() +
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "top")


# Now we do it for ITA and No ITA

resample_calibration_light_ita <- cals_light |>
  group_by(id, bin, ITA_Indicator) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')


calibration_summary_light_ita <- resample_calibration_light_ita |>
  group_by(bin,ITA_Indicator) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

light_ita_weighted <- ggplot(calibration_summary_light_ita, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = ITA_Indicator, colour = ITA_Indicator)) +
  see::geom_point2(stroke = 4) +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(ITA_Indicator)) + 
  ggokabeito::scale_colour_okabe_ito() +
  ggokabeito::scale_fill_okabe_ito() +
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "top")


# Now we do it for Hospitals

resample_calibration_light_hosp <- cals_light |>
  group_by(id, bin, Hospital) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')


calibration_summary_light_hosp <- resample_calibration_light_hosp |>
  group_by(bin,Hospital) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

light_hosp_weighted <- ggplot(calibration_summary_light_hosp, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = Hospital, colour = Hospital)) +
  see::geom_point2(stroke = 4) +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(Hospital)) + 
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "top")

# No confidence bands for hospitals due to the fold testing we introduce


# For SKS Categories

resample_calibration_light_diagn <- cals_light |>
  group_by(id, bin, SKS_Category) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')


calibration_summary_light_diagn <- resample_calibration_light_diagn |>
  group_by(bin,SKS_Category) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

light_diagn_weighted <- ggplot(calibration_summary_light_diagn, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = SKS_Category, colour = SKS_Category)) +
  see::geom_point2(stroke = 4) +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(SKS_Category),labeller = label_wrap_gen(width = 40)) + 
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft")

################################
############ IEWS ##############
################################

# Bin the predicted probabilities
cals_full <- full_fit |>
    collect_predictions() |>
    arrange(.row) |>
    mutate(weights = data$weights) |>
    mutate(bin = cut(.pred_No_Recovery, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE)) |>
    mutate(mort30D = if_else(Status30D == "No_Recovery",1,0))


# Calculate weighted fraction of positives for each resample
resample_calibration_full <- cals_full |>
  group_by(id, bin) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop'
  )


# Calculate overall mean and confidence intervals for each bin
calibration_summary_full <- resample_calibration_full |>
  group_by(bin) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

# Overall calibration line

# Plot the calibration curve with confidence bands

full_overall_weighted <- ggplot(calibration_summary_full, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, ymin = lower_ci, ymax = upper_ci)) +
  see::geom_point2(stroke = 4) +
  geom_line() + 
  geom_errorbar() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12)


# Now we need to do it for different age groups

# Calculate overall mean and confidence intervals for each bin

resample_calibration_full_age <- cals_full |>
  group_by(id, bin, Age_Group) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')


calibration_summary_full_age <- resample_calibration_full_age |>
  group_by(bin,Age_Group) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

full_age_weighted <- ggplot(calibration_summary_full_age, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = Age_Group, colour = Age_Group)) +
  see::geom_point2(stroke = 4) +
  geom_line() + 
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(Age_Group)) + 
  ggokabeito::scale_colour_okabe_ito() +
  ggokabeito::scale_fill_okabe_ito() +
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "top")


# Now we need to do it for sexes

# Calculate overall mean and confidence intervals for each bin

resample_calibration_full_sex <- cals_full |>
  group_by(id, bin, Sex) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')

calibration_summary_full_sex <- resample_calibration_full_sex |>
  group_by(bin,Sex) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

full_sex_weighted <- ggplot(calibration_summary_full_sex, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = Sex, colour = Sex)) +
  see::geom_point2(stroke = 4) +
  geom_line() + 
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(Sex)) + 
  ggokabeito::scale_colour_okabe_ito() +
  ggokabeito::scale_fill_okabe_ito() +
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "top")


# Now we do it for ITA and No ITA

resample_calibration_full_ita <- cals_full |>
  group_by(id, bin, ITA_Indicator) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')


calibration_summary_full_ita <- resample_calibration_full_ita |>
  group_by(bin,ITA_Indicator) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

full_ita_weighted <- ggplot(calibration_summary_full_ita, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = ITA_Indicator, colour = ITA_Indicator)) +
  see::geom_point2(stroke = 4) +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(ITA_Indicator)) + 
  ggokabeito::scale_colour_okabe_ito() +
  ggokabeito::scale_fill_okabe_ito() +
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "top")


# Now we do it for Hospitals

resample_calibration_full_hosp <- cals_full |>
  group_by(id, bin, Hospital) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')


calibration_summary_full_hosp <- resample_calibration_full_hosp |>
  group_by(bin,Hospital) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

full_hosp_weighted <- ggplot(calibration_summary_full_hosp, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = Hospital, colour = Hospital)) +
  see::geom_point2(stroke = 4) +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(Hospital)) + 
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "top")

# No confidence bands for hospitals due to the fold testing we introduce


# For SKS Categories

resample_calibration_full_diagn <- cals_full |>
  group_by(id, bin, SKS_Category) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')


calibration_summary_full_diagn <- resample_calibration_full_diagn |>
  group_by(bin,SKS_Category) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

full_diagn_weighted <- ggplot(calibration_summary_full_diagn, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = SKS_Category, colour = SKS_Category)) +
  see::geom_point2(stroke = 4) +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(SKS_Category),labeller = label_wrap_gen(width = 40)) + 
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft")


######################################
############ TREE-EWS ################
######################################

# Bin the predicted probabilities
cals_xgb <- xgb_fit |>
    collect_predictions() |>
    arrange(.row) |>
    mutate(weights = data$weights) |>
    mutate(bin = cut(.pred_No_Recovery, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE)) |>
    mutate(mort30D = if_else(Status30D == "No_Recovery",1,0))


# Calculate weighted fraction of positives for each resample
resample_calibration_xgb <- cals_xgb |>
  group_by(id, bin) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop'
  )


# Calculate overall mean and confidence intervals for each bin
calibration_summary_xgb <- resample_calibration_xgb |>
  group_by(bin) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

# Overall calibration line

# Plot the calibration curve with confidence bands

xgb_overall_weighted <- ggplot(calibration_summary_xgb, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, ymin = lower_ci, ymax = upper_ci)) +
  see::geom_point2(stroke = 4) +
  geom_line() + 
  geom_errorbar() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12)


# Now we need to do it for different age groups


# Calculate overall mean and confidence intervals for each bin

resample_calibration_xgb_age <- cals_xgb |>
  group_by(id, bin, Age_Group) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')


calibration_summary_xgb_age <- resample_calibration_xgb_age |>
  group_by(bin,Age_Group) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

xgb_age_weighted <- ggplot(calibration_summary_xgb_age, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = Age_Group, colour = Age_Group)) +
  see::geom_point2(stroke = 4) +
  geom_line() + 
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(Age_Group)) + 
  ggokabeito::scale_colour_okabe_ito() +
  ggokabeito::scale_fill_okabe_ito() +
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "top")


# Now we need to do it for sexes

# Calculate overall mean and confidence intervals for each bin

resample_calibration_xgb_sex <- cals_xgb |>
  group_by(id, bin, Sex) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')

calibration_summary_xgb_sex <- resample_calibration_xgb_sex |>
  group_by(bin,Sex) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

xgb_sex_weighted <- ggplot(calibration_summary_xgb_sex, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = Sex, colour = Sex)) +
  see::geom_point2(stroke = 4) +
  geom_line() + 
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(Sex)) + 
  ggokabeito::scale_colour_okabe_ito() +
  ggokabeito::scale_fill_okabe_ito() +
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "top")


# Now we do it for ITA and No ITA

resample_calibration_xgb_ita <- cals_xgb |>
  group_by(id, bin, ITA_Indicator) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')


calibration_summary_xgb_ita <- resample_calibration_xgb_ita |>
  group_by(bin,ITA_Indicator) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

xgb_ita_weighted <- ggplot(calibration_summary_xgb_ita, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = ITA_Indicator, colour = ITA_Indicator)) +
  see::geom_point2(stroke = 4) +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(ITA_Indicator)) + 
  ggokabeito::scale_colour_okabe_ito() +
  ggokabeito::scale_fill_okabe_ito() +
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "top")


# Now we do it for Hospitals

resample_calibration_xgb_hosp <- cals_xgb |>
  group_by(id, bin, Hospital) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')


calibration_summary_xgb_hosp <- resample_calibration_xgb_hosp |>
  group_by(bin,Hospital) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

xgb_hosp_weighted <- ggplot(calibration_summary_xgb_hosp, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = Hospital, colour = Hospital)) +
  see::geom_point2(stroke = 4) +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(Hospital)) + 
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "top")

# No confidence bands for hospitals due to the fold testing we introduce

# For SKS Categories

resample_calibration_xgb_diagn <- cals_xgb |>
  group_by(id, bin, SKS_Category) |>
  summarize(
    mean_pred_prob = mean(.pred_No_Recovery),
    weighted_frac_pos = sum(weights * mort30D) / sum(weights),
    .groups = 'drop')


calibration_summary_xgb_diagn <- resample_calibration_xgb_diagn |>
  group_by(bin,SKS_Category) |>
  summarize(
    mean_pred_prob = mean(mean_pred_prob),
    weighted_frac_pos_mean = mean(weighted_frac_pos),
    lower_ci = quantile(weighted_frac_pos, probs = 0.025),
    upper_ci = quantile(weighted_frac_pos, probs = 0.975),
    .groups = 'drop'
  )

xgb_diagn_weighted <- ggplot(calibration_summary_xgb_diagn, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, fill = SKS_Category, colour = SKS_Category)) +
  see::geom_point2(stroke = 4) +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(vars(SKS_Category),labeller = label_wrap_gen(width = 40)) + 
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft")