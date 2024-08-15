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

###################################
############ First step ###########
###################################

# 1. Restrict the data to people who have not received interventions
#    Fit models to that dataset
#    Use IPW to compute weights for the probability of interventions
#    Extract the predictions of the models from that dataset
#    Compute the weighted performance metrics


data <- read_parquet("df_august.parquet")

data |> janitor::tabyl(Interventions)

data <- data |> filter(Interventions == "No_Intervention")


# We need to create all our models now


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
           mort30D = if_else(Status30D == "Deceased",1,0)) |>
    group_by(id) |>
    summarise(AUC_Weighted = MetricsWeighted::AUC(mort30D, .pred_Deceased, w = weights),
              AUC_Unweighted = MetricsWeighted::AUC(mort30D, .pred_Deceased),
              Brier_Score_Weighted = MetricsWeighted::mse(mort30D,.pred_Deceased,w = weights),
              Brier_Score_Unweighted = MetricsWeighted::mse(mort30D,.pred_Deceased)) |>
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
           mort30D = if_else(Status30D == "Deceased",1,0)) |>
    group_by(id) |>
    summarise(AUC_Weighted = MetricsWeighted::AUC(mort30D, .pred_Deceased, w = weights),
              AUC_Unweighted = MetricsWeighted::AUC(mort30D, .pred_Deceased),
              Brier_Score_Weighted = MetricsWeighted::mse(mort30D,.pred_Deceased,w = weights),
              Brier_Score_Unweighted = MetricsWeighted::mse(mort30D,.pred_Deceased)) |>
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
           mort30D = if_else(Status30D == "Deceased",1,0)) |>
    group_by(id) |>
    summarise(AUC_Weighted = MetricsWeighted::AUC(mort30D, .pred_Deceased, w = weights),
              AUC_Unweighted = MetricsWeighted::AUC(mort30D, .pred_Deceased),
              Brier_Score_Weighted = MetricsWeighted::mse(mort30D,.pred_Deceased,w = weights),
              Brier_Score_Unweighted = MetricsWeighted::mse(mort30D,.pred_Deceased)) |>
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
           mort30D = if_else(Status30D == "Deceased",1,0)) |>
    group_by(id) |>
    summarise(AUC_Weighted = MetricsWeighted::AUC(mort30D, .pred_Deceased, w = weights),
              AUC_Unweighted = MetricsWeighted::AUC(mort30D, .pred_Deceased),
              Brier_Score_Weighted = MetricsWeighted::mse(mort30D,.pred_Deceased,w = weights),
              Brier_Score_Unweighted = MetricsWeighted::mse(mort30D,.pred_Deceased)) |>
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

# For current NEWS2 model

# Bin the predicted probabilities
cals_current <- current_fit |>
    collect_predictions() |>
    arrange(.row) |>
    mutate(weights = data$weights) |>
    mutate(bin = cut(.pred_Deceased, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE)) |>
    mutate(mort30D = if_else(Status30D == "Deceased",1,0))


# Calculate weighted fraction of positives for each resample
resample_calibration_current <- cals_current |>
  group_by(id, bin) |>
  summarize(
    mean_pred_prob = mean(.pred_Deceased),
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
    mean_pred_prob = mean(.pred_Deceased),
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
    mean_pred_prob = mean(.pred_Deceased),
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
    mean_pred_prob = mean(.pred_Deceased),
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
    mean_pred_prob = mean(.pred_Deceased),
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
    mean_pred_prob = mean(.pred_Deceased),
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
  facet_wrap(vars(SKS_Category)) + 
  labs(x = "Mean predicted probability", y = "Weighted fraction of mortality cases") +
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft")


#################################################
############ Decision Curve Analysis ############
#################################################

# Define your thresholds
dca_thresholds <- seq(0.01, 0.60, by = 0.01)

# Function to calculate weighted Net Benefit (unchanged)
calculate_weighted_nb <- function(data, pred_col, outcome_col, weight_col, threshold) {
  weighted_data <- data |>
    mutate(
      decision = !!sym(pred_col) >= threshold,
      TP = decision & !!sym(outcome_col) == 1,
      FP = decision & !!sym(outcome_col) == 0
    )
  
  total_weight <- sum(weighted_data[[weight_col]])
  weighted_TP <- sum(weighted_data$TP * weighted_data[[weight_col]])
  weighted_FP <- sum(weighted_data$FP * weighted_data[[weight_col]])
  
  nb <- (weighted_TP - (threshold / (1 - threshold)) * weighted_FP) / total_weight
  return(nb)
}

# Updated function to perform weighted DCA for multiple models
weighted_dca_multi <- function(data, pred_cols, outcome_col, weight_col, thresholds, labels) {
  stopifnot(length(pred_cols) == length(labels))
  
  results <- map2_dfr(pred_cols, labels, function(pred_col, label) {
    tibble(
      threshold = thresholds,
      NB = map_dbl(thresholds, ~calculate_weighted_nb(data, pred_col, outcome_col, weight_col, .x)),
      model = label
    )
  })
  
  # Calculate weighted prevalence
  weighted_prevalence <- sum(data[[weight_col]] * data[[outcome_col]]) / sum(data[[weight_col]])
  
  # Add "Treat All" and "Treat None" strategies
  results <- results |>
    bind_rows(
      tibble(
        threshold = thresholds,
        NB = pmax(0, weighted_prevalence - thresholds * (1 - weighted_prevalence)),
        model = "All"
      ),
      tibble(
        threshold = thresholds,
        NB = 0,
        model = "None"
      )
    )
  
  return(results)
}

# Pulling the estimates together

preds_xgb <- xgb_fit |> 
    collect_predictions() |>
    arrange(.row) |>
    mutate(weights = data$weights,
           mort30D = if_else(Status30D == "Deceased",1,0)) |>
    pull(.pred_Deceased)

preds_light <- light_fit |> 
    collect_predictions() |>
    arrange(.row) |>
    mutate(weights = data$weights,
           mort30D = if_else(Status30D == "Deceased",1,0)) |>
    pull(.pred_Deceased)

preds_full <- full_fit |> 
    collect_predictions() |>
    arrange(.row) |>
    mutate(weights = data$weights,
           mort30D = if_else(Status30D == "Deceased",1,0)) |>
    pull(.pred_Deceased)

cals_all <- cals_current |> mutate(preds_xgb = preds_xgb,preds_light = preds_light,preds_full = preds_full)

pred_cols <- c(".pred_Deceased", "preds_light","preds_full", "preds_xgb")

model_labels <- c("NEWS2", "NEWS2-Light", "IEWS", "TREE-EWS")

weighted_dca_results <- weighted_dca_multi(
  data = cals_all,
  pred_cols = pred_cols,
  outcome_col = "mort30D",
  weight_col = "weights",
  thresholds = dca_thresholds,
  labels = model_labels
)

# Plot the results with multiple models
ggplot(weighted_dca_results, aes(x = threshold, y = NB, color = model)) +
  geom_smooth(se = F) +
  labs(x = "Threshold Probability", y = "Net Benefit") + 
  theme_gray(base_size = 12) + 
  scale_y_continuous(labels = scales::label_number(scale = 1000)) +
  theme(legend.position = "top")


###################################################################
############# Now DCA for different age groups ####################
###################################################################

# Youngest group

weighted_dca_results_youngest <- weighted_dca_multi(
  data = cals_all |> filter(Age_Group == "18-65"),
  pred_cols = pred_cols,
  outcome_col = "mort30D",
  weight_col = "weights",
  thresholds = dca_thresholds,
  labels = model_labels
)

# Plot the results with multiple models
ggplot(weighted_dca_results_youngest, aes(x = threshold, y = NB, color = model)) +
  geom_smooth(se = F) +
  labs(x = "Threshold Probability", y = "Net Benefit") + 
  theme_gray(base_size = 12) + 
  scale_y_continuous(labels = scales::label_number(scale = 1000)) +
  theme(legend.position = "top")

# Middle group

weighted_dca_results_middle <- weighted_dca_multi(
  data = cals_all |> filter(Age_Group == "66-80"),
  pred_cols = pred_cols,
  outcome_col = "mort30D",
  weight_col = "weights",
  thresholds = dca_thresholds,
  labels = model_labels
)

# Plot the results with multiple models
ggplot(weighted_dca_results_middle, aes(x = threshold, y = NB, color = model)) +
  geom_smooth(se = F) +
  labs(x = "Threshold Probability", y = "Net Benefit") + 
  theme_gray(base_size = 12) + 
  scale_y_continuous(labels = scales::label_number(scale = 1000)) +
  theme(legend.position = "top")

# Oldest group

weighted_dca_results_oldest <- weighted_dca_multi(
  data = cals_all |> filter(Age_Group == "80+"),
  pred_cols = pred_cols,
  outcome_col = "mort30D",
  weight_col = "weights",
  thresholds = dca_thresholds,
  labels = model_labels
)

# Plot the results with multiple models
ggplot(weighted_dca_results_oldest, aes(x = threshold, y = NB, color = model)) +
  geom_smooth(se = F) +
  labs(x = "Threshold Probability", y = "Net Benefit") + 
  theme_gray(base_size = 12) + 
  scale_y_continuous(labels = scales::label_number(scale = 1000)) +
  theme(legend.position = "top")


#######################
########  Sex #########
#######################

# Males

weighted_dca_results_males <- weighted_dca_multi(
  data = cals_all |> filter(Sex == "Male"),
  pred_cols = pred_cols,
  outcome_col = "mort30D",
  weight_col = "weights",
  thresholds = dca_thresholds,
  labels = model_labels
)

# Plot the results with multiple models
ggplot(weighted_dca_results_males, aes(x = threshold, y = NB, color = model)) +
  geom_smooth(se = F) +
  labs(x = "Threshold Probability", y = "Net Benefit") + 
  theme_gray(base_size = 12) + 
  scale_y_continuous(labels = scales::label_number(scale = 1000)) +
  theme(legend.position = "top")

# Females

weighted_dca_results_females <- weighted_dca_multi(
  data = cals_all |> filter(Sex == "Female"),
  pred_cols = pred_cols,
  outcome_col = "mort30D",
  weight_col = "weights",
  thresholds = dca_thresholds,
  labels = model_labels
)

# Plot the results with multiple models
ggplot(weighted_dca_results_females, aes(x = threshold, y = NB, color = model)) +
  geom_smooth(se = F) +
  labs(x = "Threshold Probability", y = "Net Benefit") + 
  theme_gray(base_size = 12) + 
  scale_y_continuous(labels = scales::label_number(scale = 1000)) +
  theme(legend.position = "top")


#################################
########## Second step ##########
#################################

# 2. Using the TMLE framework to fix the non-correspondance between the dataset we fit our models on and the target population
#    Create a clever covariate to add to the models

# Extract the predictions from the current fit

current_df <- current_fit |>
  collect_predictions() |>
  arrange(.row) |>
  mutate(weights = data$weights,
        mort30D = if_else(Status30D == "Deceased",1,0)) |>
  mutate(bin = cut(.pred_Deceased, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE))

# Creation of the epsilon (fluctuation parameter to recalibrate the model)

epsilon <- coef(glm(mort30D ~ offset(qlogis(.pred_Deceased)), weights= weights, family=binomial, data = current_df))


current_df <- current_df |>
  mutate(.pred_current = plogis(qlogis(.pred_Deceased) + epsilon)) |>
  relocate(.pred_current,.after = .pred_Deceased)

current_tmle <- current_df |>
    group_by(id) |>
    summarise(AUC_Weighted = MetricsWeighted::AUC(mort30D, .pred_current, w = weights),
              AUC_Unweighted = MetricsWeighted::AUC(mort30D, .pred_current),
              Brier_Score_Weighted = MetricsWeighted::mse(mort30D,.pred_current,w = weights),
              Brier_Score_Unweighted = MetricsWeighted::mse(mort30D,.pred_current)) |>
    ungroup() |>
    summarise(Mean_AUC = mean(AUC_Weighted), Mean_Brier = mean(Brier_Score_Weighted), SD_AUC = sd(AUC_Weighted), SD_Brier = sd(Brier_Score_Weighted)) |>
    mutate(Lower_AUC = Mean_AUC - 1.96*(SD_AUC/sqrt(10)), Upper_AUC = Mean_AUC + 1.96*(SD_AUC/sqrt(10)), Lower_Brier = Mean_Brier - 1.96*(SD_Brier/sqrt(10)), Upper_Brier = Mean_Brier + 1.96*(SD_Brier/sqrt(10))) |>
    relocate(Lower_AUC,.after = Mean_AUC) |>
    relocate(Upper_AUC, .after = Lower_AUC) |>
    relocate(Lower_Brier, .after = Mean_Brier) |>
    relocate(Upper_Brier, .after = Lower_Brier)


# Do it for the light model

light_df <- light_fit |>
  collect_predictions() |>
  arrange(.row) |>
  mutate(weights = data$weights,
        mort30D = if_else(Status30D == "Deceased",1,0)) |>
  mutate(bin = cut(.pred_Deceased, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE))


# Creation of the epsilon (fluctuation parameter to recalibrate the model)

epsilon_light <- coef(glm(mort30D ~ offset(qlogis(.pred_Deceased)), weights= weights, family=binomial, data = light_df))


light_df <- light_df |>
  mutate(.pred_current = plogis(qlogis(.pred_Deceased) + epsilon_light)) |>
  relocate(.pred_current,.after = .pred_Deceased)


light_tmle <- light_df |>
    group_by(id) |>
    summarise(AUC_Weighted = MetricsWeighted::AUC(mort30D, .pred_current, w = weights),
              AUC_Unweighted = MetricsWeighted::AUC(mort30D, .pred_current),
              Brier_Score_Weighted = MetricsWeighted::mse(mort30D,.pred_current,w = weights),
              Brier_Score_Unweighted = MetricsWeighted::mse(mort30D,.pred_current)) |>
    ungroup() |>
    summarise(Mean_AUC = mean(AUC_Weighted), Mean_Brier = mean(Brier_Score_Weighted), SD_AUC = sd(AUC_Weighted), SD_Brier = sd(Brier_Score_Weighted)) |>
    mutate(Lower_AUC = Mean_AUC - 1.96*(SD_AUC/sqrt(10)), Upper_AUC = Mean_AUC + 1.96*(SD_AUC/sqrt(10)), Lower_Brier = Mean_Brier - 1.96*(SD_Brier/sqrt(10)), Upper_Brier = Mean_Brier + 1.96*(SD_Brier/sqrt(10))) |>
    relocate(Lower_AUC,.after = Mean_AUC) |>
    relocate(Upper_AUC, .after = Lower_AUC) |>
    relocate(Lower_Brier, .after = Mean_Brier) |>
    relocate(Upper_Brier, .after = Lower_Brier)


# Do it for the IEWS model

full_df <- full_fit |>
  collect_predictions() |>
  arrange(.row) |>
  mutate(weights = data$weights,
        mort30D = if_else(Status30D == "Deceased",1,0)) |>
  mutate(bin = cut(.pred_Deceased, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE))


# Creation of the epsilon (fluctuation parameter to recalibrate the model)

epsilon_full <- coef(glm(mort30D ~ offset(qlogis(.pred_Deceased)), weights= weights, family=binomial, data = full_df))


full_df <- full_df |>
  mutate(.pred_current = plogis(qlogis(.pred_Deceased) + epsilon_full)) |>
  relocate(.pred_current,.after = .pred_Deceased)


full_tmle <- full_df |>
    group_by(id) |>
    summarise(AUC_Weighted = MetricsWeighted::AUC(mort30D, .pred_current, w = weights),
              AUC_Unweighted = MetricsWeighted::AUC(mort30D, .pred_current),
              Brier_Score_Weighted = MetricsWeighted::mse(mort30D,.pred_current,w = weights),
              Brier_Score_Unweighted = MetricsWeighted::mse(mort30D,.pred_current)) |>
    ungroup() |>
    summarise(Mean_AUC = mean(AUC_Weighted), Mean_Brier = mean(Brier_Score_Weighted), SD_AUC = sd(AUC_Weighted), SD_Brier = sd(Brier_Score_Weighted)) |>
    mutate(Lower_AUC = Mean_AUC - 1.96*(SD_AUC/sqrt(10)), Upper_AUC = Mean_AUC + 1.96*(SD_AUC/sqrt(10)), Lower_Brier = Mean_Brier - 1.96*(SD_Brier/sqrt(10)), Upper_Brier = Mean_Brier + 1.96*(SD_Brier/sqrt(10))) |>
    relocate(Lower_AUC,.after = Mean_AUC) |>
    relocate(Upper_AUC, .after = Lower_AUC) |>
    relocate(Lower_Brier, .after = Mean_Brier) |>
    relocate(Upper_Brier, .after = Lower_Brier)



# Do it for the xgb model

xgb_df <- xgb_fit |>
  collect_predictions() |>
  arrange(.row) |>
  mutate(weights = data$weights,
        mort30D = if_else(Status30D == "Deceased",1,0)) |>
  mutate(bin = cut(.pred_Deceased, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE))


# Creation of the epsilon (fluctuation parameter to recalibrate the model)

epsilon_xgb <- coef(glm(mort30D ~ offset(qlogis(.pred_Deceased)), weights= weights, family=binomial, data = xgb_df))


xgb_df <- xgb_df |>
  mutate(.pred_current = plogis(qlogis(.pred_Deceased) + epsilon_xgb)) |>
  relocate(.pred_current,.after = .pred_Deceased)

xgb_tmle <- xgb_df |>
    group_by(id) |>
    summarise(AUC_Weighted = MetricsWeighted::AUC(mort30D, .pred_current, w = weights),
              AUC_Unweighted = MetricsWeighted::AUC(mort30D, .pred_current),
              Brier_Score_Weighted = MetricsWeighted::mse(mort30D,.pred_current,w = weights),
              Brier_Score_Unweighted = MetricsWeighted::mse(mort30D,.pred_current)) |>
    ungroup() |>
    summarise(Mean_AUC = mean(AUC_Weighted), Mean_Brier = mean(Brier_Score_Weighted), SD_AUC = sd(AUC_Weighted), SD_Brier = sd(Brier_Score_Weighted)) |>
    mutate(Lower_AUC = Mean_AUC - 1.96*(SD_AUC/sqrt(10)), Upper_AUC = Mean_AUC + 1.96*(SD_AUC/sqrt(10)), Lower_Brier = Mean_Brier - 1.96*(SD_Brier/sqrt(10)), Upper_Brier = Mean_Brier + 1.96*(SD_Brier/sqrt(10))) |>
    relocate(Lower_AUC,.after = Mean_AUC) |>
    relocate(Upper_AUC, .after = Lower_AUC) |>
    relocate(Lower_Brier, .after = Mean_Brier) |>
    relocate(Upper_Brier, .after = Lower_Brier)


# Save the metrics

weighted_metrics_final <- bind_rows(current_tmle,light_tmle,full_tmle,xgb_tmle) |>
  mutate(Model = c("NEWS2-Modified", "NEWS2-Light-Modified", "IEWS-Modified", "TREE-EWS-Modified")) |>
  relocate(Model, .before = Mean_AUC) |>
  bind_rows(weighted_metrics) 

