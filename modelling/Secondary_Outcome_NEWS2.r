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

# Workflows now

# Current model

current_wf <- workflow() |> 
  add_formula(Status30D ~ Mean_NEWS) |>
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
  cal_plot_windowed(truth = Status30D,estimate = .pred_No_Recovery,include_rug = F) + 
  theme_gray(base_size = 12) + 
  labs(x = "Predictions from NEWS2 model")

cal_current



# Get the calibration curves for the different age groups

cal_current_age <- current_fit |> 
  collect_predictions() |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_No_Recovery,include_rug = F,.by = Age_Group) + 
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") + 
  labs(x = "Predictions from NEWS2 model")


cal_current_age


# Get the calibration curves for the different sexes

cal_current_sex <- current_fit |> 
  collect_predictions() |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_No_Recovery,include_rug = F,.by = Sex) + 
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") + 
  labs(x = "Predictions from NEWS2 model")

cal_current_sex


# Get the calibration curves for people receiving interventions and those who have not

cal_current_int <- current_fit |> 
  collect_predictions() |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_No_Recovery,include_rug = F,.by = Interventions) + 
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") + 
  labs(x = "Predictions from NEWS2 model")


cal_current_int


# Get the calibration curves for people in the ITA and those not

cal_current_ita <- current_fit |> 
  collect_predictions() |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_No_Recovery,include_rug = F,.by = ITA_Indicator) + 
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") + 
  labs(x = "Predictions from NEWS2 model")


cal_current_ita


# Looking deeper at the ITA category

# For youngsters

cal_current_ita_young <- current_fit |> 
  collect_predictions() |>
  filter(Age_Group == "18-65") |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_No_Recovery,include_rug = F,.by = ITA_Indicator) + 
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") + 
  labs(x = "Predictions from NEWS2 model")


cal_current_ita_young 


# For middle aged

cal_current_ita_middle <- current_fit |> 
  collect_predictions() |>
  filter(Age_Group == "66-80") |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_No_Recovery,include_rug = F,.by = ITA_Indicator) + 
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") + 
  labs(x = "Predictions from NEWS2 model")


cal_current_ita_middle



# Old category

cal_current_ita_old <- current_fit |> 
  collect_predictions() |>
  filter(Age_Group == "80+") |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_No_Recovery,include_rug = F,.by = ITA_Indicator) + 
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") + 
  labs(x = "Predictions from NEWS2 model")


cal_current_ita_old



# Get the calibration curves for the different hospital departments

cal_current_department <- current_fit |> 
  collect_predictions() |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_No_Recovery,include_rug = F,.by = Department_Name_Fac) + 
  theme_gray(base_size = 12) + 
  facet_wrap(vars(Department_Name_Fac),nrow = 5) + 
  theme(legend.position = "topleft") +
  labs(x = "Predictions from NEWS2 model")


cal_current_department


# Get the calibration curves for the different hospitals


cal_current_hosp <- current_fit |> 
  collect_predictions() |>
  cal_plot_windowed(truth = Status30D,estimate = .pred_No_Recovery,include_rug = F,.by = Hospital) + 
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
  cal_plot_windowed(truth = Status30D,estimate = .pred_No_Recovery,include_rug = F,.by = SKS_Category) + 
  theme_gray(base_size = 12) + 
  facet_wrap(vars(SKS_Category),labeller = label_wrap_gen(width = 20)) + 
  theme(legend.position = "topleft") +
  labs(x = "Predictions from NEWS2 model")


cal_current_sks
