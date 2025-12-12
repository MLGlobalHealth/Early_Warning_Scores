# =====================================
# STEP 0: SETUP & DATA RECOVERY
# =====================================

# Note: NEWS-Light was renamed to "Simplified NEWS" in the manuscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(lubridate)
library(doParallel)
library(gt)

# 1. Load the ICU Data (to identify strict admissions)
setwd("/home/alex/ews/NEWS2_Evaluation/Additional_Data")

ita_data <- read_parquet("ita_latest_v2.parquet") %>%
  select(PT_ID, ITA_start) %>%
  filter(!is.na(ITA_start))

# 2. Load the FULL Cohort (Before Exclusions)
setwd("/home/alex/ews/NEWS2_Evaluation/Modelling")

data <- nanoparquet::read_parquet(
  "/home/alex/ews/NEWS2_Evaluation/Additional_Data/df_with_embeddings_v2.parquet",
)

# Polishing up the data

data$Hospital <- as.factor(data$Hospital)

levels(data$Hospital)[3] <- "Hospital of Bornholm"

levels(data$Hospital)[10] <- "Zealand University Hospital"

data$Status24H <- as.factor(data$Status24H)

data$Status24H <- relevel(data$Status24H, "Deceased")

data <- data |>
  relocate(Hospital, .after = CSN) |>
  relocate(Department_Name, .after = Hospital)

#############################################

data$Department_Name <- as.factor(data$Department_Name)

data |> count(Department_Name, sort = T)

levels(data$Department_Name)


######## Create a more refined mapping of specialties to broader categories ###########
specialty_categories <- list(
  "Surgical" = c(
    "Brystkirurgi",
    "Gynækologi",
    "Gynækologi-obstetrik",
    "Karkirurgi",
    "Kirurgi",
    "Neurokirurgi",
    "Ortopædkirurgi",
    "Plastikkirurgi",
    "Tand- mund- og kæbekirurgi",
    "Thoraxkirurgi",
    "Urologi",
    "Øre-næse-hals"
  ),

  "Cardiopulmonary" = c("Kardiologi", "Lungesygdomme"),

  "Gastroenterology" = c("Medicinsk gastroenterologi", "Hepatologi"),

  "Neurology" = c("Neurologi"),

  "Oncology & Hematology" = c(
    "Onkologi",
    "Hæmatologi",
    "Hæmatologi-onkologi pædiatrisk"
  ),

  "Endocrinology & Metabolism" = c("Endokrinologi"),

  "Nephrology" = c("Nefrologi"),

  "Infectious Disease" = c("Infektionsmedicin"),

  "Dermatology" = c("Dermatologi"),

  "Rheumatology" = c("Reumatologi"),

  "Geriatrics & Palliative" = c("Geriatri", "Palliation"),

  "Pediatrics" = c("Pædiatri"),

  "Internal & Acute Medicine" = c("Intern medicin", "Akutmedicin"),

  "Critical Care" = c("Anæstesiologi", "Intensiv terapi"),

  "Psychiatry" = c("Psykiatri"),

  "Rehabilitation" = c("Rehabilitering"),

  "Ophthalmology" = c("Oftalmologi"),

  "Other" = c("Ikke-klinisk speciale", "NULL")
)

# Function to categorize a specialty
get_category <- function(specialty) {
  for (category in names(specialty_categories)) {
    if (specialty %in% specialty_categories[[category]]) {
      return(category)
    }
  }
  return("Uncategorized")
}

# Apply the categorization to your dataframe
data <- data %>%
  mutate(Specialty_Category = sapply(Department_Name, get_category))

# Convert to factor with a logical order
data <- data %>%
  mutate(
    Specialty_Category = factor(
      Specialty_Category,
      levels = c(
        "Internal & Acute Medicine",
        "Cardiopulmonary",
        "Gastroenterology",
        "Neurology",
        "Oncology & Hematology",
        "Endocrinology & Metabolism",
        "Nephrology",
        "Infectious Disease",
        "Dermatology",
        "Rheumatology",
        "Geriatrics & Palliative",
        "Pediatrics",
        "Critical Care",
        "Surgical",
        "Psychiatry",
        "Rehabilitation",
        "Ophthalmology",
        "Other"
      )
    )
  )

data <- data |>
  mutate(
    Department_Name_Fac = if_else(
      is.na(Specialty_Category),
      "Other",
      Specialty_Category
    )
  ) |>
  mutate_at(vars(Department_Name_Fac), as.factor) |>
  select(-Specialty_Category)


# Lumping the 10 most frequent departments

data <- data %>%
  mutate(Department_Name_Fac = fct_lump_n(Department_Name_Fac, n = 9))

# Check the result
data %>% count(Department_Name_Fac, sort = TRUE)

# Grab all the pca columns
pca_columns <- grep("^pc", names(data), value = TRUE)

my_formula_pca <- reformulate(termlabels = pca_columns)

# Grab all the blood tests
median_columns <- grep("^median", names(data), value = TRUE)

my_formula_med <- reformulate(termlabels = median_columns)

# Create a composite variable (average of all PCA columns)
data <- data |>
  mutate(pca_composite = rowMeans(across(all_of(pca_columns)), na.rm = TRUE))

# Factorize variable regarding previous icu and respiratory
data <- data |>
  mutate(
    previous_icu_respiratory = if_else(
      previous_icu_respiratory == 0,
      "No history of ICU or respiratory support",
      "Previous history of ICU or respiratory support"
    )
  ) |>
  mutate_at(vars(previous_icu_respiratory), as.factor)

data <- data |>
  mutate_at(
    vars(Sex, Age_Group, Hospital, Oxygen_Supplement, Consciousness),
    as.factor
  )

data <- data |>
  mutate(
    day_type = if_else(wday(recorded_time) %in% 2:6, "Weekday", "Weekend")
  ) |>
  mutate(hour = hour(recorded_time)) |>
  mutate(
    time_of_day = case_when(
      hour >= 6 & hour < 12 ~ "Morning",
      hour >= 12 & hour < 22 ~ "Afternoon/Evening",
      TRUE ~ "Night"
    )
  ) |>
  mutate(month = month(recorded_time, label = TRUE, abbr = TRUE)) |>
  select(-hour) |>
  mutate_at(vars(day_type, time_of_day), as.factor)


# Create the Composite Outcome Variable

# Find the specific Identifiers that had an ICU event
valid_icu_events <- data %>%
  select(Identifier, PT_ID, recorded_time) %>% # Keep only what we need
  inner_join(ita_data, by = "PT_ID", relationship = "many-to-many") %>%
  mutate(cutoff_time = recorded_time + hours(24)) %>%
  filter(
    ITA_start >= recorded_time,
    ITA_start <= cutoff_time
  ) %>%
  pull(Identifier) %>% # Extract the vector of IDs
  unique()

# Update the Main Dataset
data_composite <- data %>%
  mutate(
    # Check membership: Is this specific assessment in our list of events?
    icu_admit_24h = if_else(Identifier %in% valid_icu_events, 1, 0),

    # Create Composite Outcome
    composite_outcome_num = if_else(icu_admit_24h == 1 | mort24H == 1, 1, 0),
    StatusComposite = if_else(composite_outcome_num == 1, "Event", "None") %>%
      factor(levels = c("Event", "None"))
  )

# ==============================
# DEFINE WORKFLOWS (UNWEIGHTED)
# ==============================

# Define Folds on the full dataset
set.seed(234)
folds_comp <- rsample::group_vfold_cv(data_composite, group = Hospital)

# Base Model Specs
model_glm <- logistic_reg(engine = "glm", mode = "classification")
model_xgb <- boost_tree(trees = 100, mtry = 30) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# 1. NEWS
current_wf_comp <- workflow() %>%
  add_formula(StatusComposite ~ EWS_score) %>%
  add_model(model_glm)

# 2. Simplified NEWS
light_wf_comp <- workflow() %>%
  add_formula(StatusComposite ~ EWS_light) %>%
  add_model(model_glm)

# 3. DEWS
full_wf_comp <- workflow() %>%
  add_formula(StatusComposite ~ IEWS_Light) %>%
  add_model(model_glm)

# 4. XGB-EWS (Same predictors as before)
xgb_wf_comp <- workflow() %>%
  add_formula(
    StatusComposite ~ Age +
      Sex +
      Respiration_Rate +
      Temperature +
      Pulse +
      Saturation +
      Oxygen_Supplement +
      Blood_Pressure.Sys +
      Blood_Pressure.Dia +
      Consciousness +
      Previous_Hosp +
      day_type +
      time_of_day +
      month +
      previous_icu_respiratory +
      median_Hemoglobin +
      median_Leukocytter +
      median_Trombocytter +
      median_Kreatinin +
      median_ALAT +
      median_LDH +
      median_Albumin +
      median_CRP +
      median_Laktat_ab +
      median_Troponin_T +
      median_Laktat_vb +
      pca_0 +
      pca_1 +
      pca_2 +
      pca_3 +
      pca_4 +
      pca_5 +
      pca_6 +
      pca_7 +
      pca_8 +
      pca_9 +
      pca_10 +
      pca_11 +
      pca_12 +
      pca_13 +
      pca_14 +
      pca_15 +
      pca_16 +
      pca_17 +
      pca_18 +
      pca_19 +
      pca_20 +
      pca_21 +
      pca_22 +
      pca_23 +
      pca_24 +
      pca_25 +
      pca_26 +
      pca_27 +
      pca_28 +
      pca_29 +
      pca_30 +
      pca_31 +
      pca_32 +
      pca_33 +
      pca_34 +
      pca_35 +
      pca_36 +
      pca_37 +
      pca_38 +
      pca_39 +
      pca_40 +
      pca_41 +
      pca_42 +
      pca_43 +
      pca_44 +
      pca_45 +
      pca_46 +
      pca_47 +
      pca_48 +
      pca_49 +
      pca_50 +
      pca_51 +
      pca_52 +
      pca_53 +
      pca_54 +
      pca_55 +
      pca_56 +
      pca_57 +
      pca_58 +
      pca_59
  ) %>%
  add_model(model_xgb)

# =================
# FIT MODELS
# =================

doParallel::registerDoParallel(cores = 40)
cntrl <- control_resamples(save_pred = TRUE)


current_fit_comp <- fit_resamples(
  current_wf_comp,
  resamples = folds_comp,
  metrics = metric_set(roc_auc, brier_class),
  control = cntrl
)
light_fit_comp <- fit_resamples(
  light_wf_comp,
  resamples = folds_comp,
  metrics = metric_set(roc_auc, brier_class),
  control = cntrl
)
full_fit_comp <- fit_resamples(
  full_wf_comp,
  resamples = folds_comp,
  metrics = metric_set(roc_auc, brier_class),
  control = cntrl
)
set.seed(234)
xgb_fit_comp <- fit_resamples(
  xgb_wf_comp,
  resamples = folds_comp,
  metrics = metric_set(roc_auc, brier_class),
  control = cntrl
)

# ==============================================================================
# CALCULATE NET BENEFIT (UNWEIGHTED)
# ==============================================================================

# 1. Extract Predictions & Align
# Since we fit on 'data_composite', we extract directly back to it
data_composite <- data_composite %>%
  mutate(
    pred_current = collect_predictions(current_fit_comp) %>%
      arrange(.row) %>%
      pull(.pred_Event),
    pred_light = collect_predictions(light_fit_comp) %>%
      arrange(.row) %>%
      pull(.pred_Event),
    pred_full = collect_predictions(full_fit_comp) %>%
      arrange(.row) %>%
      pull(.pred_Event),
    pred_xgb = collect_predictions(xgb_fit_comp) %>%
      arrange(.row) %>%
      pull(.pred_Event)
  )

# Calculate Standard (Unweighted) Prevalence
prev_comp <- mean(data_composite$composite_outcome_num)
cut_points_comp <- prev_comp * c(1, 2, 4, 8, 10, 20, 30)

# Helper Function: Standard Unweighted Net Benefit
calculate_nb_unweighted <- function(data, estimate, truth, thresholds) {
  estimate_col <- enquo(estimate)
  truth_col <- enquo(truth)

  df <- data %>%
    select(!!estimate_col, !!truth_col) %>%
    mutate(truth_numeric = if_else(!!truth_col == "Event", 1, 0))

  N <- nrow(df)

  map_dfr(
    thresholds,
    ~ {
      pt <- .x
      stats <- df %>%
        summarise(
          TP = sum((!!estimate_col >= pt) & (truth_numeric == 1)),
          FP = sum((!!estimate_col >= pt) & (truth_numeric == 0))
        )

      net_benefit <- (stats$TP / N) - (stats$FP / N) * (pt / (1 - pt))
      tibble(threshold = pt, net_benefit = net_benefit)
    }
  )
}

# Compute NB
nb_comp_current <- calculate_nb_unweighted(
  data_composite,
  pred_current,
  StatusComposite,
  cut_points_comp
) %>%
  mutate(model = "NEWS")
nb_comp_light <- calculate_nb_unweighted(
  data_composite,
  pred_light,
  StatusComposite,
  cut_points_comp
) %>%
  mutate(model = "Simplified NEWS")
nb_comp_full <- calculate_nb_unweighted(
  data_composite,
  pred_full,
  StatusComposite,
  cut_points_comp
) %>%
  mutate(model = "DEWS")
nb_comp_xgb <- calculate_nb_unweighted(
  data_composite,
  pred_xgb,
  StatusComposite,
  cut_points_comp
) %>%
  mutate(model = "XGB-EWS")

# Treat All / None
nb_comp_all <- tibble(
  threshold = cut_points_comp,
  net_benefit = prev_comp -
    (1 - prev_comp) * (cut_points_comp / (1 - cut_points_comp)),
  model = "Treat All"
)
nb_comp_none <- tibble(
  threshold = cut_points_comp,
  net_benefit = 0,
  model = "Treat None"
)

# ==============================================================================
# FORMAT TABLE
# ==============================================================================

comparison_data_comp <- bind_rows(
  nb_comp_current,
  nb_comp_light,
  nb_comp_full,
  nb_comp_xgb,
  nb_comp_all,
  nb_comp_none
) %>%
  select(threshold, model, net_benefit) %>%
  pivot_wider(names_from = model, values_from = net_benefit) %>%

  # Scale by 10,000
  mutate(across(where(is.numeric) & !matches("threshold"), ~ .x * 10000)) %>%
  mutate(across(
    .cols = any_of(c(
      "NEWS",
      "Simplified NEWS",
      "XGB-EWS",
      "DEWS",
      "Treat All",
      "Treat None"
    )),
    ~ round(.x, digits = 1)
  )) %>%

  # Calculate Differences
  mutate(
    difference1 = NEWS - `XGB-EWS`,
    difference2 = NEWS - `Simplified NEWS`,
    difference3 = DEWS - `XGB-EWS`,
    difference4 = NEWS - DEWS
  ) %>%
  select(
    threshold,
    `Treat All`,
    `Treat None`,
    NEWS,
    `Simplified NEWS`,
    DEWS,
    `XGB-EWS`,
    difference1,
    difference2,
    difference3,
    difference4
  ) %>%
  arrange(threshold)

# Render
final_table_comp <- comparison_data_comp %>%
  gt() %>%
  tab_header(
    title = "Decision Curve Analysis: Composite Outcome (ICU + Death)",
    subtitle = "Unweighted Analysis (24 Hours). Values are per 10,000 patients."
  ) %>%
  cols_label(
    threshold = "Risk Threshold",
    `Treat All` = "Treat All",
    `Treat None` = "Treat None",
    NEWS = "NEWS",
    `Simplified NEWS` = "Simplified NEWS",
    DEWS = "DEWS",
    `XGB-EWS` = "XGB-EWS",
    difference1 = "NEWS vs XGB-EWS",
    difference2 = "NEWS vs Simplified NEWS",
    difference3 = "DEWS vs XGB-EWS",
    difference4 = "NEWS vs DEWS"
  ) %>%
  fmt_number(
    columns = threshold,
    decimals = 4,
    dec_mark = "·",
    sep_mark = " "
  ) %>%
  fmt_number(
    columns = where(is.numeric) & !matches("threshold"),
    decimals = 1,
    dec_mark = "·",
    sep_mark = " "
  ) %>%
  tab_spanner(
    label = "Net Benefit per 10 000 Patients",
    columns = c(
      `Treat All`,
      `Treat None`,
      NEWS,
      `Simplified NEWS`,
      DEWS,
      `XGB-EWS`
    )
  ) %>%
  tab_spanner(
    label = "Net Benefit Difference per 10 000 Patients",
    columns = c(difference1, difference2, difference3, difference4)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = c(difference1, difference2, difference3, difference4)
    )
  ) %>%
  tab_footnote(
    footnote = "Positive values indicate the first model has higher net benefit; negative values indicate the comparator is better.",
    locations = cells_column_spanners(spanners = contains("Difference"))
  )

final_table_comp

final_table_comp |> gtsave("final_table_composite_nov_v2.docx")


# ==============================================================================
# DEFINE THE BOOTSTRAP FUNCTION (Manual Loop, Unweighted)
# ==============================================================================

calculate_composite_bootstrap_ci <- function(
  fit_object,
  original_data,
  model_label,
  n_bootstrap = 200
) {
  # Extract & Prepare Data
  preds_df <- fit_object %>%
    collect_predictions() %>%
    arrange(.row) %>%
    mutate(
      # Outcome: StatusComposite (Event vs None)
      StatusComposite = original_data$StatusComposite,

      # Binary Flag: 1 = Event (ICU or Death), 0 = None
      outcome_num = if_else(StatusComposite == "Event", 1, 0)
    )

  # Run the Manual Loop (Sequential)
  set.seed(42)

  boot_results <- map_dfr(
    1:n_bootstrap,
    ~ {
      # A. Resample Indices
      boot_indices <- sample(1:nrow(preds_df), nrow(preds_df), replace = TRUE)
      boot_sample <- preds_df[boot_indices, ]

      # B. Calculate Metrics (UNWEIGHTED)
      # We use w = NULL to ensure standard calculation
      auc_val <- MetricsWeighted::AUC(
        actual = boot_sample$outcome_num,
        predicted = boot_sample$.pred_Event,
        w = NULL
      )

      brier_val <- MetricsWeighted::mse(
        actual = boot_sample$outcome_num,
        predicted = boot_sample$.pred_Event,
        w = NULL
      )

      # C. Return row
      tibble(
        bootstrap_id = .x,
        auc = auc_val,
        brier = brier_val
      )
    }
  )

  # Summarize (Same Quantile Logic as 24H/48H/72H)
  summary_metrics <- boot_results %>%
    pivot_longer(
      cols = c(auc, brier),
      names_to = ".metric",
      values_to = "estimate"
    ) %>%
    group_by(.metric) %>%
    summarise(
      .estimate = mean(estimate, na.rm = TRUE),
      .lower = quantile(estimate, 0.025, na.rm = TRUE),
      .upper = quantile(estimate, 0.975, na.rm = TRUE),
      .n = n()
    ) %>%
    mutate(model = model_label)

  return(summary_metrics)
}

# ==============================================================================
# RUN THE BOOTSTRAP
# ==============================================================================
n_boot <- 200
possibly_bootstrap_comp <- possibly(
  calculate_composite_bootstrap_ci,
  otherwise = NULL
)


all_model_performance_comp <- bind_rows(
  possibly_bootstrap_comp(
    current_fit_comp,
    data_composite,
    "NEWS",
    n_bootstrap = n_boot
  ),
  possibly_bootstrap_comp(
    light_fit_comp,
    data_composite,
    "Simplified NEWS",
    n_bootstrap = n_boot
  ),
  possibly_bootstrap_comp(
    full_fit_comp,
    data_composite,
    "DEWS",
    n_bootstrap = n_boot
  ),
  possibly_bootstrap_comp(
    xgb_fit_comp,
    data_composite,
    "XGB-EWS",
    n_bootstrap = n_boot
  )
)

print(all_model_performance_comp)

# ======================
# CREATE THE PLOTS
# ======================

# AUC Plot
auc_p_comp <- all_model_performance_comp %>%
  filter(.metric == "auc") %>%
  ggplot(aes(
    x = fct_reorder(model, .estimate, .desc = TRUE),
    y = .estimate,
    color = model
  )) +
  geom_pointinterval(
    aes(ymin = .lower, ymax = .upper),
    point_size = 4,
    interval_size = 1.2,
    fatten_point = 1.5
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  labs(x = NULL, y = "AUC (Composite Outcome)") +
  scale_color_lancet() +
  coord_cartesian(
    ylim = c(
      min(all_model_performance_comp$.lower[
        all_model_performance_comp$.metric == 'auc'
      ]) -
        0.005,
      NA
    )
  )

# Brier Plot
brier_p_comp <- all_model_performance_comp %>%
  filter(.metric == "brier") %>%
  ggplot(aes(x = fct_reorder(model, .estimate), y = .estimate, color = model)) +
  geom_pointinterval(
    aes(ymin = .lower, ymax = .upper),
    point_size = 4,
    interval_size = 1.2,
    fatten_point = 1.5
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  labs(x = NULL, y = "Brier Score (Composite Outcome)") +
  scale_color_lancet()

# Combine
final_plot_comp <- auc_p_comp +
  brier_p_comp +
  plot_annotation(
    title = "Performance: ICU Admission or Death (24h)",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 18))
  )

print(final_plot_comp)
