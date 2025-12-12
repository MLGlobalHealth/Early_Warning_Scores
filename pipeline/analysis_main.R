# ================================
# LANCET STYLE GGPLOT SYSTEM
# ================================

# Note: NEWS-Light was renamed to "Simplified NEWS" in the manuscript

# Other helper functions

# Helper function: Largest Remainder Method to ensure sum is exactly 100%
fix_percentages <- function(counts, digits = 1) {
  total <- sum(counts)
  raw_pct <- (counts / total) * 100
  factor <- 10^digits
  floored_pct <- floor(raw_pct * factor) / factor
  current_sum <- sum(floored_pct)
  remainder <- round(100 - current_sum, digits)
  fractional_parts <- raw_pct - floored_pct
  n_units <- round(remainder * factor)

  if (n_units > 0) {
    indices <- order(fractional_parts, decreasing = TRUE)[1:n_units]
    floored_pct[indices] <- floored_pct[indices] + (1 / factor)
  }
  return(floored_pct)
}

# Formatting helper
format_group_label <- function(name, n, pct) {
  paste0(
    name,
    " (N: ",
    format(n, big.mark = " ", trim = TRUE),
    ", ",
    format(pct, nsmall = 1, decimal.mark = "·", trim = TRUE),
    "%)"
  )
}

# ----------------------------
# NUMBER FORMATTING FUNCTIONS
# ----------------------------

#' Format numbers with Lancet conventions (mid point for decimals)
#' @param x numeric vector
#' @param digits number of decimal places
#' @param big_mark separator for thousands (default: space)
#' @return formatted character vector
format_lancet <- function(x, digits = 0, big_mark = " ") {
  format(
    round(x, digits),
    big.mark = big_mark,
    decimal.mark = "·",
    scientific = FALSE,
    trim = TRUE
  )
}

#' Convert any text with decimal points to mid points
#' @param text character vector
#' @return character vector with mid points
convert_to_midpoints <- function(text) {
  str_replace_all(text, "\\.", "·")
}

# ------------------------
# LANCET THEME FUNCTION
# ------------------------

#' Lancet journal style theme
#' @param base_size base font size
#' @param base_family base font family
#' @return ggplot theme
theme_lancet <- function(base_size = 14, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Plot titles and subtitles
      plot.title = element_text(
        size = base_size + 2,
        face = "bold",
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        size = base_size - 1,
        color = "gray40",
        margin = margin(b = 15)
      ),

      # Axis text and titles
      axis.text.y = element_text(size = base_size - 2, color = "gray20"),
      axis.text.x = element_text(size = base_size - 3, color = "gray20"),
      axis.title.x = element_text(
        size = base_size - 2,
        color = "gray20",
        margin = margin(t = 10)
      ),
      axis.title.y = element_text(
        size = base_size - 2,
        color = "gray20",
        margin = margin(r = 10)
      ),

      # Grid lines - minimal and subtle
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),

      # Legend
      legend.text = element_text(size = base_size - 3),
      legend.title = element_text(size = base_size - 2, face = "bold"),
      legend.margin = margin(5, 5, 5, 5),

      # Plot margins
      plot.margin = margin(15, 15, 15, 15),

      # Strip text for facets
      strip.text = element_text(size = base_size - 2, face = "bold"),
      strip.background = element_rect(fill = "gray95", color = NA)
    )
}

# -----------------------
# LANCET COLOR PALETTES
# -----------------------

# Professional color palette suitable for medical journals
lancet_colors <- c(
  "#2E86AB", # Blue
  "#A23B72", # Purple/Magenta
  "#F18F01", # Orange
  "#C73E1D", # Red
  "#5D737E", # Gray-blue
  "#64A6BD", # Light blue
  "#F4B942", # Yellow
  "#8B5A2B" # Brown
)

# Color palette function
scale_fill_lancet <- function(...) {
  scale_fill_manual(values = lancet_colors, ...)
}

scale_color_lancet <- function(...) {
  scale_color_manual(values = lancet_colors, ...)
}

# --------------------------------
# 4. AXIS FORMATTING FUNCTIONS
# --------------------------------

#' Lancet-style continuous scale with mid points
scale_y_lancet <- function(digits = 1, ...) {
  scale_y_continuous(
    labels = function(x) format_lancet(x, digits = digits),
    ...
  )
}

scale_x_lancet <- function(digits = 1, ...) {
  scale_x_continuous(
    labels = function(x) format_lancet(x, digits = digits),
    ...
  )
}


# Working directory
setwd("/home/alex/ews/NEWS2_Evaluation/Modelling")

#########################################
############# Libraries #################
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
library(arrow) # Read parquet files
library(riskRegression) # Risk modelling
library(halfmoon) # Checking propensity
library(geomtextpath) # Text in ggplot2
library(ggdist) # For further plotting
library(ggridges) # For plotting
library(scales) # GGplot modification
library(WeightIt) # Weighting methods
library(cobalt) # Balance checking
library(gtsummary) # For summary tables
library(survey) # For weights
library(gt) # For summary tables
library(gtsummary) # For tables
library(survey) # For weights
library(janitor) # For quick tabyls
library(rlang) # For misc functions
library(shapviz) # For SHAP values

###############################################################
################ Start of the pre-processing ##################
###############################################################

data <- nanoparquet::read_parquet(
  "/home/alex/ews/NEWS2_Evaluation/Additional_Data/df_with_embeddings_v2.parquet",
)


# General polishing of the dataframe

data$Hospital <- as.factor(data$Hospital)

levels(data$Hospital)[3] <- "Hospital of Bornholm"

levels(data$Hospital)[10] <- "Zealand University Hospital"

data$Status24H <- as.factor(data$Status24H)

data$Status24H <- relevel(data$Status24H, "Deceased")

data <- data |>
  relocate(Hospital, .after = CSN) |>
  relocate(Department_Name, .after = Hospital)

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

# Apply the categorization to dataframe
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

### Grab all the pca columns
pca_columns <- grep("^pc", names(data), value = TRUE)

my_formula_pca <- reformulate(termlabels = pca_columns)

### Grab all the blood tests
median_columns <- grep("^median", names(data), value = TRUE)

my_formula_med <- reformulate(termlabels = median_columns)

### Create a composite variable (average of all PCA columns)
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


######################## Propensity Score ##########################

# Use propensity score modelling for the probability of interventions at 24 hours

weight_model <- weightit(
  Interventions_24 ~
    Age_Group +
    Sex +
    Hospital +
    Blood_Pressure.Sys +
    Temperature +
    Respiration_Rate +
    Saturation +
    Pulse +
    Previous_Hosp +
    previous_icu_respiratory +
    Oxygen_Supplement +
    Consciousness +
    pca_composite,
  data = data,
  method = "cbps",
  estimand = "ATE"
)

# Store the weights

w_raw <- weight_model$weights

w_df <- as.data.frame(w_raw)

colnames(w_df) <- c("weights_new")

# Save the weights in a dataframe

write_parquet(w_df, "weights_cbps_24_new_nov_v2.parquet")

# Attach to the data
data <- data %>%
  mutate(weights_new = w_raw)

# Absolute standardized mean differences

plot_ipw <- data |>
  tidy_smd(
    c(
      Age_Group,
      Sex,
      Hospital,
      Blood_Pressure.Sys,
      Respiration_Rate,
      Temperature,
      Saturation,
      Pulse,
      Oxygen_Supplement,
      Consciousness,
      pca_composite,
      Previous_Hosp,
      previous_icu_respiratory
    ),
    .group = Interventions_24,
    .wts = weights_new
  )

# Love plot of absolute standardized mean differences

plot_love <- love_plot(plot_ipw) +
  theme_lancet(base_size = 16) +
  theme(legend.position = "top") +
  labs(
    x = "Absolute standardized mean differences",
    y = "",
    fill = "",
    color = ""
  ) +
  scale_fill_lancet(labels = c("Unweighted", "Weighted")) +
  scale_color_lancet(labels = c("Unweighted", "Weighted")) +
  scale_x_lancet() +
  scale_y_discrete(
    labels = c(
      "Age group",
      "Systolic blood pressure",
      "Consciousness",
      "Hospital",
      "Oxygen supplement",
      "Embedding score",
      "Number of previous hospitalizations",
      "History of previous ICU or respiratory support",
      "Pulse",
      "Respiratory rate",
      "Saturation",
      "Sex",
      "Temperature"
    )
  )


plot_love

# Check the distributions of the propensity scores amongs groups

bal_plot <- bal.plot(
  weight_model,
  var.name = "prop.score",
  which = "both",
  type = "histogram",
  mirror = TRUE
) +
  theme_lancet(base_size = 16) +
  labs(x = "Propensity Score", y = "Proportion", title = "") +
  scale_color_lancet(
    name = "Received major intervention",
    labels = c("No", "Yes")
  ) +
  scale_fill_lancet(
    name = "Received major intervention",
    labels = c("No", "Yes")
  ) +
  scale_x_lancet() +
  scale_y_lancet() +
  theme(legend.position = "top")

bal_plot

####################################################
###################### Modelling ###################
####################################################

data$imp_weights <- importance_weights(data$weights_new)

# Keep only the weighted individuals without interventions

data <- data |> filter(Interventions_24 == 0)

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

# Check the unique individuals in the data

length(unique(data$PT_ID)) # 825,200 individuals

# Check number of unique hospital encounters

length(unique(data$CSN)) # 2,080,765 unique encounters

set.seed(234)

data_folds <- rsample::group_vfold_cv(data, group = Hospital)

# Now we will create our model

model <- logistic_reg(engine = "glm", mode = "classification")

# Let's also create an xgboost model

xgb <- boost_tree(trees = 100, mtry = 30) |>
  set_engine("xgboost") |>
  set_mode("classification")

############## Workflows now ###############

# Current model

current_wf <- workflow() |>
  add_formula(Status24H ~ EWS_score) |>
  add_model(model) |>
  add_case_weights(imp_weights)

# NEWS2-Light

light_wf <- workflow() |>
  add_formula(Status24H ~ EWS_light) |>
  add_model(model) |>
  add_case_weights(imp_weights)

# IEWS

full_wf <- workflow() |>
  add_formula(Status24H ~ IEWS_Light) |>
  add_model(model) |>
  add_case_weights(imp_weights)

# XGBoost

xgb_wf <- workflow() |>
  add_formula(
    Status24H ~
      Age +
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
  ) |>
  add_model(xgb) |>
  add_case_weights(imp_weights)

xgb_wf


# Set up parallel processing
doParallel::registerDoParallel(cores = 40)

cntrl <- control_resamples(save_pred = T)


# Grouped cross validations using hospitals as folds

current_fit <- fit_resamples(
  current_wf,
  resamples = data_folds,
  metrics = metric_set(
    roc_auc,
    brier_class
  ),
  control = cntrl
)

light_fit <- fit_resamples(
  light_wf,
  resamples = data_folds,
  metrics = metric_set(
    roc_auc,
    brier_class
  ),
  control = cntrl
)


full_fit <- fit_resamples(
  full_wf,
  resamples = data_folds,
  metrics = metric_set(
    roc_auc,
    brier_class
  ),
  control = cntrl
)

set.seed(234)

xgb_fit <- fit_resamples(
  xgb_wf,
  resamples = data_folds,
  metrics = metric_set(
    roc_auc,
    brier_class
  ),
  control = cntrl
)

# Generate bootstrapped predictions

calculate_overall_bootstrap_ci <- function(
  fit_object,
  original_data,
  model_name,
  n_bootstrap = 200
) {
  preds_df <- fit_object %>%
    collect_predictions() %>%
    arrange(.row) %>%
    mutate(
      Status24H = original_data$Status24H,
      weights_new = original_data$weights_new,
      mort24H = if_else(Status24H == "Deceased", 1, 0)
    )

  # Perform Bootstrapping
  set.seed(42)

  boot_results <- map_dfr(
    1:n_bootstrap,
    ~ {
      boot_indices <- sample(1:nrow(preds_df), nrow(preds_df), replace = TRUE)
      boot_sample <- preds_df[boot_indices, ]

      auc_val <- MetricsWeighted::AUC(
        actual = boot_sample$mort24H,
        predicted = boot_sample$.pred_Deceased,
        w = boot_sample$weights_new
      )
      brier_val <- MetricsWeighted::mse(
        actual = boot_sample$mort24H,
        predicted = boot_sample$.pred_Deceased,
        w = boot_sample$weights_new
      )

      # Return a one-row tibble for this iteration.
      tibble(
        bootstrap_id = .x,
        auc = auc_val,
        brier = brier_val
      )
    }
  )

  # Summarize Bootstrap Results
  summary_metrics <- boot_results %>%
    # Pivot to a long format for easy summarization by metric.
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
    mutate(model = model_name)

  return(summary_metrics)
}

# Run the Analysis and Generate Plots
n_boot <- 200

# Create a "safe" version of the function that returns NULL on error.
possibly_bootstrap <- possibly(calculate_overall_bootstrap_ci, otherwise = NULL)

# Calculate bootstrapped performance metrics for all four models.
all_model_performance <- bind_rows(
  possibly_bootstrap(current_fit, data, "NEWS", n_bootstrap = n_boot),
  possibly_bootstrap(light_fit, data, "Simplified NEWS", n_bootstrap = n_boot),
  possibly_bootstrap(full_fit, data, "DEWS", n_bootstrap = n_boot),
  possibly_bootstrap(xgb_fit, data, "XGB-EWS", n_bootstrap = n_boot)
)

# Create the Dot-Interval Plot
auc_p <- all_model_performance %>%
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
  labs(
    x = NULL,
    y = "Area Under the Curve (AUC)"
  ) +
  scale_fill_lancet() +
  scale_color_lancet() +
  scale_y_lancet(digits = 5) +
  coord_cartesian(
    ylim = c(
      min(all_model_performance$.lower[
        all_model_performance$.metric == "auc"
      ]) -
        0.005,
      NA
    )
  )

# Brier Score
# For Brier Score, lower is better, so we reorder in ascending order (default).
brier_p <- all_model_performance %>%
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
  scale_fill_lancet() +
  scale_color_lancet() +
  scale_y_lancet(digits = 5) +
  labs(
    x = NULL,
    y = "Brier Score"
  )

# Combine the two plots into a single figure
final_plot <- auc_p +
  brier_p +
  plot_annotation(
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
  )

final_plot

# Compute the metrics for each hospital

calculate_metrics_by_hospital_with_bootstrap_ci_refactored <- function(
  fit_object,
  data,
  model_name,
  n_bootstrap = 200
) {
  calculate_metrics_on_sample <- function(df) {
    tryCatch(
      {
        tibble(
          auc = MetricsWeighted::AUC(
            actual = df$mort24H,
            predicted = df$.pred_Deceased,
            w = df$weights_new
          ),
          brier = MetricsWeighted::mse(
            actual = df$mort24H,
            predicted = df$.pred_Deceased,
            w = df$weights_new
          )
        )
      },
      error = function(e) {
        # Return NA if metric calculation fails on a bootstrap sample
        tibble(auc = NA_real_, brier = NA_real_)
      }
    )
  }

  # Set a single seed for reproducibility of the entire process
  set.seed(42)

  # Prepare the initial predictions data
  preds_data <- fit_object %>%
    collect_predictions() %>%
    arrange(.row) %>%
    mutate(
      mort24H = if_else(data$Status24H == "Deceased", 1, 0),
      Hospital = data$Hospital,
      weights_new = data$weights_new
    )

  metrics_by_hospital <- preds_data %>%
    group_by(Hospital) %>%
    nest() %>% # Creates a list-column 'data' containing the data for each hospital
    mutate(
      n_patients = map_int(data, nrow),

      # Calculate point estimates
      point_estimates = map(data, calculate_metrics_on_sample),

      # Generate bootstrap resamples using rsample
      boot_samples = map(data, ~ rsample::bootstraps(.x, times = n_bootstrap)),

      # Calculate metrics on each bootstrap sample
      boot_metrics = map(
        boot_samples,
        ~ {
          .x %>%
            mutate(
              metrics = map(
                splits,
                ~ calculate_metrics_on_sample(rsample::analysis(.))
              )
            ) %>%
            select(id, metrics) %>%
            unnest(metrics)
        }
      ),

      # Summarize bootstrap results to get percentile confidence intervals
      conf_intervals = map(
        boot_metrics,
        ~ {
          summarise(
            .x,
            across(
              c(auc, brier),
              list(
                lci = ~ quantile(.x, 0.025, na.rm = TRUE),
                uci = ~ quantile(.x, 0.975, na.rm = TRUE)
              ),
              .names = "{.col}_{.fn}"
            )
          )
        }
      )
    ) %>%
    # Unnest and format the final results table
    select(Hospital, n_patients, point_estimates, conf_intervals) %>%
    unnest(c(point_estimates, conf_intervals)) %>%
    rename(
      AUC = auc,
      Brier_Score = brier,
      AUC_LCI = auc_lci,
      AUC_UCI = auc_uci,
      Brier_LCI = brier_lci,
      Brier_UCI = brier_uci
    ) %>%
    mutate(model = model_name) %>%
    arrange(desc(n_patients))

  return(metrics_by_hospital)
}


# Calculate metrics by hospital for each model using the bootstrap function
hospital_metrics_current <- calculate_metrics_by_hospital_with_bootstrap_ci_refactored(
  current_fit,
  data,
  "NEWS"
)
hospital_metrics_light <- calculate_metrics_by_hospital_with_bootstrap_ci_refactored(
  light_fit,
  data,
  "Simplified NEWS"
)
hospital_metrics_full <- calculate_metrics_by_hospital_with_bootstrap_ci_refactored(
  full_fit,
  data,
  "DEWS"
)
hospital_metrics_xgb <- calculate_metrics_by_hospital_with_bootstrap_ci_refactored(
  xgb_fit,
  data,
  "XGB-EWS"
)

# Combine all hospital metrics
all_hospital_metrics <- bind_rows(
  hospital_metrics_current,
  hospital_metrics_light,
  hospital_metrics_full,
  hospital_metrics_xgb
)

# First do the original cleaning
all_hospital_metrics <- all_hospital_metrics |>
  mutate(
    Hospital = if_else(
      Hospital == "HGH, Herlev and Gentofte Hospital",
      "Herlev and Gentofte Hospital",
      Hospital
    )
  ) |>
  mutate(
    Hospital = if_else(
      Hospital == "NOH, Hospital of North Zealand",
      "Hospital of North Zealand",
      Hospital
    )
  ) |>
  mutate(
    Hospital = if_else(
      Hospital == "Nykøbing Sygehus",
      "Nykøbing Hospital",
      Hospital
    )
  ) |>
  mutate(
    Hospital = if_else(
      Hospital == "Holbæk Sygehus",
      "Holbæk Hospital",
      Hospital
    )
  )

# Now create hospital masking with the cleaned names
hospital_mapping <- all_hospital_metrics %>%
  distinct(Hospital, n_patients) %>%
  arrange(desc(n_patients)) %>%
  mutate(
    Hospital_Masked = case_when(
      Hospital == "Herlev and Gentofte Hospital" ~ "Hospital A and B",
      Hospital == "Hospital of North Zealand" ~ "Hospital C",
      Hospital == "Amager and Hvidovre Hospital" ~ "Hospital D and E",
      Hospital == "Rigshospitalet" ~ "Hospital F",
      Hospital == "Bispebjerg and Frederiksberg Hospitals" ~ "Hospital G and H",
      Hospital == "Zealand University Hospital" ~ "Hospital I",
      Hospital == "Næstved, Slagelse and Ringsted Hospitals" ~
        "Hospital J, K, and L",
      Hospital == "Holbæk Hospital" ~ "Hospital M",
      Hospital == "Nykøbing Hospital" ~ "Hospital N",
      Hospital == "Hospital of Bornholm" ~ "Hospital O"
    )
  )

# Apply the masking
all_hospital_metrics <- all_hospital_metrics %>%
  left_join(
    hospital_mapping %>% select(Hospital, Hospital_Masked),
    by = "Hospital"
  ) %>%
  mutate(Hospital = Hospital_Masked) %>%
  select(-Hospital_Masked) %>%
  mutate_at(vars(Hospital), as.factor)

##### Create a dotplot for the hospital metrics ######

hospital_order <- all_hospital_metrics %>%
  ungroup() %>%
  distinct(Hospital, n_patients) %>%
  arrange(desc(n_patients)) %>%
  mutate(y_level = row_number())

shading_data <- hospital_order %>%
  filter(y_level %% 2 == 1) %>%
  mutate(ymin = y_level - 0.5, ymax = y_level + 0.5)

all_hospital_metrics <- all_hospital_metrics %>%
  ungroup() %>%
  left_join(hospital_order %>% select(Hospital, y_level), by = "Hospital") %>%
  mutate(Hospital_Ordered = fct_reorder(Hospital, y_level, .desc = FALSE))

# Plot of the AUC for hospitals
auc_dotplot_ci_refined <- ggplot(all_hospital_metrics) +
  geom_rect(
    data = shading_data,
    aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
    fill = "grey90",
    alpha = 0.5
  ) +
  geom_hline(
    yintercept = hospital_order$y_level,
    color = "grey80",
    linetype = "dotted",
    linewidth = 0.3
  ) +
  geom_errorbarh(
    aes(xmin = AUC_LCI, xmax = AUC_UCI, y = Hospital_Ordered, color = model),
    height = 0,
    position = position_dodge(width = 0.7),
    alpha = 0.7,
    linewidth = 0.6
  ) +
  geom_point(
    aes(x = AUC, y = Hospital_Ordered, color = model),
    position = position_dodge(width = 0.7),
    size = 2.5
  ) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey50") +
  scale_color_lancet() +
  scale_x_lancet(digits = 3) +
  coord_cartesian(
    xlim = c(
      min(all_hospital_metrics$AUC_LCI, na.rm = TRUE) - 0.02,
      max(all_hospital_metrics$AUC_UCI, na.rm = TRUE) + 0.02
    )
  ) +
  labs(
    x = "Area Under the Receiver Operating Characteristic Curve (AUC)",
    y = "",
    color = ""
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = rel(0.95)),
    axis.title.x = element_text(margin = margin(t = 15))
  )

# Define dodging width for consistency
dodge_width <- 0.7

# Plot for Brier Score for the hospitals
brier_dotplot_ci_refined <- ggplot(
  all_hospital_metrics,
  aes(x = Brier_Score, y = Hospital_Ordered, color = model)
) +
  geom_rect(
    data = shading_data,
    aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
    fill = "grey90",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  geom_hline(
    yintercept = hospital_order$y_level,
    color = "grey80",
    linetype = "dotted",
    linewidth = 0.3
  ) +
  geom_errorbarh(
    aes(xmin = Brier_LCI, xmax = Brier_UCI),
    height = 0,
    position = position_dodge(width = dodge_width),
    alpha = 0.7,
    linewidth = 0.6
  ) +
  geom_point(position = position_dodge(width = dodge_width), size = 2.5) +
  scale_color_lancet() +
  scale_x_lancet(digits = 5) +
  coord_cartesian(
    xlim = c(
      max(0, min(all_hospital_metrics$Brier_LCI, na.rm = TRUE) - 0.001),
      max(all_hospital_metrics$Brier_UCI, na.rm = TRUE) + 0.001
    )
  ) +
  labs(
    x = "Brier Score",
    y = NULL,
    color = ""
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = rel(0.95)),
    axis.title.x = element_text(margin = margin(t = 15))
  )

##############################################
########### Calibration Curves ###############
##############################################

# Get the predicted probabilities for all models

current_preds <- current_fit |>
  collect_predictions() |>
  arrange(.row) |>
  pull(.pred_Deceased)

light_preds <- light_fit |>
  collect_predictions() |>
  arrange(.row) |>
  pull(.pred_Deceased)

full_preds <- full_fit |>
  collect_predictions() |>
  arrange(.row) |>
  pull(.pred_Deceased)

xgb_preds <- xgb_fit |>
  collect_predictions() |>
  arrange(.row) |>
  pull(.pred_Deceased)

# Then I need to put them in the dataframe called data

data <- data |>
  mutate(
    pred_current = current_preds,
    pred_light = light_preds,
    pred_full = full_preds,
    pred_xgb = xgb_preds
  )

# Now compute the distribution of the predicted probabilities for all models in separate plots

summary(data$pred_current)
summary(data$pred_light)
summary(data$pred_full)
summary(data$pred_xgb)

# ===================================================
# Analyze (Returns Smooth Curve + Raw Data for Rug
# ===================================================

create_focused_curve <- function(data, truth, estimate, wt, limit = 0.1) {
  truth_col <- enquo(truth)
  estimate_col <- enquo(estimate)
  wt_col <- enquo(wt)

  # Clean Data
  df <- data %>%
    select(
      outcome_raw = !!truth_col,
      pred_prob = !!estimate_col,
      weight_val = !!wt_col
    ) %>%
    mutate(outcome_numeric = if_else(outcome_raw == "Deceased", 1, 0)) %>%
    drop_na()

  # FIT a local restricted cubic spline using rms
  # Fit on data up to (limit + 0.05) to anchor spline correctly
  buffer <- limit + 0.05
  df_curve <- df %>% filter(pred_prob <= buffer)

  # Normalize weights
  df_curve$w_scaled <- df_curve$weight_val / mean(df_curve$weight_val)

  # Fit GLM with Restricted Cubic Splines (4 knots)
  rcs_model <- glm(
    outcome_numeric ~ rcs(pred_prob, 4),
    data = df_curve,
    weights = w_scaled,
    family = binomial
  )

  # Predict Grid (Strictly 0 to limit)
  pred_grid <- seq(0, limit, length.out = 300)

  link_pred <- predict(
    rcs_model,
    newdata = data.frame(pred_prob = pred_grid),
    type = "link",
    se.fit = TRUE
  )

  smooth_data <- tibble(
    predicted = pred_grid,
    observed = plogis(link_pred$fit),
    ci_low = plogis(link_pred$fit - 1.96 * link_pred$se.fit),
    ci_high = plogis(link_pred$fit + 1.96 * link_pred$se.fit)
  )

  # Return both the smooth line AND the raw data (filtered to plot limit) for the rug
  list(
    smooth = smooth_data,
    raw = df_curve %>% filter(pred_prob <= limit)
  )
}

# =================================================
# Plotting (Y-axis to 20%, Rug added)
# =================================================

plot_focused_curve <- function(
  cal_data,
  model_name,
  limit = 0.1,
  color = "blue"
) {
  smooth_data <- cal_data$smooth
  raw_data <- cal_data$raw

  ggplot() +
    # Diagonal Reference (Slope=1)
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = "gray60"
    ) +

    # Confidence Ribbon
    geom_ribbon(
      data = smooth_data,
      aes(x = predicted, ymin = ci_low, ymax = ci_high),
      fill = color,
      alpha = 0.15,
      color = NA
    ) +

    # RCS Curve
    geom_line(
      data = smooth_data,
      aes(x = predicted, y = observed),
      color = color,
      linewidth = 1.5
    ) +

    # Formatting
    coord_cartesian(xlim = c(0, limit), ylim = c(0, 0.2)) +
    scale_x_continuous(
      breaks = seq(0, limit, by = 0.01),
      labels = scales::percent_format(accuracy = 1)
    ) +
    scale_y_continuous(
      breaks = seq(0, 0.2, by = 0.05), # Breaks every 5% on Y-axis
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(
      title = model_name,
      x = "Predicted Probability",
      y = "Observed Probability"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray92"),
      plot.title = element_text(face = "bold", hjust = 0),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# ===========
# EXECUTION
# ===========

colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D")
zoom_limit <- 0.1

# Analyze
cal_current <- create_focused_curve(
  data,
  Status24H,
  pred_current,
  weights_new,
  limit = zoom_limit
)
cal_light <- create_focused_curve(
  data,
  Status24H,
  pred_light,
  weights_new,
  limit = zoom_limit
)
cal_full <- create_focused_curve(
  data,
  Status24H,
  pred_full,
  weights_new,
  limit = zoom_limit
)
cal_xgb <- create_focused_curve(
  data,
  Status24H,
  pred_xgb,
  weights_new,
  limit = zoom_limit
)

# Plot
p1 <- plot_focused_curve(
  cal_current,
  "NEWS",
  limit = zoom_limit,
  color = colors[1]
)
p2 <- plot_focused_curve(
  cal_light,
  "Simplified NEWS",
  limit = zoom_limit,
  color = colors[2]
)
p3 <- plot_focused_curve(
  cal_full,
  "DEWS",
  limit = zoom_limit,
  color = colors[3]
)
p4 <- plot_focused_curve(
  cal_xgb,
  "XGB-EWS",
  limit = zoom_limit,
  color = colors[4]
)

# Final Plot for all models

(p1 + p2) / (p3 + p4)


################################################################################
########################## Calibration Curves by subgroups ####################
################################################################################

# Define colors and order once
model_colors <- c(
  "NEWS" = "#2E86AB",
  "Simplified NEWS" = "#A23B72",
  "DEWS" = "#F18F01",
  "XGB-EWS" = "#C73E1D"
)
model_levels <- c("NEWS", "Simplified NEWS", "DEWS", "XGB-EWS")

# Function to generating the data for the Facet Plot
# (Calculates the RCS spline for every subgroup/model combination)
prepare_calibration_data <- function(
  data,
  group_col,
  levels_vec,
  models_list,
  limit = 0.1
) {
  map_dfr(levels_vec, function(grp) {
    map_dfr(models_list, function(mod) {
      # Filter Data
      sub_data <- data %>% filter(.data[[group_col]] == grp)

      # Calculate Spline
      cal_obj <- create_focused_curve(
        data = sub_data,
        truth = Status24H,
        estimate = !!sym(mod$col),
        wt = weights_new,
        limit = limit
      )

      # Extract Smooth Data and add labels
      cal_obj$smooth %>%
        mutate(
          Group_Label = grp,
          Model = mod$label
        )
    })
  }) %>%
    mutate(
      Model = factor(Model, levels = model_levels),
      Group_Label = factor(Group_Label, levels = levels_vec)
    )
}

# PREPARE DATA
age_levels <- c("18-65", "66-80", "80+")
models_config <- list(
  list(col = "pred_current", label = "NEWS"),
  list(col = "pred_light", label = "Simplified NEWS"),
  list(col = "pred_full", label = "DEWS"),
  list(col = "pred_xgb", label = "XGB-EWS")
)

plot_data_age <- prepare_calibration_data(
  data,
  "Age_Group",
  age_levels,
  models_config
)

# PLOT
p_age <- ggplot(
  plot_data_age,
  aes(x = predicted, y = observed, color = Model, fill = Model)
) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +

  # MATRIX LAYOUT
  facet_grid(Group_Label ~ Model) +

  # Scales & Limits
  coord_cartesian(xlim = c(0, 0.1), ylim = c(0, 0.3)) +
  scale_x_continuous(
    breaks = seq(0, 0.1, 0.02),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.3, 0.1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_color_manual(values = model_colors) +
  scale_fill_manual(values = model_colors) +
  labs(
    x = "Predicted Risk",
    y = "Observed Mortality"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",

    # SPACINGS & MARGINS
    # Space between the panels (columns/rows)
    panel.spacing = unit(2, "lines"),

    # 2. Space around the entire plot
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),

    # 3. Push the X-Axis TITLE further down
    axis.title.x = element_text(margin = margin(t = 20)),

    # 4. Push the X-Axis TICKS/LABELS further from the line
    axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = 10)),
    strip.text = element_text(face = "bold", size = 14),
    strip.background = element_rect(fill = "gray95", color = NA),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray90", fill = NA),
    plot.title = element_text(face = "bold")
  )

p_age

# PREPARE DATA for sex stratified analysis
sex_levels <- c("Male", "Female")

plot_data_sex <- prepare_calibration_data(
  data,
  "Sex",
  sex_levels,
  models_config
)

# PLOT

p_sex <- ggplot(
  plot_data_sex,
  aes(x = predicted, y = observed, color = Model, fill = Model)
) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +

  # MATRIX LAYOUT
  facet_grid(Group_Label ~ Model) +

  # Scales & Limits
  coord_cartesian(xlim = c(0, 0.1), ylim = c(0, 0.3)) +
  scale_x_continuous(
    breaks = seq(0, 0.1, 0.02),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.3, 0.1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_color_manual(values = model_colors) +
  scale_fill_manual(values = model_colors) +
  labs(
    x = "Predicted Risk",
    y = "Observed Mortality"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",

    # --- SPACINGS & MARGINS
    panel.spacing = unit(2, "lines"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = 10)),
    strip.text = element_text(face = "bold", size = 14),
    strip.background = element_rect(fill = "gray95", color = NA),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray90", fill = NA),
    plot.title = element_text(face = "bold")
  )

p_sex

################################################################################
########################## Decision curve analysis #############################
################################################################################

# First compute the weighted prevalence of my data

weighted_prevalence <- weighted.mean(
  x = if_else(data$Status24H == "Deceased", 1, 0),
  w = data$weights_new,
  na.rm = TRUE
)

# Compute the multiples of the prevalence to use as thresholds
cut_points <- weighted_prevalence * c(1, 2, 4, 8, 10, 20, 30)

#  Filter out any cut points that are beyond our plot's x-axis limit
plot_xlim <- 0.05
cut_points_to_plot <- cut_points[cut_points <= plot_xlim]

# Print the calculated values to see what they are
print("Weighted Prevalence of Mortality:")
print(weighted_prevalence)
print("Cut Points for Vertical Lines:")
print(cut_points_to_plot)


calculate_weighted_nb <- function(data, estimate, truth, wt, thresholds) {
  # Use {{}} to handle unquoted column names
  estimate_col <- enquo(estimate)
  truth_col <- enquo(truth)
  wt_col <- enquo(wt)

  # Prepare the data once
  df <- data %>%
    select(!!estimate_col, !!truth_col, !!wt_col) %>%
    mutate(truth_numeric = if_else(!!truth_col == "Deceased", 1, 0))

  # Get the total sum of weights
  total_weight <- sum(df[[quo_name(wt_col)]], na.rm = TRUE)

  # Use map_dfr to iterate over each threshold and calculate net benefit
  map_dfr(
    thresholds,
    ~ {
      pt <- .x # Current threshold

      # Calculate weighted counts of TP and FP at the current threshold
      summary_df <- df %>%
        mutate(
          is_positive = if_else(!!estimate_col >= pt, 1, 0)
        ) %>%
        group_by(truth_numeric, is_positive) %>%
        summarise(sum_w = sum(!!wt_col, na.rm = TRUE), .groups = "drop")

      # Extract weighted TP and FP
      tp_w <- summary_df$sum_w[
        summary_df$truth_numeric == 1 & summary_df$is_positive == 1
      ]
      fp_w <- summary_df$sum_w[
        summary_df$truth_numeric == 0 & summary_df$is_positive == 1
      ]

      # Handle cases where there are no TPs or FPs
      if (length(tp_w) == 0) {
        tp_w <- 0
      }
      if (length(fp_w) == 0) {
        fp_w <- 0
      }

      # Calculate net benefit
      net_benefit <- (tp_w / total_weight) -
        (fp_w / total_weight) * (pt / (1 - pt))

      tibble(
        threshold = pt,
        net_benefit = net_benefit
      )
    }
  )
}

# Define the thresholds to evaluate
thresholds <- seq(0, 0.05, by = 0.0005)

# Calculate Net Benefit for each model
nb_current <- calculate_weighted_nb(
  data,
  pred_current,
  Status24H,
  weights_new,
  thresholds
) %>%
  mutate(model = "NEWS")
nb_light <- calculate_weighted_nb(
  data,
  pred_light,
  Status24H,
  weights_new,
  thresholds
) %>%
  mutate(model = "Simplified NEWS")
nb_full <- calculate_weighted_nb(
  data,
  pred_full,
  Status24H,
  weights_new,
  thresholds
) %>%
  mutate(model = "DEWS")
nb_xgb <- calculate_weighted_nb(
  data,
  pred_xgb,
  Status24H,
  weights_new,
  thresholds
) %>%
  mutate(model = "XGB-EWS")

# Calculate Net Benefit for "Treat All"

# First, find the weighted prevalence of the event
weighted_prevalence <- weighted.mean(
  if_else(data$Status24H == "Deceased", 1, 0),
  w = data$weights_new
)
nb_all <- tibble(
  threshold = thresholds,
  net_benefit = weighted_prevalence -
    (1 - weighted_prevalence) * (thresholds / (1 - thresholds)),
  model = "Treat All"
)

# Create the "Treat None" curve
nb_none <- tibble(
  threshold = thresholds,
  net_benefit = 0,
  model = "Treat None"
)

# Combine all data for plotting
all_nb_data <- bind_rows(
  nb_current,
  nb_light,
  nb_full,
  nb_xgb,
  nb_all,
  nb_none
) %>%
  mutate(
    model = factor(
      model,
      levels = c(
        "XGB-EWS",
        "DEWS",
        "NEWS",
        "Simplified NEWS",
        "Treat All",
        "Treat None"
      )
    )
  )

# Create the weighted Decision Curve Analysis plot
plot_data_stopped <- all_nb_data %>%
  mutate(
    net_benefit = if_else(
      model == "Treat All" & net_benefit < -0.005,
      NA_real_,
      net_benefit
    )
  )

# Create the weighted Decision Curve Analysis plot
weighted_dca_final_with_lines <- ggplot(
  plot_data_stopped,
  aes(x = threshold, y = net_benefit, color = model)
) +
  geom_line(linewidth = 1.1) +
  coord_cartesian(
    xlim = c(0, plot_xlim),
    ylim = c(-0.001, max(all_nb_data$net_benefit, na.rm = TRUE))
  ) +
  scale_x_lancet(digits = 4) +
  scale_y_lancet(digits = 4, breaks = scales::pretty_breaks(n = 7)) +
  labs(
    x = "Probability Threshold",
    y = "Net Benefit",
    color = ""
  ) +
  scale_color_lancet() +
  theme_minimal(base_size = 16) +
  theme(legend.position = "top")

# Print the final plot
weighted_dca_final_with_lines


# Creating the comparison of net benefit differences

# For models
nb_at_cuts_current <- calculate_weighted_nb(
  data,
  pred_current,
  Status24H,
  weights_new,
  cut_points
) %>%
  mutate(model = "NEWS")

nb_at_cuts_light <- calculate_weighted_nb(
  data,
  pred_light,
  Status24H,
  weights_new,
  cut_points
) %>%
  mutate(model = "Simplified NEWS") # Renamed from NEWS-Light

nb_at_cuts_full <- calculate_weighted_nb(
  data,
  pred_full,
  Status24H,
  weights_new,
  cut_points
) %>%
  mutate(model = "DEWS")

nb_at_cuts_xgb <- calculate_weighted_nb(
  data,
  pred_xgb,
  Status24H,
  weights_new,
  cut_points
) %>%
  mutate(model = "XGB-EWS")

# For "Treat All" strategy
nb_at_cuts_all <- tibble(
  threshold = cut_points,
  net_benefit = weighted_prevalence -
    (1 - weighted_prevalence) * (cut_points / (1 - cut_points)),
  model = "Treat All"
)

# For "Treat None" strategy
nb_at_cuts_none <- tibble(
  threshold = cut_points,
  net_benefit = 0,
  model = "Treat None"
)

# Combine these precise calculations into a single data frame
nb_at_exact_cuts <- bind_rows(
  nb_at_cuts_current,
  nb_at_cuts_light,
  nb_at_cuts_full,
  nb_at_cuts_xgb,
  nb_at_cuts_all,
  nb_at_cuts_none
)

# Pivot, Scale, and Calculate Differences
comparison_data_consistent <- nb_at_exact_cuts %>%
  select(threshold, model, net_benefit) %>%
  pivot_wider(names_from = model, values_from = net_benefit) %>%
  # Scale by 10,000 to get values per 10,000 patients
  mutate(across(where(is.numeric) & !matches("threshold"), ~ .x * 10000)) %>%
  mutate(
    across(
      .cols = c(
        NEWS,
        `Simplified NEWS`,
        `XGB-EWS`,
        `DEWS`,
        `Treat All`,
        `Treat None`
      ),
      .fns = ~ round(.x, digits = 1)
    )
  ) %>%
  mutate(
    # 1. NEWS vs XGB-EWS (Standard vs. Complex ML)
    difference1 = NEWS - `XGB-EWS`,

    # 2. NEWS vs Simplified NEWS (Standard vs. Simple)
    difference2 = NEWS - `Simplified NEWS`,

    # 3. DEWS vs XGB-EWS (Requested Change)
    # Negative value means XGB-EWS is better
    difference3 = DEWS - `XGB-EWS`,

    # 4. NEWS vs DEWS (The Missing Link)
    # Tests if Age/Sex adjustment (DEWS) beats the Standard (NEWS)
    difference4 = NEWS - DEWS
  ) %>%
  select(
    threshold,
    `Treat All`,
    `Treat None`,
    NEWS,
    `Simplified NEWS`,
    `DEWS`,
    `XGB-EWS`,
    difference1,
    difference2,
    difference3,
    difference4
  ) %>%
  arrange(threshold)

# Create the Final gt Table

final_comparison_table <- comparison_data_consistent %>%
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
  gt() %>%
  tab_header(
    title = "Decision Curve Analysis: Net Benefit Comparison",
    subtitle = "Comparing models at exact prevalence-based thresholds. Values are per 10,000 patients."
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
    difference3 = "DEWS vs XGB-EWS", # Updated Label
    difference4 = "NEWS vs DEWS" # New Label
  ) %>%
  # Format numbers with middle dot and space as thousands separator
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

# Display the final table
final_comparison_table
final_comparison_table |> gtsave("weights_final_comparison_table_nov.docx")

################################################################################
########################## Create DCA for subgroups ############################
################################################################################

# ==============================================================================
# AGE STRATIFIED DCA (With N and %)
# ==============================================================================

# CALCULATE STATS (Raw N, Weighted %)
age_stats <- data %>%
  group_by(Age_Group) %>%
  summarise(
    Raw_N = n(),
    Weighted_N = sum(weights_new, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Pct_Weighted = fix_percentages(Weighted_N, digits = 1),
    Group_Label = format_group_label(Age_Group, Raw_N, Pct_Weighted)
  )

# RUN DCA LOOP
age_groups <- unique(data$Age_Group)

age_stratified_nb_exact <- map_dfr(
  age_groups,
  ~ {
    current_age_group <- .x
    data_subset <- data %>% filter(Age_Group == current_age_group)

    # Calculate Weighted Prevalence for DCA
    prevalence <- weighted.mean(
      if_else(data_subset$Status24H == "Deceased", 1, 0),
      w = data_subset$weights_new,
      na.rm = TRUE
    )
    if (is.nan(prevalence) || prevalence == 0) {
      return(NULL)
    }
    cut_points <- prevalence * c(1, 2, 4, 8, 10, 20, 30)

    nb_current <- calculate_weighted_nb(
      data_subset,
      pred_current,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "NEWS")
    nb_light <- calculate_weighted_nb(
      data_subset,
      pred_light,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "Simplified NEWS")
    nb_full <- calculate_weighted_nb(
      data_subset,
      pred_full,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "DEWS")
    nb_xgb <- calculate_weighted_nb(
      data_subset,
      pred_xgb,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "XGB-EWS")
    nb_all <- tibble(
      threshold = cut_points,
      net_benefit = prevalence -
        (1 - prevalence) * (cut_points / (1 - cut_points)),
      model = "Treat All"
    )
    nb_none <- tibble(
      threshold = cut_points,
      net_benefit = 0,
      model = "Treat None"
    )

    bind_rows(nb_current, nb_light, nb_full, nb_xgb, nb_all, nb_none) %>%
      mutate(Age_Group = current_age_group)
  }
)

# GENERATE TABLE
final_age_table <- age_stratified_nb_exact %>%
  left_join(age_stats, by = "Age_Group") %>%
  group_by(Group_Label) %>%
  arrange(threshold, .by_group = TRUE) %>%
  select(threshold, model, net_benefit) %>%
  pivot_wider(names_from = model, values_from = net_benefit) %>%
  mutate(across(where(is.numeric) & !matches("threshold"), ~ .x * 10000)) %>%
  mutate(across(
    c(NEWS, `Simplified NEWS`, `XGB-EWS`, DEWS, `Treat All`, `Treat None`),
    ~ round(.x, 1)
  )) %>%
  mutate(
    diff1 = NEWS - `XGB-EWS`,
    diff2 = NEWS - `Simplified NEWS`,
    diff3 = DEWS - `XGB-EWS`,
    diff4 = NEWS - DEWS
  ) %>%
  select(
    threshold,
    `Treat All`,
    `Treat None`,
    NEWS,
    `Simplified NEWS`,
    DEWS,
    `XGB-EWS`,
    diff1,
    diff2,
    diff3,
    diff4
  ) %>%
  gt(groupname_col = "Group_Label") %>%
  tab_header(
    title = "Weighted Age-Stratified Decision Curve Analysis",
    subtitle = md(
      "Net benefit differences at exact prevalence-based thresholds. **Values are per 10,000 patients.**"
    )
  ) %>%
  cols_label(
    threshold = "Risk Threshold",
    `Treat All` = "Treat All",
    `Treat None` = "Treat None",
    NEWS = "NEWS",
    `Simplified NEWS` = "Simplified NEWS",
    DEWS = "DEWS",
    `XGB-EWS` = "XGB-EWS",
    diff1 = "NEWS vs XGB-EWS",
    diff2 = "NEWS vs Simplified NEWS",
    diff3 = "DEWS vs XGB-EWS",
    diff4 = "NEWS vs DEWS"
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
    columns = c(diff1, diff2, diff3, diff4)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(diff1, diff2, diff3, diff4))
  ) %>%
  tab_options(row_group.font.weight = "bold") %>%
  tab_footnote(
    footnote = "Positive values indicate the first model has higher net benefit.",
    locations = cells_column_spanners(spanners = contains("Difference"))
  )

final_age_table
final_age_table |> gtsave("weights_final_age_stratified_table_nov.docx")

# CALCULATE STATS
dept_stats <- data %>%
  filter(Department_Name_Fac != "Other") %>%
  group_by(Department_Name_Fac) %>%
  summarise(
    Raw_N = n(),
    Weighted_N = sum(weights_new, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Pct_Weighted = fix_percentages(Weighted_N, digits = 1),
    Group_Label = format_group_label(Department_Name_Fac, Raw_N, Pct_Weighted)
  )

# RUN DCA LOOP
dept_levels <- levels(droplevels(data$Department_Name_Fac[
  data$Department_Name_Fac != "Other"
]))
dept_nb_exact <- map_dfr(
  dept_levels,
  ~ {
    current_dept <- .x
    data_subset <- data %>% filter(Department_Name_Fac == current_dept)

    prevalence <- weighted.mean(
      if_else(data_subset$Status24H == "Deceased", 1, 0),
      w = data_subset$weights_new,
      na.rm = TRUE
    )
    if (is.nan(prevalence) || prevalence == 0) {
      return(NULL)
    }
    cut_points <- prevalence * c(1, 2, 4, 8, 10, 20, 30)

    nb_current <- calculate_weighted_nb(
      data_subset,
      pred_current,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "NEWS")
    nb_light <- calculate_weighted_nb(
      data_subset,
      pred_light,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "Simplified NEWS")
    nb_full <- calculate_weighted_nb(
      data_subset,
      pred_full,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "DEWS")
    nb_xgb <- calculate_weighted_nb(
      data_subset,
      pred_xgb,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "XGB-EWS")
    nb_all <- tibble(
      threshold = cut_points,
      net_benefit = prevalence -
        (1 - prevalence) * (cut_points / (1 - cut_points)),
      model = "Treat All"
    )
    nb_none <- tibble(
      threshold = cut_points,
      net_benefit = 0,
      model = "Treat None"
    )

    bind_rows(nb_current, nb_light, nb_full, nb_xgb, nb_all, nb_none) %>%
      mutate(Department_Name_Fac = current_dept)
  }
)

# GENERATE TABLE
final_dept_table <- dept_nb_exact %>%
  left_join(dept_stats, by = "Department_Name_Fac") %>%
  group_by(Group_Label) %>%
  arrange(threshold, .by_group = TRUE) %>%
  select(threshold, model, net_benefit) %>%
  pivot_wider(names_from = model, values_from = net_benefit) %>%
  mutate(across(where(is.numeric) & !matches("threshold"), ~ .x * 10000)) %>%
  mutate(across(
    c(NEWS, `Simplified NEWS`, `XGB-EWS`, DEWS, `Treat All`, `Treat None`),
    ~ round(.x, 1)
  )) %>%
  mutate(
    diff1 = NEWS - `XGB-EWS`,
    diff2 = NEWS - `Simplified NEWS`,
    diff3 = DEWS - `XGB-EWS`,
    diff4 = NEWS - DEWS
  ) %>%
  select(
    threshold,
    `Treat All`,
    `Treat None`,
    NEWS,
    `Simplified NEWS`,
    DEWS,
    `XGB-EWS`,
    diff1,
    diff2,
    diff3,
    diff4
  ) %>%
  gt(groupname_col = "Group_Label") %>%
  tab_header(
    title = "Department-Stratified Decision Curve Analysis",
    subtitle = md(
      "Net benefit differences at exact prevalence-based thresholds. **Values are per 10,000 patients.**"
    )
  ) %>%
  cols_label(
    threshold = "Risk Threshold",
    `Treat All` = "Treat All",
    `Treat None` = "Treat None",
    NEWS = "NEWS",
    `Simplified NEWS` = "Simplified NEWS",
    DEWS = "DEWS",
    `XGB-EWS` = "XGB-EWS",
    diff1 = "NEWS vs XGB-EWS",
    diff2 = "NEWS vs Simplified NEWS",
    diff3 = "DEWS vs XGB-EWS",
    diff4 = "NEWS vs DEWS"
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
    columns = c(diff1, diff2, diff3, diff4)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(diff1, diff2, diff3, diff4))
  ) %>%
  tab_options(row_group.font.weight = "bold") %>%
  tab_footnote(
    footnote = "Positive values indicate the first model has higher net benefit.",
    locations = cells_column_spanners(spanners = contains("Difference"))
  )

final_dept_table
final_dept_table |> gtsave("weights_dep_stratified_table_nov.docx")


# CALCULATE STATS
sex_stats <- data %>%
  group_by(Sex) %>%
  summarise(
    Raw_N = n(),
    Weighted_N = sum(weights_new, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Pct_Weighted = fix_percentages(Weighted_N, digits = 1),
    Group_Label = format_group_label(Sex, Raw_N, Pct_Weighted)
  )

# RUN DCA LOOP
sex_levels <- unique(data$Sex)
sex_nb_exact <- map_dfr(
  sex_levels,
  ~ {
    current_sex <- .x
    data_subset <- data %>% filter(Sex == current_sex)

    prevalence <- weighted.mean(
      if_else(data_subset$Status24H == "Deceased", 1, 0),
      w = data_subset$weights_new,
      na.rm = TRUE
    )
    if (is.nan(prevalence) || prevalence == 0) {
      return(NULL)
    }
    cut_points <- prevalence * c(1, 2, 4, 8, 10, 20, 30)

    nb_current <- calculate_weighted_nb(
      data_subset,
      pred_current,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "NEWS")
    nb_light <- calculate_weighted_nb(
      data_subset,
      pred_light,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "Simplified NEWS")
    nb_full <- calculate_weighted_nb(
      data_subset,
      pred_full,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "DEWS")
    nb_xgb <- calculate_weighted_nb(
      data_subset,
      pred_xgb,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "XGB-EWS")
    nb_all <- tibble(
      threshold = cut_points,
      net_benefit = prevalence -
        (1 - prevalence) * (cut_points / (1 - cut_points)),
      model = "Treat All"
    )
    nb_none <- tibble(
      threshold = cut_points,
      net_benefit = 0,
      model = "Treat None"
    )

    bind_rows(nb_current, nb_light, nb_full, nb_xgb, nb_all, nb_none) %>%
      mutate(Sex = current_sex)
  }
)

# GENERATE TABLE
final_sex_table <- sex_nb_exact %>%
  left_join(sex_stats, by = "Sex") %>%
  group_by(Group_Label) %>%
  arrange(threshold, .by_group = TRUE) %>%
  select(threshold, model, net_benefit) %>%
  pivot_wider(names_from = model, values_from = net_benefit) %>%
  mutate(across(where(is.numeric) & !matches("threshold"), ~ .x * 10000)) %>%
  mutate(across(
    c(NEWS, `Simplified NEWS`, `XGB-EWS`, DEWS, `Treat All`, `Treat None`),
    ~ round(.x, 1)
  )) %>%
  mutate(
    diff1 = NEWS - `XGB-EWS`,
    diff2 = NEWS - `Simplified NEWS`,
    diff3 = DEWS - `XGB-EWS`,
    diff4 = NEWS - DEWS
  ) %>%
  select(
    threshold,
    `Treat All`,
    `Treat None`,
    NEWS,
    `Simplified NEWS`,
    DEWS,
    `XGB-EWS`,
    diff1,
    diff2,
    diff3,
    diff4
  ) %>%
  gt(groupname_col = "Group_Label") %>%
  tab_header(
    title = "Sex-Stratified Decision Curve Analysis",
    subtitle = md(
      "Net benefit differences at exact prevalence-based thresholds. **Values are per 10,000 patients.**"
    )
  ) %>%
  cols_label(
    threshold = "Risk Threshold",
    `Treat All` = "Treat All",
    `Treat None` = "Treat None",
    NEWS = "NEWS",
    `Simplified NEWS` = "Simplified NEWS",
    DEWS = "DEWS",
    `XGB-EWS` = "XGB-EWS",
    diff1 = "NEWS vs XGB-EWS",
    diff2 = "NEWS vs Simplified NEWS",
    diff3 = "DEWS vs XGB-EWS",
    diff4 = "NEWS vs DEWS"
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
    columns = c(diff1, diff2, diff3, diff4)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(diff1, diff2, diff3, diff4))
  ) %>%
  tab_options(row_group.font.weight = "bold") %>%
  tab_footnote(
    footnote = "Positive values indicate the first model has higher net benefit.",
    locations = cells_column_spanners(spanners = contains("Difference"))
  )

final_sex_table
final_sex_table |> gtsave("weights_sex_stratified_table_nov.docx")

# DEFINE ICD-10 MAPPING
icd_mapping <- c(
  "Certain infectious and parasitic diseases" = "A00-B99",
  "Neoplasms" = "C00-D48",
  "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism" = "D50-D89",
  "Endocrine, nutritional and metabolic diseases" = "E00-E90",
  "Mental, Behavioral and Neurodevelopmental disorders" = "F00-F99",
  "Diseases of the nervous system" = "G00-G99",
  "Diseases of the eye and adnexa" = "H00-H59",
  "Diseases of the ear and mastoid process" = "H60-H95",
  "Diseases of the circulatory system" = "I00-I99",
  "Diseases of the respiratory system" = "J00-J99",
  "Diseases of the digestive system" = "K00-K93",
  "Diseases of the skin and subcutaneous tissue" = "L00-L99",
  "Diseases of the musculoskeletal system and connective tissue" = "M00-M99",
  "Diseases of the genitourinary system" = "N00-N99",
  "Pregnancy, childbirth and the puerperium" = "O00-O99",
  "Certain conditions originating in the perinatal period" = "P00-P96",
  "Congenital malformations, deformations and chromosomal abnormalities" = "Q00-Q99",
  "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified" = "R00-R99",
  "Injury, poisoning and certain other consequences of external causes" = "S00-T98",
  "External causes of morbidity" = "V01-Y98",
  "Factors influencing health status and contact with health services" = "Z00-Z99",
  "Codes for special purposes" = "U00-U85"
)

# Helper to format label with Code, N, and %
format_diag_label <- function(name, n, pct) {
  code <- icd_mapping[name]
  display_name <- ifelse(is.na(code), name, paste0(name, " [", code, "]"))

  paste0(
    display_name,
    " (N: ",
    format(n, big.mark = " ", trim = TRUE),
    ", ",
    format(pct, nsmall = 1, decimal.mark = "·", trim = TRUE),
    "%)"
  )
}

# CALCULATE STATS & LABELS
diag_levels <- unique(data$Diagnosis_Category[!is.na(data$Diagnosis_Category)])

diag_stats <- data %>%
  filter(Diagnosis_Category %in% diag_levels) %>%
  group_by(Diagnosis_Category) %>%
  summarise(
    Raw_N = n(),
    Weighted_N = sum(weights_new, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Pct_Weighted = fix_percentages(Weighted_N, digits = 1),
    # Use the specialized label function here
    Group_Label = map_chr(seq_along(Diagnosis_Category), function(i) {
      format_diag_label(Diagnosis_Category[i], Raw_N[i], Pct_Weighted[i])
    })
  )

# RUN DCA LOOP
diag_nb_exact <- map_dfr(
  diag_levels,
  ~ {
    current_cat <- .x
    data_subset <- data %>% filter(Diagnosis_Category == current_cat)

    prevalence <- weighted.mean(
      if_else(data_subset$Status24H == "Deceased", 1, 0),
      w = data_subset$weights_new,
      na.rm = TRUE
    )
    if (is.nan(prevalence) || prevalence == 0) {
      return(NULL)
    }

    cut_points <- prevalence * c(1, 2, 4, 8, 10, 20, 30)

    nb_current <- calculate_weighted_nb(
      data_subset,
      pred_current,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "NEWS")
    nb_light <- calculate_weighted_nb(
      data_subset,
      pred_light,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "Simplified NEWS")
    nb_full <- calculate_weighted_nb(
      data_subset,
      pred_full,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "DEWS")
    nb_xgb <- calculate_weighted_nb(
      data_subset,
      pred_xgb,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "XGB-EWS")
    nb_all <- tibble(
      threshold = cut_points,
      net_benefit = prevalence -
        (1 - prevalence) * (cut_points / (1 - cut_points)),
      model = "Treat All"
    )
    nb_none <- tibble(
      threshold = cut_points,
      net_benefit = 0,
      model = "Treat None"
    )

    bind_rows(nb_current, nb_light, nb_full, nb_xgb, nb_all, nb_none) %>%
      mutate(Diagnosis_Category = current_cat)
  }
)

# GENERATE TABLE
final_diag_table <- diag_nb_exact %>%
  left_join(diag_stats, by = "Diagnosis_Category") %>%
  group_by(Group_Label) %>%
  arrange(threshold, .by_group = TRUE) %>%
  select(threshold, model, net_benefit) %>%
  pivot_wider(names_from = model, values_from = net_benefit) %>%
  mutate(across(where(is.numeric) & !matches("threshold"), ~ .x * 10000)) %>%
  mutate(across(
    c(NEWS, `Simplified NEWS`, `XGB-EWS`, DEWS, `Treat All`, `Treat None`),
    ~ round(.x, 1)
  )) %>%
  mutate(
    diff1 = NEWS - `XGB-EWS`,
    diff2 = NEWS - `Simplified NEWS`,
    diff3 = DEWS - `XGB-EWS`,
    diff4 = NEWS - DEWS
  ) %>%
  select(
    threshold,
    `Treat All`,
    `Treat None`,
    NEWS,
    `Simplified NEWS`,
    DEWS,
    `XGB-EWS`,
    diff1,
    diff2,
    diff3,
    diff4
  ) %>%
  gt(groupname_col = "Group_Label") %>%
  tab_header(
    title = "Diagnosis-Stratified Decision Curve Analysis",
    subtitle = md(
      "Net benefit differences at exact prevalence-based thresholds. **Values are per 10,000 patients.**"
    )
  ) %>%
  cols_label(
    threshold = "Risk Threshold",
    `Treat All` = "Treat All",
    `Treat None` = "Treat None",
    NEWS = "NEWS",
    `Simplified NEWS` = "Simplified NEWS",
    DEWS = "DEWS",
    `XGB-EWS` = "XGB-EWS",
    diff1 = "NEWS vs XGB-EWS",
    diff2 = "NEWS vs Simplified NEWS",
    diff3 = "DEWS vs XGB-EWS",
    diff4 = "NEWS vs DEWS"
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
    columns = c(diff1, diff2, diff3, diff4)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(diff1, diff2, diff3, diff4))
  ) %>%
  tab_options(row_group.font.weight = "bold") %>%
  tab_footnote(
    footnote = "Positive values indicate the first model has higher net benefit.",
    locations = cells_column_spanners(spanners = contains("Difference"))
  )

final_diag_table
final_diag_table |> gtsave("weights_diagnoses_stratified_table_nov.docx")


# Create plot for NEWS Thresholds and Net Benefit Differences

# CALCULATE STATS

data$EWS_Grouping <- data$Risk_Groups_EWS

ews_stats <- data %>%
  group_by(EWS_Grouping) %>%
  summarise(
    Raw_N = n(),
    Weighted_N = sum(weights_new, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Pct_Weighted = fix_percentages(Weighted_N, digits = 1),
    Group_Label = format_group_label(EWS_Grouping, Raw_N, Pct_Weighted)
  )

# RUN DCA LOOP
ews_groups <- unique(data$EWS_Grouping)
ews_nb_exact <- map_dfr(
  ews_groups,
  ~ {
    current_group <- .x
    data_subset <- data %>% filter(EWS_Grouping == current_group)

    prevalence <- weighted.mean(
      if_else(data_subset$Status24H == "Deceased", 1, 0),
      w = data_subset$weights_new,
      na.rm = TRUE
    )
    if (is.nan(prevalence) || prevalence == 0) {
      return(NULL)
    }
    cut_points <- prevalence * c(1, 2, 4, 8, 10, 20, 30)

    nb_current <- calculate_weighted_nb(
      data_subset,
      pred_current,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "NEWS")
    nb_light <- calculate_weighted_nb(
      data_subset,
      pred_light,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "Simplified NEWS")
    nb_full <- calculate_weighted_nb(
      data_subset,
      pred_full,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "DEWS")
    nb_xgb <- calculate_weighted_nb(
      data_subset,
      pred_xgb,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "XGB-EWS")
    nb_all <- tibble(
      threshold = cut_points,
      net_benefit = prevalence -
        (1 - prevalence) * (cut_points / (1 - cut_points)),
      model = "Treat All"
    )
    nb_none <- tibble(
      threshold = cut_points,
      net_benefit = 0,
      model = "Treat None"
    )

    bind_rows(nb_current, nb_light, nb_full, nb_xgb, nb_all, nb_none) %>%
      mutate(EWS_Grouping = current_group)
  }
)

# GENERATE TABLE
final_ews_table <- ews_nb_exact %>%
  left_join(ews_stats, by = "EWS_Grouping") %>%
  group_by(Group_Label) %>%
  arrange(threshold, .by_group = TRUE) %>%
  select(threshold, model, net_benefit) %>%
  pivot_wider(names_from = model, values_from = net_benefit) %>%
  mutate(across(where(is.numeric) & !matches("threshold"), ~ .x * 10000)) %>%
  mutate(across(
    c(NEWS, `Simplified NEWS`, `XGB-EWS`, DEWS, `Treat All`, `Treat None`),
    ~ round(.x, 1)
  )) %>%
  mutate(
    diff1 = NEWS - `XGB-EWS`,
    diff2 = NEWS - `Simplified NEWS`,
    diff3 = DEWS - `XGB-EWS`,
    diff4 = NEWS - DEWS
  ) %>%
  select(
    threshold,
    `Treat All`,
    `Treat None`,
    NEWS,
    `Simplified NEWS`,
    DEWS,
    `XGB-EWS`,
    diff1,
    diff2,
    diff3,
    diff4
  ) %>%
  gt(groupname_col = "Group_Label") %>%
  tab_header(
    title = "EWS Risk Group-Stratified Decision Curve Analysis",
    subtitle = md(
      "Net benefit differences at exact prevalence-based thresholds. **Values are per 10,000 patients.**"
    )
  ) %>%
  cols_label(
    threshold = "Risk Threshold",
    `Treat All` = "Treat All",
    `Treat None` = "Treat None",
    NEWS = "NEWS",
    `Simplified NEWS` = "Simplified NEWS",
    DEWS = "DEWS",
    `XGB-EWS` = "XGB-EWS",
    diff1 = "NEWS vs XGB-EWS",
    diff2 = "NEWS vs Simplified NEWS",
    diff3 = "DEWS vs XGB-EWS",
    diff4 = "NEWS vs DEWS"
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
    columns = c(diff1, diff2, diff3, diff4)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(diff1, diff2, diff3, diff4))
  ) %>%
  tab_options(row_group.font.weight = "bold") %>%
  tab_footnote(
    footnote = "Positive values indicate the first model has higher net benefit.",
    locations = cells_column_spanners(spanners = contains("Difference"))
  )

final_ews_table


# -----------------
# HELPER FUNCTIONS
# -----------------

fix_percentages <- function(counts, digits = 1) {
  total <- sum(counts)
  raw_pct <- (counts / total) * 100
  factor <- 10^digits
  floored_pct <- floor(raw_pct * factor) / factor
  current_sum <- sum(floored_pct)
  remainder <- round(100 - current_sum, digits)
  fractional_parts <- raw_pct - floored_pct
  n_units <- round(remainder * factor)
  if (n_units > 0) {
    indices <- order(fractional_parts, decreasing = TRUE)[1:n_units]
    floored_pct[indices] <- floored_pct[indices] + (1 / factor)
  }
  return(floored_pct)
}

# -------------------------------------
# DATA PREPARATION & LABEL GENERATION
# -------------------------------------

# Define the data levels
risk_order <- c("High", "Medium", "Low-Medium", "Low")

# Define the EXPLICIT DISPLAY NAMES mapping
explicit_names <- c(
  "High" = "High risk group<br>(NEWS ≥ 7)",
  "Medium" = "Medium risk group<br>(NEWS 5-6)",
  "Low-Medium" = "Low-medium risk group<br>(Score of 3 in any parameter)",
  "Low" = "Low risk group<br>(NEWS 0-4)"
)

# Clean and Filter Data
clean_data <- data %>%
  filter(!is.na(EWS_Grouping)) %>%
  filter(EWS_Grouping %in% risk_order) %>%
  mutate(EWS_Grouping = factor(EWS_Grouping, levels = risk_order))

# GENERATE RICH HTML STRIP LABELS
label_stats <- clean_data %>%
  group_by(EWS_Grouping) %>%
  summarise(
    Raw_N = n(),
    Sum_Weights = sum(weights_new, na.rm = TRUE),
    Mort_Pct = weighted.mean(
      if_else(Status24H == "Deceased", 1, 0),
      w = weights_new,
      na.rm = TRUE
    ) *
      100
  ) %>%
  ungroup() %>%
  mutate(Freq_Pct = fix_percentages(Sum_Weights, digits = 1)) %>%
  mutate(
    Long_Name = explicit_names[as.character(EWS_Grouping)],
    Strip_Label = paste0(
      "<span style='font-size:12pt; color:black'>**",
      Long_Name,
      "**</span><br><br>",
      "<span style='font-size:11pt; color:#404040'>",
      "Frequency: ",
      format(Freq_Pct, nsmall = 1, decimal.mark = "·", trim = TRUE),
      "% ",
      "(N: ",
      format(Raw_N, big.mark = " ", trim = TRUE),
      ")<br>",
      "24H Mortality: ",
      format(round(Mort_Pct, 2), nsmall = 2, decimal.mark = "·", trim = TRUE),
      "%",
      "</span>"
    )
  )

strip_label_map <- setNames(label_stats$Strip_Label, label_stats$EWS_Grouping)

# ---------------------------------
# CALCULATION LOOP (Standard)
# ---------------------------------

fine_thresholds <- seq(0, 0.5, length.out = 200)

delta_data <- map_dfr(
  levels(clean_data$EWS_Grouping),
  ~ {
    current_group <- .x
    data_subset <- clean_data %>% filter(EWS_Grouping == current_group)
    if (nrow(data_subset) == 0) {
      return(NULL)
    }

    w_prev <- weighted.mean(
      if_else(data_subset$Status24H == "Deceased", 1, 0),
      w = data_subset$weights_new,
      na.rm = TRUE
    )

    if (is.nan(w_prev) || w_prev == 0) {
      return(NULL)
    }

    nb_news <- calculate_weighted_nb(
      data_subset,
      pred_current,
      Status24H,
      weights_new,
      fine_thresholds
    ) %>%
      mutate(model = "NEWS")
    nb_light <- calculate_weighted_nb(
      data_subset,
      pred_light,
      Status24H,
      weights_new,
      fine_thresholds
    ) %>%
      mutate(model = "Simplified NEWS")
    nb_dews <- calculate_weighted_nb(
      data_subset,
      pred_full,
      Status24H,
      weights_new,
      fine_thresholds
    ) %>%
      mutate(model = "DEWS")
    nb_xgb <- calculate_weighted_nb(
      data_subset,
      pred_xgb,
      Status24H,
      weights_new,
      fine_thresholds
    ) %>%
      mutate(model = "XGB-EWS")

    bind_rows(nb_news, nb_light, nb_dews, nb_xgb) %>%
      select(threshold, model, net_benefit) %>%
      pivot_wider(names_from = model, values_from = net_benefit) %>%
      mutate(
        `XGB-EWS vs NEWS` = `XGB-EWS` - NEWS,
        `DEWS vs NEWS` = DEWS - NEWS,
        `Simplified NEWS vs NEWS` = `Simplified NEWS` - NEWS
      ) %>%
      select(threshold, contains(" vs ")) %>%
      pivot_longer(
        cols = contains(" vs "),
        names_to = "comparison",
        values_to = "delta_nb"
      ) %>%
      mutate(EWS_Grouping = current_group, group_prevalence = w_prev)
  }
)

plot_data <- delta_data %>%
  mutate(
    delta_nb_scaled = delta_nb * 10000,
    gain = ifelse(delta_nb_scaled > 0, delta_nb_scaled, 0),
    loss = ifelse(delta_nb_scaled < 0, delta_nb_scaled, 0)
  )

plot_data$comparison <- factor(
  plot_data$comparison,
  levels = c("XGB-EWS vs NEWS", "DEWS vs NEWS", "Simplified NEWS vs NEWS")
)
plot_data$EWS_Grouping <- factor(plot_data$EWS_Grouping, levels = risk_order)

# ---------------------
# ANNOTATION LOGIC
# ---------------------
panel_annotations <- plot_data %>%
  group_by(EWS_Grouping, comparison) %>%
  summarize(
    ceiling_val = max(delta_nb_scaled, na.rm = TRUE),
    floor_val = min(delta_nb_scaled, na.rm = TRUE),
    mag_label = ifelse(
      abs(ceiling_val) > abs(floor_val),
      paste0("Max Gain: +", format(round(ceiling_val, 1), decimal.mark = "·")),
      paste0("Max Loss: ", format(round(floor_val, 1), decimal.mark = "·"))
    ),
    .groups = "drop"
  )

# ----------------
# PLOTTING
# ----------------

p_clean <- ggplot(plot_data, aes(x = threshold)) +
  geom_hline(yintercept = 0, color = "gray40", linewidth = 0.6) +
  geom_ribbon(aes(ymin = 0, ymax = gain), fill = "#2E86AB", alpha = 0.7) +
  geom_ribbon(aes(ymin = loss, ymax = 0), fill = "#C73E1D", alpha = 0.7) +
  geom_line(aes(y = delta_nb_scaled), color = "black", linewidth = 0.4) +
  geom_label(
    data = panel_annotations,
    aes(x = 0.5, y = Inf, label = mag_label),
    hjust = 1,
    vjust = 1.2,
    size = 4.5,
    fontface = "bold",
    fill = "white",
    alpha = 0.8,
    label.size = 0
  ) +
  facet_grid(
    EWS_Grouping ~ comparison,
    scales = "free_y",
    switch = "y",
    labeller = labeller(EWS_Grouping = as_labeller(strip_label_map))
  ) +
  scale_x_continuous(
    labels = function(x) format(x, decimal.mark = "·"),
    expand = expansion(mult = c(0.02, 0.05)),
    name = "Risk Threshold"
  ) +
  scale_y_continuous(
    labels = function(x) {
      map_chr(x, function(val) {
        if (is.na(val)) {
          return(NA)
        }
        if (abs(val) < 5 && abs(val) > 0.01) {
          format(round(val, 1), decimal.mark = "·")
        } else {
          format(round(val, 0), big.mark = " ")
        }
      })
    },
    breaks = scales::pretty_breaks(n = 5),
    expand = expansion(mult = c(0.1, 0.35)),
    name = expression(paste(Delta, " Net Benefit (per 10 000 patients)"))
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_blank(),
    panel.spacing.y = unit(2.5, "lines"),
    panel.spacing.x = unit(1.5, "lines"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_markdown(
      angle = 0,
      hjust = 1,
      vjust = 1,
      size = 11,
      lineheight = 1.4,
      margin = margin(r = 15, t = 10, b = 10)
    ),
    strip.text.x = element_text(
      face = "bold",
      size = 13,
      margin = margin(t = 8, b = 8)
    ),
    axis.title.x = element_text(face = "plain", margin = margin(t = 20)),
    axis.title.y = element_text(face = "plain", margin = margin(r = 20)),
    axis.text = element_text(color = "black", size = 11),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "gray30", fill = NA, linewidth = 0.5)
  )

print(p_clean)

##################

#### Creation of table with missingness (we need the unimputed data)

df <- read_parquet("/home/alex/ews/NEWS2_Evaluation/single_ews_v2.parquet")

# Now I need to keep only those individuals who were in my modelling workflow

df <- df |> filter(CSN %in% data$CSN)

df <- df |>
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

# And now for the missingness percentage

df <- df |>
  left_join(
    data |>
      select(PT_ID, CSN, starts_with("median"), previous_icu_respiratory),
    by = c("PT_ID", "CSN")
  )


missingness_table <- df %>%
  select(
    Age,
    Sex,
    Previous_Hosp,
    Hospital,
    Consciousness,
    Oxygen_Supplement,
    Respiration_Rate,
    Pulse,
    Temperature,
    Saturation,
    Blood_Pressure.Sys,
    Blood_Pressure.Dia,
    previous_icu_respiratory,
    EWS_score,
    median_Hemoglobin,
    median_Leukocytter,
    median_Trombocytter,
    median_Kreatinin,
    median_ALAT,
    median_LDH,
    median_Albumin,
    median_CRP,
    median_Laktat_ab,
    median_Troponin_T,
    median_Laktat_vb
  ) %>%
  summarise_all(~ sum(is.na(.))) %>%
  pivot_longer(
    everything(),
    names_to = "Variable",
    values_to = "Missing_Count"
  ) %>%
  mutate(
    Total_N = nrow(df),
    Missing_Percentage = (Missing_Count / Total_N) * 100,
    # Add polished variable names
    Variable_Label = case_when(
      Variable == "Age" ~ "Age",
      Variable == "Sex" ~ "Sex",
      Variable == "Previous_Hosp" ~ "Previous hospitalizations",
      Variable == "Hospital" ~ "Hospital",
      Variable == "Consciousness" ~ "Consciousness",
      Variable == "Oxygen_Supplement" ~ "Supplemental oxygen",
      Variable == "Respiration_Rate" ~ "Respiratory rate (breaths/min)",
      Variable == "Pulse" ~ "Pulse (beats/min)",
      Variable == "Temperature" ~ "Temperature (°C)",
      Variable == "Saturation" ~ "Oxygen saturation (%)",
      Variable == "Blood_Pressure.Sys" ~ "Systolic blood pressure (mm Hg)",
      Variable == "Blood_Pressure.Dia" ~ "Diastolic blood pressure (mm Hg)",
      Variable == "previous_icu_respiratory" ~
        "Previous ICU/respiratory support",
      Variable == "EWS_score" ~ "NEWS score",
      Variable == "median_Hemoglobin" ~ "Hemoglobin (g/L)",
      Variable == "median_Leukocytter" ~ "Leukocytes (×10⁹/L)",
      Variable == "median_Trombocytter" ~ "Platelets (×10⁹/L)",
      Variable == "median_Kreatinin" ~ "Creatinine (μmol/L)",
      Variable == "median_ALAT" ~ "Alanine aminotransferase (U/L)",
      Variable == "median_LDH" ~ "Lactate dehydrogenase (U/L)",
      Variable == "median_Albumin" ~ "Albumin (g/L)",
      Variable == "median_CRP" ~ "C-reactive protein (mg/L)",
      Variable == "median_Laktat_ab" ~ "Lactate, arterial blood (mmol/L)",
      Variable == "median_Troponin_T" ~ "Troponin T (ng/L)",
      Variable == "median_Laktat_vb" ~ "Lactate, venous blood (mmol/L)",
      TRUE ~ Variable
    )
  ) %>%
  select(Variable_Label, Missing_Count, Missing_Percentage, Total_N) %>%
  gt() %>%
  tab_header(
    title = "Missing Data Summary",
    subtitle = "Absolute numbers and percentages of missing values by variable"
  ) %>%
  cols_label(
    Variable_Label = "Variable",
    Missing_Count = "Missing (N)",
    Missing_Percentage = "Missing (%)",
    Total_N = "Total N"
  ) %>%
  fmt_number(
    columns = Missing_Percentage,
    decimals = 1,
    dec_mark = "·",
    sep_mark = " "
  ) %>%
  fmt_number(
    columns = c(Missing_Count, Total_N),
    decimals = 0,
    sep_mark = " "
  ) %>%
  cols_align(
    align = "center",
    columns = c(Missing_Count, Missing_Percentage, Total_N)
  ) %>%
  cols_align(align = "left", columns = Variable_Label)

# Display the table

missingness_table

gtsave(missingness_table, "missingness_table.docx")

#################################################

####### Supplementary Analyses ############

# 48-Hour mortality

data <- data |>
  mutate(Status48H = if_else(mort48H == 0, "Alive", "Deceased")) |>
  mutate_at(vars(Status48H), as.factor)

data$Status48H <- relevel(data$Status48H, "Deceased")

set.seed(234)

data_folds <- rsample::group_vfold_cv(data, group = Hospital)

# Now we will create our model

model <- logistic_reg(engine = "glm", mode = "classification")

# Let's also create an xgboost model

xgb <- boost_tree(trees = 100, mtry = 30) |>
  set_engine("xgboost") |>
  set_mode("classification")

# We reuse the existing data_folds, model (glm), and xgb (spec) objects
# as they are generic definitions/splits.

############## Workflows (Renamed for 48H) ###############

# Current model (NEWS) - 48H
current_wf_48h <- workflow() |>
  add_formula(Status48H ~ EWS_score) |>
  add_model(model) |>
  add_case_weights(imp_weights)

# NEWS-Light - 48H
light_wf_48h <- workflow() |>
  add_formula(Status48H ~ EWS_light) |>
  add_model(model) |>
  add_case_weights(imp_weights)

# DEWS (IEWS) - 48H
full_wf_48h <- workflow() |>
  add_formula(Status48H ~ IEWS_Light) |>
  add_model(model) |>
  add_case_weights(imp_weights)

# XGBoost - 48H
xgb_wf_48h <- workflow() |>
  add_formula(
    Status48H ~
      Age +
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
  ) |>
  add_model(xgb) |>
  add_case_weights(imp_weights)

# Set up parallel processing
doParallel::registerDoParallel(cores = 40)

cntrl <- control_resamples(save_pred = T)

# Grouped cross validations (Renamed Fits)

current_fit_48h <- fit_resamples(
  current_wf_48h,
  resamples = data_folds,
  metrics = metric_set(roc_auc, brier_class),
  control = cntrl
)

light_fit_48h <- fit_resamples(
  light_wf_48h,
  resamples = data_folds,
  metrics = metric_set(roc_auc, brier_class),
  control = cntrl
)

full_fit_48h <- fit_resamples(
  full_wf_48h,
  resamples = data_folds,
  metrics = metric_set(roc_auc, brier_class),
  control = cntrl
)

set.seed(234)

xgb_fit_48h <- fit_resamples(
  xgb_wf_48h,
  resamples = data_folds,
  metrics = metric_set(roc_auc, brier_class),
  control = cntrl
)

# Generate bootstrapped predictions
# Renamed function to avoid conflict with 24h version

calculate_overall_bootstrap_ci_48h <- function(
  fit_object,
  original_data,
  model_name,
  n_bootstrap = 200
) {
  preds_df <- fit_object %>%
    collect_predictions() %>%
    arrange(.row) %>%
    mutate(
      Status48H = original_data$Status48H,
      weights_new = original_data$weights_new,
      mort48H = if_else(Status48H == "Deceased", 1, 0)
    )

  set.seed(42)

  boot_results <- map_dfr(
    1:n_bootstrap,
    ~ {
      boot_indices <- sample(1:nrow(preds_df), nrow(preds_df), replace = TRUE)
      boot_sample <- preds_df[boot_indices, ]

      auc_val <- MetricsWeighted::AUC(
        actual = boot_sample$mort48H,
        predicted = boot_sample$.pred_Deceased,
        w = boot_sample$weights_new
      )
      brier_val <- MetricsWeighted::mse(
        actual = boot_sample$mort48H,
        predicted = boot_sample$.pred_Deceased,
        w = boot_sample$weights_new
      )

      tibble(
        bootstrap_id = .x,
        auc = auc_val,
        brier = brier_val
      )
    }
  )

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
    mutate(model = model_name)

  return(summary_metrics)
}

# Run the Analysis
n_boot <- 200

possibly_bootstrap_48h <- possibly(
  calculate_overall_bootstrap_ci_48h,
  otherwise = NULL
)

all_model_performance_48h <- bind_rows(
  possibly_bootstrap_48h(current_fit_48h, data, "NEWS", n_bootstrap = n_boot),
  possibly_bootstrap_48h(
    light_fit_48h,
    data,
    "Simplified NEWS",
    n_bootstrap = n_boot
  ),
  possibly_bootstrap_48h(full_fit_48h, data, "DEWS", n_bootstrap = n_boot),
  possibly_bootstrap_48h(xgb_fit_48h, data, "XGB-EWS", n_bootstrap = n_boot)
)

# Create the Plots

auc_p_48h <- all_model_performance_48h %>%
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
  labs(
    x = NULL,
    y = "Area Under the Curve (AUC) - 48 Hours"
  ) +
  scale_fill_lancet() +
  scale_color_lancet() +
  scale_y_lancet(digits = 5) +
  coord_cartesian(
    ylim = c(
      min(all_model_performance_48h$.lower[
        all_model_performance_48h$.metric == "auc"
      ]) -
        0.005,
      NA
    )
  )

brier_p_48h <- all_model_performance_48h %>%
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
  scale_fill_lancet() +
  scale_color_lancet() +
  scale_y_lancet(digits = 5) +
  labs(
    x = NULL,
    y = "Brier Score - 48 Hours"
  )

# Combine and Print Final Plot
final_plot_48h <- auc_p_48h +
  brier_p_48h +
  plot_annotation(
    title = "48-Hour Mortality Prediction Performance",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
  )

final_plot_48h


# Now let's do the net benefit difference across the models

# ==============================================================================
# EXTRACT AND ALIGN PREDICTIONS
# ==============================================================================

# Extract predictions and sort by original row index to align with 'data'
current_preds_48h <- current_fit_48h |>
  collect_predictions() |>
  arrange(.row) |>
  pull(.pred_Deceased)

light_preds_48h <- light_fit_48h |>
  collect_predictions() |>
  arrange(.row) |>
  pull(.pred_Deceased)

full_preds_48h <- full_fit_48h |>
  collect_predictions() |>
  arrange(.row) |>
  pull(.pred_Deceased)

xgb_preds_48h <- xgb_fit_48h |>
  collect_predictions() |>
  arrange(.row) |>
  pull(.pred_Deceased)

# Safely mutate them into the main dataframe
data <- data |>
  mutate(
    pred_current_48h = current_preds_48h,
    pred_light_48h = light_preds_48h,
    pred_full_48h = full_preds_48h,
    pred_xgb_48h = xgb_preds_48h
  )

# ==============================================================================
# CALCULATE WEIGHTED PREVALENCE (48H)
# ==============================================================================

# First compute the weighted prevalence of mortality (48H)
weighted_prevalence_48h <- weighted.mean(
  x = if_else(data$Status48H == "Deceased", 1, 0),
  w = data$weights_new,
  na.rm = TRUE
)

# Compute the multiples of the prevalence to use as thresholds
cut_points_48h <- weighted_prevalence_48h * c(1, 2, 4, 8, 10, 20, 30)

print("Weighted Prevalence of Mortality (48H):")
print(weighted_prevalence_48h)

# =========================
# CALCULATE NET BENEFIT
# =========================

# Apply function using the columns we just added to 'data'

nb_at_cuts_current <- calculate_weighted_nb(
  data,
  pred_current_48h,
  Status48H,
  weights_new,
  cut_points_48h
) %>%
  mutate(model = "NEWS")

nb_at_cuts_light <- calculate_weighted_nb(
  data,
  pred_light_48h,
  Status48H,
  weights_new,
  cut_points_48h
) %>%
  mutate(model = "Simplified NEWS")

nb_at_cuts_full <- calculate_weighted_nb(
  data,
  pred_full_48h,
  Status48H,
  weights_new,
  cut_points_48h
) %>%
  mutate(model = "DEWS")

nb_at_cuts_xgb <- calculate_weighted_nb(
  data,
  pred_xgb_48h,
  Status48H,
  weights_new,
  cut_points_48h
) %>%
  mutate(model = "XGB-EWS")

# Treat All Strategy
nb_at_cuts_all <- tibble(
  threshold = cut_points_48h,
  net_benefit = weighted_prevalence_48h -
    (1 - weighted_prevalence_48h) * (cut_points_48h / (1 - cut_points_48h)),
  model = "Treat All"
)

# Treat None Strategy
nb_at_cuts_none <- tibble(
  threshold = cut_points_48h,
  net_benefit = 0,
  model = "Treat None"
)

# ==============================================================================
# COMBINE AND FORMAT TABLE
# ==============================================================================

# Combine precise calculations
nb_at_exact_cuts_48h <- bind_rows(
  nb_at_cuts_current,
  nb_at_cuts_light,
  nb_at_cuts_full,
  nb_at_cuts_xgb,
  nb_at_cuts_all,
  nb_at_cuts_none
)

# Pivot, Scale, and Calculate Differences
comparison_data_consistent <- nb_at_exact_cuts_48h %>%
  select(threshold, model, net_benefit) %>%
  pivot_wider(names_from = model, values_from = net_benefit) %>%
  # Scale by 10,000 to get values per 10,000 patients
  mutate(across(where(is.numeric) & !matches("threshold"), ~ .x * 10000)) %>%
  mutate(
    across(
      .cols = c(
        NEWS,
        `Simplified NEWS`,
        `XGB-EWS`,
        `DEWS`,
        `Treat All`,
        `Treat None`
      ),
      .fns = ~ round(.x, digits = 1)
    )
  ) %>%
  mutate(
    # 1. NEWS vs XGB-EWS
    difference1 = NEWS - `XGB-EWS`,

    # 2. NEWS vs Simplified NEWS
    difference2 = NEWS - `Simplified NEWS`,

    # 3. DEWS vs XGB-EWS
    difference3 = DEWS - `XGB-EWS`,

    # 4. NEWS vs DEWS
    difference4 = NEWS - DEWS
  ) %>%
  select(
    threshold,
    `Treat All`,
    `Treat None`,
    NEWS,
    `Simplified NEWS`,
    `DEWS`,
    `XGB-EWS`,
    difference1,
    difference2,
    difference3,
    difference4
  ) %>%
  arrange(threshold)

# Create the Final gt Table
final_comparison_table_48h <- comparison_data_consistent %>%
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
  gt() %>%
  tab_header(
    title = "Decision Curve Analysis: Net Benefit Comparison (48 Hours)",
    subtitle = "Comparing models at exact prevalence-based thresholds. Values are per 10,000 patients."
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

# Display
final_comparison_table_48h

# Save it

final_comparison_table_48h |>
  gtsave("weights_final_comparison_table_nov_48_v2.docx")


###### 72-Hour Mortality Analysis #######

data <- data |>
  mutate(
    # Calculate 72-hour threshold
    limit_72h = recorded_time + hours(72),

    # Create binary flag (1 = Died within 72h, 0 = Alive or died later)
    mort72H = if_else(
      !is.na(deathDate) & deathDate <= limit_72h,
      1,
      0
    ),
    Status72H = if_else(mort72H == 1, "Deceased", "Alive") |>
      factor(levels = c("Deceased", "Alive"))
  )

# Verify the counts
print(table(data$Status72H))

set.seed(234)

data_folds <- rsample::group_vfold_cv(data, group = Hospital)

# Now we will create our model

model <- logistic_reg(engine = "glm", mode = "classification")

# Let's also create an xgboost model

xgb <- boost_tree(trees = 100, mtry = 30) |>
  set_engine("xgboost") |>
  set_mode("classification")


# Update Workflows for 72H
current_wf_72h <- workflow() |>
  add_formula(Status72H ~ EWS_score) |>
  add_model(model) |>
  add_case_weights(imp_weights)

light_wf_72h <- workflow() |>
  add_formula(Status72H ~ EWS_light) |>
  add_model(model) |>
  add_case_weights(imp_weights)

full_wf_72h <- workflow() |>
  add_formula(Status72H ~ IEWS_Light) |>
  add_model(model) |>
  add_case_weights(imp_weights)

# Note: Ensure the XGBoost formula uses the exact same predictors as before
xgb_wf_72h <- workflow() |>
  add_formula(
    Status72H ~ Age +
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
  ) |>
  add_model(xgb) |>
  add_case_weights(imp_weights)

# Set up parallel processing
doParallel::registerDoParallel(cores = 40)

cntrl <- control_resamples(save_pred = T)

# Fit Models
current_fit_72h <- fit_resamples(
  current_wf_72h,
  resamples = data_folds,
  metrics = metric_set(roc_auc, brier_class),
  control = cntrl
)
light_fit_72h <- fit_resamples(
  light_wf_72h,
  resamples = data_folds,
  metrics = metric_set(roc_auc, brier_class),
  control = cntrl
)
full_fit_72h <- fit_resamples(
  full_wf_72h,
  resamples = data_folds,
  metrics = metric_set(roc_auc, brier_class),
  control = cntrl
)

set.seed(234)
xgb_fit_72h <- fit_resamples(
  xgb_wf_72h,
  resamples = data_folds,
  metrics = metric_set(roc_auc, brier_class),
  control = cntrl
)

# Extract Predictions
current_preds_72h <- collect_predictions(current_fit_72h) |>
  arrange(.row) |>
  pull(.pred_Deceased)
light_preds_72h <- collect_predictions(light_fit_72h) |>
  arrange(.row) |>
  pull(.pred_Deceased)
full_preds_72h <- collect_predictions(full_fit_72h) |>
  arrange(.row) |>
  pull(.pred_Deceased)
xgb_preds_72h <- collect_predictions(xgb_fit_72h) |>
  arrange(.row) |>
  pull(.pred_Deceased)

# Store in Dataframe
data <- data |>
  mutate(
    pred_current_72h = current_preds_72h,
    pred_light_72h = light_preds_72h,
    pred_full_72h = full_preds_72h,
    pred_xgb_72h = xgb_preds_72h
  )
# ========================
# Calculate Net Benefit
# ========================

# 1. Weighted Prevalence (72H)
weighted_prevalence_72h <- weighted.mean(
  x = if_else(data$Status72H == "Deceased", 1, 0),
  w = data$weights_new,
  na.rm = TRUE
)
cut_points_72h <- weighted_prevalence_72h * c(1, 2, 4, 8, 10, 20, 30)

print(paste("Weighted 72H Prevalence:", round(weighted_prevalence_72h, 5)))

# 2. Calculate NB
nb_at_cuts_current <- calculate_weighted_nb(
  data,
  pred_current_72h,
  Status72H,
  weights_new,
  cut_points_72h
) %>%
  mutate(model = "NEWS")
nb_at_cuts_light <- calculate_weighted_nb(
  data,
  pred_light_72h,
  Status72H,
  weights_new,
  cut_points_72h
) %>%
  mutate(model = "Simplified NEWS")
nb_at_cuts_full <- calculate_weighted_nb(
  data,
  pred_full_72h,
  Status72H,
  weights_new,
  cut_points_72h
) %>%
  mutate(model = "DEWS")
nb_at_cuts_xgb <- calculate_weighted_nb(
  data,
  pred_xgb_72h,
  Status72H,
  weights_new,
  cut_points_72h
) %>%
  mutate(model = "XGB-EWS")

# 3. Baselines
nb_at_cuts_all <- tibble(
  threshold = cut_points_72h,
  net_benefit = weighted_prevalence_72h -
    (1 - weighted_prevalence_72h) * (cut_points_72h / (1 - cut_points_72h)),
  model = "Treat All"
)
nb_at_cuts_none <- tibble(
  threshold = cut_points_72h,
  net_benefit = 0,
  model = "Treat None"
)

# 4. Combine
nb_at_exact_cuts_72h <- bind_rows(
  nb_at_cuts_current,
  nb_at_cuts_light,
  nb_at_cuts_full,
  nb_at_cuts_xgb,
  nb_at_cuts_all,
  nb_at_cuts_none
)

# ==============================================================================
#  Format Table (GT Pipeline)
# ==============================================================================

comparison_data_consistent <- nb_at_exact_cuts_72h %>%
  select(threshold, model, net_benefit) %>%
  pivot_wider(names_from = model, values_from = net_benefit) %>%
  mutate(across(where(is.numeric) & !matches("threshold"), ~ .x * 10000)) %>%
  mutate(across(
    .cols = c(
      NEWS,
      `Simplified NEWS`,
      `XGB-EWS`,
      DEWS,
      `Treat All`,
      `Treat None`
    ),
    ~ round(.x, digits = 1)
  )) %>%
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

final_comparison_table_72h <- comparison_data_consistent %>%
  gt() %>%
  tab_header(
    title = "Decision Curve Analysis: Net Benefit Comparison (72 Hours)",
    subtitle = "Comparing models at exact prevalence-based thresholds. Values are per 10,000 patients."
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

final_comparison_table_72h

final_comparison_table_72h |>
  gtsave("weights_final_comparison_table_nov_72_v2.docx")

# Bootstrapping for performance metrics

# Custom function for 72H outcomes
calculate_overall_bootstrap_ci_72h <- function(
  fit_object,
  original_data,
  model_name,
  n_bootstrap = 200
) {
  preds_df <- fit_object %>%
    collect_predictions() %>%
    arrange(.row) %>%
    mutate(
      Status72H = original_data$Status72H,
      weights_new = original_data$weights_new,
      mort72H = if_else(Status72H == "Deceased", 1, 0)
    )

  set.seed(42)
  boot_results <- map_dfr(
    1:n_bootstrap,
    ~ {
      boot_indices <- sample(1:nrow(preds_df), nrow(preds_df), replace = TRUE)
      boot_sample <- preds_df[boot_indices, ]

      tibble(
        bootstrap_id = .x,
        auc = MetricsWeighted::AUC(
          actual = boot_sample$mort72H,
          predicted = boot_sample$.pred_Deceased,
          w = boot_sample$weights_new
        ),
        brier = MetricsWeighted::mse(
          actual = boot_sample$mort72H,
          predicted = boot_sample$.pred_Deceased,
          w = boot_sample$weights_new
        )
      )
    }
  )

  boot_results %>%
    pivot_longer(
      cols = c(auc, brier),
      names_to = ".metric",
      values_to = "estimate"
    ) %>%
    group_by(.metric) %>%
    summarise(
      .estimate = mean(estimate, na.rm = TRUE),
      .lower = quantile(estimate, 0.025, na.rm = TRUE),
      .upper = quantile(estimate, 0.975, na.rm = TRUE)
    ) %>%
    mutate(model = model_name)
}

# Run Bootstrap
n_boot <- 200
possibly_bootstrap_72h <- possibly(
  calculate_overall_bootstrap_ci_72h,
  otherwise = NULL
)

all_model_performance_72h <- bind_rows(
  possibly_bootstrap_72h(current_fit_72h, data, "NEWS", n_bootstrap = n_boot),
  possibly_bootstrap_72h(
    light_fit_72h,
    data,
    "Simplified NEWS",
    n_bootstrap = n_boot
  ),
  possibly_bootstrap_72h(full_fit_72h, data, "DEWS", n_bootstrap = n_boot),
  possibly_bootstrap_72h(xgb_fit_72h, data, "XGB-EWS", n_bootstrap = n_boot)
)

# Create Plots
auc_p_72h <- all_model_performance_72h %>%
  filter(.metric == "auc") %>%
  ggplot(aes(
    x = fct_reorder(model, .estimate, .desc = TRUE),
    y = .estimate,
    color = model
  )) +
  geom_pointinterval(
    aes(ymin = .lower, ymax = .upper),
    point_size = 4,
    interval_size = 1.2
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = NULL, y = "AUC (72-Hour)") +
  scale_color_lancet() +
  coord_cartesian(
    ylim = c(
      min(all_model_performance_72h$.lower[
        all_model_performance_72h$.metric == "auc"
      ]) -
        0.005,
      NA
    )
  )

brier_p_72h <- all_model_performance_72h %>%
  filter(.metric == "brier") %>%
  ggplot(aes(x = fct_reorder(model, .estimate), y = .estimate, color = model)) +
  geom_pointinterval(
    aes(ymin = .lower, ymax = .upper),
    point_size = 4,
    interval_size = 1.2
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = NULL, y = "Brier Score (72-Hour)") +
  scale_color_lancet()

final_plot_72h <- auc_p_72h +
  brier_p_72h +
  plot_annotation(title = "72-Hour Mortality Prediction Performance")
print(final_plot_72h)


########## Sensitivity Analysis for > 60 min length of stay ##############

# PREPARE DATA & STATS
total_n <- nrow(data)

# Define Sensitivity Cohort (>= 60 min)
data_sens <- data %>%
  filter(as.numeric(Hosp_spent_time, units = "secs") >= 3600)

# Removed 18,019 assessments
# Count unique patients and hospitalizations

length(unique(data_sens$PT_ID)) # 816,912 individuals
length(unique(data_sens$CSN)) # 2,062,746 encounters

# Calculate Stats
n_sens <- nrow(data_sens)

# Explicitly round to 1 decimal place here
pct_sens <- round((n_sens / total_n) * 100, 1)

# Create Label (Now uses the rounded percentage)
label_sens <- format_group_label(
  "Cohort with Length of Stay > 60 min",
  n_sens,
  pct_sens
)

# RUN DCA (On Sensitivity Cohort)
prev_sens <- weighted.mean(
  if_else(data_sens$Status24H == "Deceased", 1, 0),
  w = data_sens$weights_new,
  na.rm = TRUE
)

cut_points <- prev_sens * c(1, 2, 4, 8, 10, 20, 30)

nb_sens <- bind_rows(
  calculate_weighted_nb(
    data_sens,
    pred_current,
    Status24H,
    weights_new,
    cut_points
  ) %>%
    mutate(model = "NEWS"),
  calculate_weighted_nb(
    data_sens,
    pred_light,
    Status24H,
    weights_new,
    cut_points
  ) %>%
    mutate(model = "Simplified NEWS"),
  calculate_weighted_nb(
    data_sens,
    pred_full,
    Status24H,
    weights_new,
    cut_points
  ) %>%
    mutate(model = "DEWS"),
  calculate_weighted_nb(
    data_sens,
    pred_xgb,
    Status24H,
    weights_new,
    cut_points
  ) %>%
    mutate(model = "XGB-EWS"),
  tibble(
    threshold = cut_points,
    net_benefit = prev_sens - (1 - prev_sens) * (cut_points / (1 - cut_points)),
    model = "Treat All"
  ),
  tibble(threshold = cut_points, net_benefit = 0, model = "Treat None")
) %>%
  mutate(Group_Label = label_sens)

# GENERATE TABLE
final_sens_table <- nb_sens %>%
  group_by(Group_Label) %>%
  arrange(threshold, .by_group = TRUE) %>%
  select(threshold, model, net_benefit) %>%
  pivot_wider(names_from = model, values_from = net_benefit) %>%
  mutate(across(where(is.numeric) & !matches("threshold"), ~ .x * 10000)) %>%
  mutate(across(
    c(NEWS, `Simplified NEWS`, `XGB-EWS`, DEWS, `Treat All`, `Treat None`),
    ~ round(.x, 1)
  )) %>%
  mutate(
    diff1 = NEWS - `XGB-EWS`,
    diff2 = NEWS - `Simplified NEWS`,
    diff3 = DEWS - `XGB-EWS`,
    diff4 = NEWS - DEWS
  ) %>%
  select(
    threshold,
    `Treat All`,
    `Treat None`,
    NEWS,
    `Simplified NEWS`,
    DEWS,
    `XGB-EWS`,
    diff1,
    diff2,
    diff3,
    diff4
  ) %>%
  gt(groupname_col = "Group_Label") %>%
  tab_header(
    title = "Sensitivity Analysis: Decision Curve Analysis",
    subtitle = md(
      "Net benefit differences restricted to encounters > 60 minutes. **Values are per 10,000 patients.**"
    )
  ) %>%
  cols_label(
    threshold = "Risk Threshold",
    `Treat All` = "Treat All",
    `Treat None` = "Treat None",
    NEWS = "NEWS",
    `Simplified NEWS` = "Simplified NEWS",
    DEWS = "DEWS",
    `XGB-EWS` = "XGB-EWS",
    diff1 = "NEWS vs XGB-EWS",
    diff2 = "NEWS vs Simplified NEWS",
    diff3 = "DEWS vs XGB-EWS",
    diff4 = "NEWS vs DEWS"
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
    columns = c(diff1, diff2, diff3, diff4)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(diff1, diff2, diff3, diff4))
  ) %>%
  tab_options(row_group.font.weight = "bold") %>%
  tab_footnote(
    footnote = "Values represent the difference in Net Benefit. Positive values indicate higher net benefit for the first model listed.",
    locations = cells_column_spanners(spanners = contains("Difference"))
  )

# SAVE
final_sens_table
final_sens_table |> gtsave("sensitivity_analysis_60min_nov_v2.docx")


############# SHAP value analysis ###############

# Create SHAP values for XGBoost

set.seed(234)

doParallel::registerDoParallel(cores = 40)

# Create recipe with exact variables
xgb_recipe <- recipe(Status24H ~ ., data = data) |>
  update_role(CSN, PT_ID, new_role = "ID") |>
  step_rm(
    all_predictors(),
    -all_of(c(
      # Clinical measurements
      "Age",
      "Sex",
      "Respiration_Rate",
      "Temperature",
      "Saturation",
      "Pulse",
      "Oxygen_Supplement",
      "Blood_Pressure.Sys",
      "Blood_Pressure.Dia",
      "Consciousness",
      "Previous_Hosp",

      # Temporal features
      "day_type",
      "time_of_day",
      "month",

      # Weights
      "imp_weights",

      # History
      "previous_icu_respiratory",

      # Biomarkers
      "median_Hemoglobin",
      "median_Leukocytter",
      "median_Trombocytter",
      "median_Kreatinin",
      "median_ALAT",
      "median_LDH",
      "median_Albumin",
      "median_CRP",
      "median_Laktat_ab",
      "median_Troponin_T",
      "median_Laktat_vb",

      # PCA features (pca_0 through pca_59)
      "pca_0",
      "pca_1",
      "pca_2",
      "pca_3",
      "pca_4",
      "pca_5",
      "pca_6",
      "pca_7",
      "pca_8",
      "pca_9",
      "pca_10",
      "pca_11",
      "pca_12",
      "pca_13",
      "pca_14",
      "pca_15",
      "pca_16",
      "pca_17",
      "pca_18",
      "pca_19",
      "pca_20",
      "pca_21",
      "pca_22",
      "pca_23",
      "pca_24",
      "pca_25",
      "pca_26",
      "pca_27",
      "pca_28",
      "pca_29",
      "pca_30",
      "pca_31",
      "pca_32",
      "pca_33",
      "pca_34",
      "pca_35",
      "pca_36",
      "pca_37",
      "pca_38",
      "pca_39",
      "pca_40",
      "pca_41",
      "pca_42",
      "pca_43",
      "pca_44",
      "pca_45",
      "pca_46",
      "pca_47",
      "pca_48",
      "pca_49",
      "pca_50",
      "pca_51",
      "pca_52",
      "pca_53",
      "pca_54",
      "pca_55",
      "pca_56",
      "pca_57",
      "pca_58",
      "pca_59"
    ))
  ) |>
  step_dummy(all_nominal_predictors())

# Create workflow with recipe
xgb_wf_recipe <- workflow() |>
  add_recipe(xgb_recipe) |>
  add_model(xgb) |>
  add_case_weights(imp_weights)

set.seed(234)

doParallel::registerDoParallel(cores = 40)


# Fit on full data
xgb_final_fit <- xgb_wf_recipe |>
  fit(data = data)

# Extract model and prepare features for SHAP
xgb_model <- extract_fit_engine(xgb_final_fit)
xgb_prep <- prep(xgb_recipe, training = data)
feature_matrix <- bake(
  xgb_prep,
  new_data = data,
  has_role("predictor"),
  composition = "matrix"
)
feature_matrix <- feature_matrix[, !colnames(feature_matrix) %in% "Status24H"]

# Create SHAP values

shp <- shapviz(xgb_model, X_pred = feature_matrix)

sv_importance(shp, kind = "bar", max_display = 20, fill = "#A23B72") +
  theme_minimal(base_size = 16) +
  scale_y_discrete(
    labels = c(
      "Median albumin",
      "Median C-reactive protein",
      "Medical embedding component",
      "Medical embedding component",
      "Medical embedding component",
      "Diastolic blood pressure",
      "Median creatinine",
      "Median Lactate dehydrogenase ",
      "Medical embedding component",
      "Previous ICU/respiratory support",
      "Sex",
      "Median hemoglobin",
      "Temperature",
      "Oxygen saturation",
      "Systolic blood pressure",
      "Pulse",
      "Respiratory rate",
      "Supplemental oxygen",
      "Age",
      "Consciousness"
    )
  ) +
  labs(x = "Average absolute SHAP value")


######### Ablation analysis ############

# ===================
# DATA PREPARATION
# ====================

data_ablation <- data |>
  mutate(
    # --- Component Scores ---
    Temp_Score = case_when(
      round(Temperature, 1) <= 35.0 ~ 3,
      round(Temperature, 1) >= 35.1 & round(Temperature, 1) <= 36.0 ~ 1,
      round(Temperature, 1) >= 36.1 & round(Temperature, 1) <= 38.0 ~ 0,
      round(Temperature, 1) >= 38.1 & round(Temperature, 1) <= 39.0 ~ 1,
      round(Temperature, 1) >= 39.1 ~ 2,
      TRUE ~ 0
    ),
    BP_Score = case_when(
      Blood_Pressure.Sys <= 90 ~ 3,
      Blood_Pressure.Sys >= 91 & Blood_Pressure.Sys <= 100 ~ 2,
      Blood_Pressure.Sys >= 101 & Blood_Pressure.Sys <= 110 ~ 1,
      Blood_Pressure.Sys >= 111 & Blood_Pressure.Sys <= 219 ~ 0,
      Blood_Pressure.Sys >= 220 ~ 3,
      TRUE ~ 0
    ),
    RR_Score = case_when(
      Respiration_Rate <= 8 ~ 3,
      Respiration_Rate >= 9 & Respiration_Rate <= 11 ~ 1,
      Respiration_Rate >= 12 & Respiration_Rate <= 20 ~ 0,
      Respiration_Rate >= 21 & Respiration_Rate <= 24 ~ 2,
      Respiration_Rate >= 25 ~ 3,
      TRUE ~ 0
    ),
    Sat_Score = case_when(
      Saturation <= 91 ~ 3,
      Saturation >= 92 & Saturation <= 93 ~ 2,
      Saturation >= 94 & Saturation <= 95 ~ 1,
      Saturation >= 96 ~ 0,
      TRUE ~ 0
    ),
    Pulse_Score = case_when(
      Pulse <= 40 ~ 3,
      Pulse >= 41 & Pulse <= 50 ~ 1,
      Pulse >= 51 & Pulse <= 90 ~ 0,
      Pulse >= 91 & Pulse <= 110 ~ 1,
      Pulse >= 111 & Pulse <= 130 ~ 2,
      Pulse >= 131 ~ 3,
      TRUE ~ 0
    ),
    O2_Score = if_else(Oxygen_Supplement == "Oxygen", 2, 0),
    AVPU_Score = if_else(Consciousness == "VPU", 3, 0),

    # --- Ablated Predictors ---
    Score_Full = EWS_score,
    Score_No_Temp = EWS_score - Temp_Score,
    Score_No_BP = EWS_score - BP_Score,
    Score_Light = EWS_score - Temp_Score - BP_Score,
    Score_No_RR = EWS_score - RR_Score,
    Score_No_Sat = EWS_score - Sat_Score,
    Score_No_Pulse = EWS_score - Pulse_Score,
    Score_No_O2 = EWS_score - O2_Score,
    Score_No_AVPU = EWS_score - AVPU_Score
  )

# Exact Folds
set.seed(234)
data_folds_ablation <- rsample::group_vfold_cv(data_ablation, group = Hospital)

# Setup
model <- logistic_reg(engine = "glm", mode = "classification")
doParallel::registerDoParallel(cores = 40)
cntrl <- control_resamples(save_pred = TRUE)

# ========================================
# DEFINE BOOTSTRAP & WRAPPER FUNCTIONS
# ========================================

# Bootstrap Function for Brier Score
calculate_ablation_bootstrap <- function(
  fit_object,
  original_data,
  model_name,
  n_bootstrap = 200
) {
  preds_df <- fit_object %>%
    collect_predictions() %>%
    arrange(.row) %>%
    mutate(
      weights_new = original_data$weights_new,
      mort24H = if_else(Status24H == "Deceased", 1, 0)
    )

  set.seed(42)
  boot_results <- map_dfr(
    1:n_bootstrap,
    ~ {
      boot_indices <- sample(1:nrow(preds_df), nrow(preds_df), replace = TRUE)
      boot_sample <- preds_df[boot_indices, ]

      # CHANGED: Using weighted MSE for Brier Score
      # Brier Score is effectively the Mean Squared Error of probabilistic predictions
      brier_val <- MetricsWeighted::mse(
        actual = boot_sample$mort24H,
        predicted = boot_sample$.pred_Deceased,
        w = boot_sample$weights_new
      )

      tibble(bootstrap_id = .x, brier = brier_val)
    }
  )

  summary_metrics <- boot_results %>%
    summarise(
      Brier_Mean = mean(brier, na.rm = TRUE),
      Brier_Lower = quantile(brier, 0.025, na.rm = TRUE),
      Brier_Upper = quantile(brier, 0.975, na.rm = TRUE)
    ) %>%
    mutate(model = model_name)

  return(summary_metrics)
}

# Single Scenario Runner
run_single_scenario <- function(scenario) {
  cat(paste0("\n--- Processing: ", scenario$name, " ---\n"))

  # Workflow
  wf <- workflow() |>
    add_formula(scenario$formula) |>
    add_model(model) |>
    add_case_weights(imp_weights)

  # Fit
  fit <- fit_resamples(
    wf,
    resamples = data_folds_ablation,
    metrics = metric_set(brier_class),
    control = cntrl
  )

  # Extract Metrics (Brier)
  result <- calculate_ablation_bootstrap(
    fit_object = fit,
    original_data = data_ablation,
    model_name = scenario$name
  )

  # 4. CLEANUP
  rm(fit, wf)
  gc()

  return(result)
}

# ==============================================================================
# BATCH EXECUTION
# ==============================================================================

# Define all 9 scenarios
all_scenarios <- list(
  list(name = "Full NEWS", formula = Status24H ~ Score_Full),
  list(name = "(-) Temperature", formula = Status24H ~ Score_No_Temp),
  list(name = "(-) Blood Pressure", formula = Status24H ~ Score_No_BP),
  list(name = "(-) BP & Temp (Light)", formula = Status24H ~ Score_Light),
  list(name = "(-) Resp. Rate", formula = Status24H ~ Score_No_RR),
  list(name = "(-) Saturation", formula = Status24H ~ Score_No_Sat),
  list(name = "(-) Pulse", formula = Status24H ~ Score_No_Pulse),
  list(name = "(-) Supp. Oxygen", formula = Status24H ~ Score_No_O2),
  list(name = "(-) Consciousness", formula = Status24H ~ Score_No_AVPU)
)

# Split into batches
batches <- list(
  all_scenarios[1:3],
  all_scenarios[4:6],
  all_scenarios[7:9]
)

results_bucket <- list()

# Loop through batches
for (i in seq_along(batches)) {
  cat(paste0("\n================ STARTING BATCH ", i, " ================\n"))

  # Process current batch
  batch_results <- map_dfr(batches[[i]], run_single_scenario)

  # Store results
  results_bucket[[i]] <- batch_results

  # Clean up batch memory
  rm(batch_results)
  gc()
}

# =====================
# MERGE AND DISPLAY
# =====================

final_table <- bind_rows(results_bucket) %>%
  mutate(
    Reference_Brier = Brier_Mean[model == "Full NEWS"],
    # Logic change: For Brier, LOWER is better.
    # Positive Performance_Loss means Brier increased (worse) when feature was removed.
    Performance_Loss = Brier_Mean - Reference_Brier
  ) %>%
  arrange(Brier_Mean) %>% # Arrange ascending (best to worst)
  select(model, Brier_Mean, Performance_Loss, Brier_Lower, Brier_Upper)

print(final_table)

final_table |> write_parquet("ablation_nov_v2.parquet")


# ===========================
# PREPARE DATA FOR PLOTTING
# ===========================

# ===========================
# DATA CLEANING & RENAMING
# ===========================

formatted_data <- final_table %>%
  mutate(
    clean_label = case_when(
      model == "Full NEWS" ~ "Full NEWS (Reference)",
      model == "(-) Pulse" ~ "Excluding Pulse",
      model == "(-) Resp. Rate" ~ "Excluding Respiratory rate",
      model == "(-) Temperature" ~ "Excluding Temperature",
      model == "(-) Supp. Oxygen" ~ "Excluding Oxygen supplement",
      model == "(-) Saturation" ~ "Excluding Saturation",
      model == "(-) Blood Pressure" ~ "Excluding Systolic blood pressure",
      model ==
        "(-) BP & Temp (Light)" ~ "Simplified NEWS (Without Systolic blood pressure & Temperature)",
      model == "(-) Consciousness" ~ "Excluding Consciousness",
      TRUE ~ model
    ),

    # Define highlight groups for colors
    highlight_group = case_when(
      model == "Full NEWS" ~ "Reference",
      model == "(-) BP & Temp (Light)" ~ "Proposed",
      TRUE ~ "Other"
    )
  ) %>%
  mutate(clean_label = fct_reorder(clean_label, desc(Brier_Mean)))

# Extract reference value for the vertical line
ref_value <- formatted_data$Brier_Mean[formatted_data$model == "Full NEWS"]

# ===================
# GENERATE PLOT
# ===================

p <- ggplot(
  formatted_data,
  aes(x = Brier_Mean, y = clean_label, color = highlight_group)
) +

  # Reference Line
  geom_vline(
    xintercept = ref_value,
    linetype = "dashed",
    color = "firebrick",
    alpha = 0.6
  ) +

  # Error Bars
  geom_errorbarh(
    aes(xmin = Brier_Lower, xmax = Brier_Upper),
    height = 0.25,
    linewidth = 0.6
  ) +

  # Points
  geom_point(size = 3) +

  # Colors
  scale_color_manual(
    values = c(
      "Reference" = "firebrick",
      "Proposed" = "#1f77b4",
      "Other" = "gray40"
    )
  ) +

  # Labels (No Title/Subtitle as requested)
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Brier Score",
    y = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(
      color = "black",
      face = "plain",
      margin = margin(r = 10)
    ),
    axis.text.x = element_text(color = "black", face = "plain"),
    axis.title.x = element_text(face = "plain", margin = margin(t = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(), # Removes horizontal grid lines
    panel.grid.major.x = element_line(color = "gray90")
  ) +

  # Zoom in on the relevant range (optional, keeps focus on the small differences)
  coord_cartesian(
    xlim = c(
      min(formatted_data$Brier_Lower) * 0.999,
      max(formatted_data$Brier_Upper) * 1.001
    )
  )

# Print
print(p)


########## Table 1 creation ###########

# ==============================================================================
# FINAL TABLE 1: RAW N + WEIGHTED % + INTERVAL IQR
# ==============================================================================

# SETUP
# Remove the weights column to ensure clean survey design object
data1 <- data |> select(-imp_weights)

# Create the survey design object
svy_design <- svydesign(ids = ~1, weights = ~weights_new, data = data1)

# Set the Lancet theme (Points for decimals, specific fonts)
gtsummary::set_gtsummary_theme(gtsummary::theme_gtsummary_journal("lancet"))

# ---------------------------------------
# CALCULATE RAW Ns (For the Headers)
# ---------------------------------------

n_raw_total <- nrow(data1)
n_raw_deceased <- nrow(data1 %>% filter(Status24H == "Deceased"))
n_raw_alive <- nrow(data1 %>% filter(Status24H == "Alive"))

# Helper to format with spaces
fmt_n <- function(n) format(n, big.mark = " ", trim = TRUE)

# -----------------
# GENERATE TABLE
# -----------------

tbl_summary_weighted <- tbl_svysummary(
  svy_design,
  by = Status24H,
  include = c(
    "Age_Group",
    "Sex",
    "Previous_Hosp",
    "Hospital",
    "Department_Name_Fac",
    "Consciousness",
    "Oxygen_Supplement",
    "Respiration_Rate",
    "Pulse",
    "Temperature",
    "Saturation",
    "Blood_Pressure.Sys",
    "Blood_Pressure.Dia",
    "previous_icu_respiratory",
    "EWS_score",
    "median_Hemoglobin",
    "median_Leukocytter",
    "median_Trombocytter",
    "median_Kreatinin",
    "median_ALAT",
    "median_LDH",
    "median_Albumin",
    "median_CRP",
    "median_Laktat_ab",
    "median_Troponin_T",
    "median_Laktat_vb"
  ),
  missing = "no",

  # A) FORCE 1 DECIMAL PLACE
  digits = list(
    all_categorical() ~ c(0, 1),
    all_continuous() ~ 1
  ),

  # B) STATISTICS FORMAT
  # Categorical: Raw Count (Weighted Percent)
  # Continuous: Weighted Median (Weighted P25 - P75)
  statistic = list(
    all_categorical() ~ "{n_unweighted} ({p}%)",
    all_continuous() ~ "{median} ({p25} - {p75})"
  ),
  label = list(
    Age_Group = "Age group",
    Department_Name_Fac = "Clinical department",
    Previous_Hosp = "Previous hospitalizations (count)",
    Respiration_Rate = "Respiratory rate (breaths/min)",
    Pulse = "Pulse (beats/min)",
    Temperature = "Temperature (°C)",
    Saturation = "Oxygen saturation (%)",
    Oxygen_Supplement = "Supplemental oxygen",
    Blood_Pressure.Dia = "Diastolic blood pressure (mm Hg)",
    Blood_Pressure.Sys = "Systolic blood pressure (mm Hg)",
    previous_icu_respiratory = "Previous ICU/respiratory support",
    EWS_score = "NEWS score",
    median_Hemoglobin = "Hemoglobin (g/L)",
    median_Leukocytter = "Leukocytes (×10⁹/L)",
    median_Trombocytter = "Platelets (×10⁹/L)",
    median_Kreatinin = "Creatinine (μmol/L)",
    median_ALAT = "Alanine aminotransferase (U/L)",
    median_LDH = "Lactate dehydrogenase (U/L)",
    median_Albumin = "Albumin (g/L)",
    median_CRP = "C-reactive protein (mg/L)",
    median_Laktat_ab = "Lactate, arterial blood (mmol/L)",
    median_Troponin_T = "Troponin T (ng/L)",
    median_Laktat_vb = "Lactate, venous blood (mmol/L)"
  )
) %>%
  add_overall() %>%
  # C) OVERRIDE HEADERS WITH RAW Ns
  modify_header(
    label = "**Characteristic**",
    stat_0 = paste0("**Overall**<br>N = ", fmt_n(n_raw_total)),
    stat_1 = paste0("**Alive**<br>N = ", fmt_n(n_raw_alive)),
    stat_2 = paste0("**Deceased**<br>N = ", fmt_n(n_raw_deceased))
  ) %>%
  # D) EXPLICIT FOOTNOTE
  modify_footnote(
    all_stat_cols() ~ "Values are Raw N (Weighted %) or Weighted Median (IQR). N represents the unweighted sample size. Percentages and medians are weighted to represent the target population. IQR = 25th - 75th percentile."
  ) %>%
  as_gt() %>%
  # E) LANCET FORMATTING
  fmt_number(
    columns = everything(),
    decimals = 1,
    dec_mark = "·",
    sep_mark = " "
  ) %>%
  tab_header(
    title = md("Table 1: Baseline characteristics of the study population")
  )

# 4. SAVE
tbl_summary_weighted
gtsave(tbl_summary_weighted, "Table1_Hybrid_Final_Nov_v2.docx")
