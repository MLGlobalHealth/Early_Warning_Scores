# =============================
# SUPPLEMENTAL DATA PROCESSING
# =============================

# Note: NEWS-Light was renamed to "Simplified NEWS" in the manuscript

# Load necessary libraries

library(arrow)
library(tidyverse)
library(tidylog)
library(lubridate)
library(duckdb)

# Set the working directory
setwd("/home/alex/ews/NEWS2_Evaluation/Additional_Data")

# HELPER FUNCTIONS #

transform_enterprise_id <- function(column) {
  # Ensure input is character to avoid numeric conversion errors
  paste0("Z", as.numeric(str_sub(as.character(column), 2)) + 1)
}

fix_pt_id <- function(pt_id) {
  # We use local variables to avoid 'case_when' evaluating the "e+" logic
  # on rows that don't have it (which can cause errors)

  # 1. Identify scientific notation cases
  is_scientific <- str_detect(pt_id, "e\\+")

  # 2. Process ONLY the scientific ones
  fixed_vals <- pt_id

  if (any(is_scientific, na.rm = TRUE)) {
    # Extract parts
    sci_vals <- pt_id[is_scientific]
    z_part <- str_extract(sci_vals, "^Z")
    number_part <- str_remove(sci_vals, "^Z")

    # Reform
    clean_numbers <- format(
      as.numeric(number_part),
      scientific = FALSE,
      trim = TRUE
    )
    fixed_vals[is_scientific] <- paste0(z_part, clean_numbers)
  }

  return(fixed_vals)
}

# =========================
# 1. DIAGNOSES PROCESSING
# =========================

diagnoses <- read_parquet("diagnoses.parquet") |>
  mutate(across(where(is.character), ~ na_if(.x, "NULL"))) |>
  mutate(
    Kontakt_start = as.POSIXct(
      format(`Kontakt start`, "%Y-%m-%d %H:%M:%S"),
      tz = ""
    ),
    PT_ID = transform_enterprise_id(EnterpriseID)
  ) |>
  filter(!is.na(CSN)) |>
  select(
    PT_ID,
    CSN,
    Department_ID = `Department ID`,
    Kontakt_start,
    Diagnosis_Name = Aktionsdiagnose,
    Diagnosis_Code = `Aktionsdiagnose kode`
  ) |>
  arrange(PT_ID, Kontakt_start)

# Create Categories
diagnoses <- diagnoses |>
  mutate(Diagnosis_Code = str_sub(Diagnosis_Code, 2)) |>
  mutate(
    Diagnosis_Category = case_when(
      str_detect(
        Diagnosis_Code,
        "^[AB]"
      ) ~ "Certain infectious and parasitic diseases",
      str_detect(Diagnosis_Code, "^C|^D[0-4]") ~ "Neoplasms",
      str_detect(
        Diagnosis_Code,
        "^D[5-8]"
      ) ~ "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
      str_detect(
        Diagnosis_Code,
        "^E"
      ) ~ "Endocrine, nutritional and metabolic diseases",
      str_detect(
        Diagnosis_Code,
        "^F"
      ) ~ "Mental, Behavioral and Neurodevelopmental disorders",
      str_detect(Diagnosis_Code, "^G") ~ "Diseases of the nervous system",
      str_detect(Diagnosis_Code, "^H[0-5]") ~ "Diseases of the eye and adnexa",
      str_detect(
        Diagnosis_Code,
        "^H[6-9]"
      ) ~ "Diseases of the ear and mastoid process",
      str_detect(Diagnosis_Code, "^I") ~ "Diseases of the circulatory system",
      str_detect(Diagnosis_Code, "^J") ~ "Diseases of the respiratory system",
      str_detect(Diagnosis_Code, "^K") ~ "Diseases of the digestive system",
      str_detect(
        Diagnosis_Code,
        "^L"
      ) ~ "Diseases of the skin and subcutaneous tissue",
      str_detect(
        Diagnosis_Code,
        "^M"
      ) ~ "Diseases of the musculoskeletal system and connective tissue",
      str_detect(Diagnosis_Code, "^N") ~ "Diseases of the genitourinary system",
      str_detect(
        Diagnosis_Code,
        "^O"
      ) ~ "Pregnancy, childbirth and the puerperium",
      str_detect(
        Diagnosis_Code,
        "^P"
      ) ~ "Certain conditions originating in the perinatal period",
      str_detect(
        Diagnosis_Code,
        "^Q"
      ) ~ "Congenital malformations, deformations and chromosomal abnormalities",
      str_detect(
        Diagnosis_Code,
        "^R"
      ) ~ "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
      str_detect(
        Diagnosis_Code,
        "^[ST]"
      ) ~ "Injury, poisoning and certain other consequences of external causes",
      str_detect(Diagnosis_Code, "^[VWX]|^Y") ~ "External causes of morbidity",
      str_detect(
        Diagnosis_Code,
        "^Z"
      ) ~ "Factors influencing health status and contact with health services",
      str_detect(Diagnosis_Code, "^U") ~ "Codes for special purposes",
      TRUE ~ "Unknown category"
    )
  )

write_parquet(diagnoses, "diagnoses_latest_v2.parquet")


# ============================
# 2. PROCEDURES PROCESSING
# ============================

procedures <- read_parquet("procedures.parquet") |>
  mutate(PT_ID = transform_enterprise_id(EnterpriseID)) |>
  rename(SKS_Code = `SKS-Kode`, Procedure_Date = `Udført procedurer dato`) |>
  mutate(
    Procedure_Date = as.POSIXct(
      format(ymd_hms(Procedure_Date), "%Y-%m-%d %H:%M:%S"),
      tz = ""
    )
  ) |>
  mutate(across(where(is.character), ~ na_if(.x, "NULL"))) |>
  filter(!is.na(CSN)) |>
  select(PT_ID, CSN, SKS_Code, Procedurenavn, Procedure_Date) |> # Removed redundant columns
  mutate(
    SKS_Group = case_when(
      str_starts(SKS_Code, "A") ~ "Administrative Matters",
      str_starts(SKS_Code, "B") ~ "Treatment Care",
      str_starts(SKS_Code, "D") ~ "Diseases Health Conditions",
      str_starts(SKS_Code, "E") ~ "External Causes Injury",
      str_starts(SKS_Code, "F") ~ "Functional Ability",
      str_starts(SKS_Code, "K") ~ "Surgical Operations",
      str_starts(SKS_Code, "M") ~ "Drug Substance ATC",
      str_starts(SKS_Code, "N") ~ "Anesthesia or Intensive Care",
      str_starts(SKS_Code, "R") ~ "Results report",
      str_starts(SKS_Code, "U") ~ "Clinical Examinations / Radiological",
      str_starts(SKS_Code, "W") ~ "Clinical Physiological Examinations",
      str_starts(SKS_Code, "Z") ~ "Various Procedures",
      TRUE ~ NA_character_
    )
  )

write_parquet(procedures, "procedures_latest_v2.parquet")


# ==============================================================================
# 3. ICU (ITA) PROCESSING
# ==============================================================================

ita <- read_parquet("ita_respirator.parquet") |>
  mutate(PT_ID = transform_enterprise_id(EnterpriseID)) |>
  mutate(across(where(is.character), ~ na_if(.x, "NULL"))) |>
  mutate(
    Respirator_start = as.POSIXct(
      format(`Respirator start`, "%Y-%m-%d %H:%M:%S"),
      tz = ""
    ),
    ITA_start = as.POSIXct(format(`ITA start`, "%Y-%m-%d %H:%M:%S"), tz = "")
  ) |>
  select(PT_ID, Respirator_start, ITA_start) # Removed redundant end times/departments

write_parquet(ita, "ita_latest_v2.parquet")


# =================================================
# 4. BLOOD TEST PROCESSING (CLEANING & IMPUTATION)
# =================================================

tests <- read_parquet("prøvesvar.parquet") |>
  rename(
    EnterpriseID = V1,
    Blood_Test_Name = V3,
    Blood_Test_Value = V4,
    Blood_Test_Status = V5,
    Blood_Test_Start = V6,
    Blood_Test_End = V7
  ) |>
  mutate(PT_ID = transform_enterprise_id(EnterpriseID)) |>
  mutate(across(
    c(Blood_Test_Start, Blood_Test_End),
    ~ as.POSIXct(format(., "%Y-%m-%d %H:%M:%S"), tz = "")
  )) |>
  pivot_wider(
    names_from = Blood_Test_Name,
    values_from = Blood_Test_Value,
    id_cols = c(PT_ID, Blood_Test_Status, Blood_Test_Start, Blood_Test_End),
    values_fn = first
  ) |>
  filter(Blood_Test_Status == "Endelig")

# Clean & Impute Detection Limits
blood_test_cols <- tail(names(tests), 11)
tests_imputed <- tests |> select(PT_ID, Blood_Test_End) # Keep only ID and Time

for (col in blood_test_cols) {
  # Clean Text
  clean_vals <- tests[[col]]
  clean_vals <- case_when(
    is.na(clean_vals) ~ NA_character_,
    !is.na(suppressWarnings(as.numeric(clean_vals))) ~ clean_vals,
    str_detect(clean_vals, "^[><]\\s*[0-9]+\\.?[0-9]*") ~ clean_vals,
    TRUE ~ NA_character_
  )

  # Impute
  values <- clean_vals
  numeric_values <- as.numeric(values)
  sure_values <- numeric_values[!is.na(numeric_values)]
  problematic_values <- values[!is.na(values) & str_detect(values, "^[><]")] |>
    unique()

  if (length(problematic_values) > 0) {
    replacement_map <- tibble(original = problematic_values) |>
      mutate(
        operator = str_sub(original, 1, 1),
        bound = as.numeric(str_sub(original, 2)),
        replacement = case_when(
          operator == "<" ~ {
            lower <- sure_values[sure_values < bound]
            if (length(lower) > 0) median(lower) else NA_real_
          },
          operator == ">" ~ {
            upper <- sure_values[sure_values > bound]
            if (length(upper) > 0) median(upper) else NA_real_
          },
          TRUE ~ NA_real_
        )
      )
    lookup <- setNames(replacement_map$replacement, replacement_map$original)
    imputed_values <- ifelse(
      values %in% names(lookup),
      lookup[values],
      as.numeric(values)
    )
  } else {
    imputed_values <- as.numeric(values)
  }
  tests_imputed[[paste0(col, "_imputed")]] <- imputed_values
}

# Rename to English
tests_imputed <- tests_imputed |>
  rename(
    Hemoglobin = `Hæmoglobin;B_imputed`,
    Leukocytter = `Leukocytter;B_imputed`,
    Trombocytter = `Trombocytter;B_imputed`,
    Kreatinin = `Kreatinin;P_imputed`,
    ALAT = `Alanintransaminase [ALAT];P_imputed`,
    LDH = `Laktatdehydrogenase [LDH];P_imputed`,
    Albumin = `Albumin;P_imputed`,
    CRP = `C-reaktivt protein [CRP];P_imputed`,
    Laktat_ab = `Laktat;P(aB)_imputed`,
    Troponin_T = `Troponin T;P_imputed`,
    Laktat_vb = `Laktat;P(vB)_imputed`
  )

write_parquet(tests_imputed, "tests_latest.parquet")


# =======================================
# 5. MERGING WITH EWS DATA
# =======================================

df <- read_parquet("/home/alex/ews/NEWS2_Evaluation/single_ews_imp_v2.parquet")

blood_test_cols <- setdiff(
  names(tests_imputed),
  c(
    "PT_ID",
    "Blood_Test_End",
    "Blood_Test_Status",
    "EnterpriseID",
    "Blood_Test_Start"
  )
)

# Using duckdb

# Connect to In-Memory DuckDB & Register Tables
# This creates "virtual tables" pointing our R dataframes without copying them

con <- dbConnect(duckdb())
duckdb_register(con, "df_tbl", df)
duckdb_register(con, "tests_tbl", tests_imputed)

# Construct the SQL Query dynamically
# Generates: "MEDIAN(Hemoglobin) AS median_Hemoglobin, MEDIAN(CRP) AS median_CRP..."
agg_part <- paste0(
  "MEDIAN(",
  blood_test_cols,
  ") AS median_",
  blood_test_cols,
  collapse = ", "
)

query <- paste0(
  "
  SELECT
    t1.PT_ID,
    t1.recorded_time,
    ",
  agg_part,
  "
  FROM df_tbl AS t1
  INNER JOIN tests_tbl AS t2
    ON t1.PT_ID = t2.PT_ID
  WHERE t2.Blood_Test_End < t1.recorded_time # Prior and not at recording time
  GROUP BY t1.PT_ID, t1.recorded_time
"
)

# Execute
df_labs_agg <- dbGetQuery(con, query)

# Clean up connection
dbDisconnect(con, shutdown = TRUE)

# Join back to main dataset
# We use left_join to keep all patients (even those with no labs)
cat("Joining Aggregated Labs back to main dataframe...\n")

df <- df |>
  left_join(df_labs_agg, by = c("PT_ID", "recorded_time")) |>
  # Fill NAs for patients who had no matching blood tests
  mutate(across(
    starts_with("median_"),
    ~ ifelse(is.nan(.x) | is.na(.x), NA_real_, .x)
  ))

# Clean up memory
rm(df_labs_agg)
gc()

# ==============================
# 6. ADD ICU HISTORY & FLAGS
# ==============================

ita <- read_parquet("ita_latest_v2.parquet")

# Previous History
prev_icu <- df |>
  select(PT_ID, recorded_time) |>
  inner_join(ita, by = "PT_ID") |>
  filter(ITA_start < recorded_time | Respirator_start < recorded_time) |>
  distinct(PT_ID, recorded_time) |>
  mutate(previous_icu_respiratory = 1)

df <- df |>
  left_join(prev_icu, by = c("PT_ID", "recorded_time")) |>
  mutate(previous_icu_respiratory = replace_na(previous_icu_respiratory, 0))

# Early Future ICU (Within 24h)
early_icu <- df |>
  select(PT_ID, recorded_time) |>
  mutate(end_win = recorded_time + hours(24)) |>
  inner_join(ita, by = "PT_ID") |>
  filter(
    (ITA_start >= recorded_time & ITA_start <= end_win) |
      (Respirator_start >= recorded_time & Respirator_start <= end_win)
  ) |>
  distinct(PT_ID, recorded_time) |>
  mutate(early_icu_respiratory_24h = 1)

df <- df |>
  left_join(early_icu, by = c("PT_ID", "recorded_time")) |>
  mutate(early_icu_respiratory_24h = replace_na(early_icu_respiratory_24h, 0))


# ========================================
# 7. ADD TEXT HISTORIES & DIAGNOSES
# ========================================

procedures <- read_parquet("procedures_latest_v2.parquet") |>
  mutate(PT_ID = fix_pt_id(PT_ID))

diagnoses <- read_parquet("diagnoses_latest_v2.parquet") |>
  filter(!duplicated(CSN)) |>
  mutate(PT_ID = fix_pt_id(PT_ID))

# --- PROCEDURES TEXT ---
# Select only necessary columns to avoid 'CSN.x' collision
procedures <- procedures |>
  select(PT_ID, Procedure_Date, Procedurenavn)

# REGISTER TO DUCKDB
con <- dbConnect(duckdb())
duckdb_register(con, "df_tbl", df)
duckdb_register(con, "proc_tbl", procedures)
duckdb_register(con, "diag_tbl", diagnoses)

# CONSTRUCT SQL QUERY
# We use CTEs (Common Table Expressions) to calculate the text strings
# separately, then join them to the main table at the end.

query <- "
WITH proc_agg AS (
    SELECT
        t1.CSN,
        STRING_AGG(t2.Procedurenavn, ' | ') AS Aggregated_Procedures
    FROM df_tbl AS t1
    JOIN proc_tbl AS t2 ON t1.PT_ID = t2.PT_ID
    WHERE t2.Procedure_Date < t1.recorded_time
    GROUP BY t1.CSN
),
diag_agg AS (
    SELECT
        t1.CSN,
        STRING_AGG(t3.Diagnosis_Name, ' | ') AS Aggregated_Diagnoses
    FROM df_tbl AS t1
    JOIN diag_tbl AS t3 ON t1.PT_ID = t3.PT_ID
    WHERE t3.Kontakt_start < t1.recorded_time
    GROUP BY t1.CSN
)
SELECT
    main.*,
    COALESCE(pa.Aggregated_Procedures, 'Ingen') AS Aggregated_Procedures,
    COALESCE(da.Aggregated_Diagnoses, 'Ingen') AS Aggregated_Diagnoses,
    d.Diagnosis_Category
FROM df_tbl AS main
LEFT JOIN proc_agg pa ON main.CSN = pa.CSN
LEFT JOIN diag_agg da ON main.CSN = da.CSN
LEFT JOIN diag_tbl d ON main.PT_ID = d.PT_ID AND main.CSN = d.CSN
"

# EXECUTE AND SAVE
df_final <- dbGetQuery(con, query)

# CLEAN UP
dbDisconnect(con, shutdown = TRUE)
rm(procedures, diagnoses)
gc()

# SAVE FINAL PARQUET
write_parquet(df_final, "df_with_bt_procs_diags_latest_v2.parquet")


# ========================
# 8. INTERVENTION OUTCOME
# ========================

# REGISTER TABLES
con <- dbConnect(duckdb())
duckdb_register(con, "df_tbl", df_final)
duckdb_register(con, "proc_tbl", procedures)

# DEFINE QUERY
# This query identifies ONLY the CSNs that had a qualifying intervention.
# logic: Join on Patient ID -> Check if procedure time is within [0, 24h] of EWS -> Check Categories
query <- "
SELECT DISTINCT t1.CSN
FROM df_tbl AS t1
JOIN proc_tbl AS t2
  ON t1.PT_ID = t2.PT_ID
WHERE
  -- Temporal Filter (The 'Nested Admission' fix logic)
  t2.Procedure_Date >= t1.recorded_time
  AND t2.Procedure_Date <= t1.recorded_time + INTERVAL '24' HOUR
  AND (
    -- Category Filter
    t2.SKS_Group IN ('Anesthesia or Intensive Care', 'Surgical Operations')
    OR
    -- Regex Filter for Ventilation codes (BGDA)
    t2.SKS_Code LIKE '%BGDA%'
  )
"

# EXECUTE
major_intervention_csns <- dbGetQuery(con, query)

# CLEAN UP DB
dbDisconnect(con, shutdown = TRUE)

# APPLY FLAG IN R
# Now we just check if the CSN is in our list of 'bad' CSNs
df_final <- df_final |>
  mutate(
    Interventions_24 = case_when(
      CSN %in% major_intervention_csns$CSN | early_icu_respiratory_24h == 1 ~ 1,
      TRUE ~ 0
    )
  )

# SAVE
write_parquet(df_final, "df_without_embeddings_v2.parquet")
