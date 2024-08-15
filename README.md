# Development and validation of EWS systems 🚑

## Contents

- `preprocessing`

    Contains info on initial pre-processing of Electronic Health Records consisting of blood tests, diagnoses, various procedures, and intensive care data for individuals residing in Denmark, with a general admission to the hospitals in the region of Zealand, Denmark, between 2018-2023.

  - `Original_Preprocessing.ipynb` contains python code with an initial pre-processing of all datasets with clinical information.

  - `Intensive_Care.R` contains R code with more thorough analysis of intensive care data.

  - `Diagnoses.R` contains R code performing categorization/grouping of various ICD-10 diagnoses of patients.

  - `Blood_Tests.py` contains python code on imputation of blood tests containing string values not suitable for analysis.

  - `Procedures.R` contains R code on various medical procedures for each individual. Categorization of procedures (SKS-Codes) has been performed + text mining/topic modelling for the characterization of them.

- `Merging`

  - `EWS_Blood.py` contains python code on merging of EWS (Early Warning Score) data of individuals with blood tests.

  - `EWS_ITA.py` contains python code on merging EWS + Blood Tests with Intensive Care data

  - `EWS_Blood_ITA_Procedures.R` contains R code on merging EWS + Blood Tests + Intensive Care with Procedures data

  - `EWS_Final_Merging.py` contains python code on the final merging of the datasets (diagnoses included)

- `modelling`

  - `Evaluating_NEWS2_Only.r` :
    - Contains R code for the validation of NEWS2 system in terms of predictive performance
    - 🔗 Internal-External Cross-Validation (IECV) based on hospitals
    - 🔗 AUC, Brier Score, Calibration, Net Benefit
    - 🔗 Thresholds added in the Decision Curve Analysis
  - `IECV_NEWS2.r` :
    - Contains R code on IECV with a meta-analysis approach
  - `Development_Comparisons.r` :
    - Contains R code comparing various models and algorithms with the current NEWS2 system
      - 🔗 NEWS2-Light: NEWS2 - Blood Pressure - Temperature
      - 🔗 IEWS: NEWS2 + Age + Sex
      - 🔗 TREE-EWS: XGBoost with Age,Sex,Vital Signs, Previous Hospitalization & Blood Tests
      - 🔗 Weighted performance metrics

- **To do list:**

  - Assessment of NEWS2 current system based on predictive performance metrics using data-splitting techniques ✅.

  - De-biasing the dataset with IPW based on intervention scenarios ✅

  - Development of alternative early warning score systems and model comparison ✅

  - Add calibration plots for the newly developed models 🔨

  - Assess sustained recovery prediction of NEWS2  🔨

  - Assess performance on various strata of target population / Assess fairness 🔨

  - Try a DL architecture as an additional benchmark 🔨

  - Create a Table 1 🏷️
