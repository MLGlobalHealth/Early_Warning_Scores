# Development and validation of EWS systems 🚑

## Contents

- ```preprocessing```

    Contains info on initial pre-processing of Electronic Health Records consisting of blood tests, diagnoses, various procedures, and intensive care data for individuals residing in Denmark, with a general admission to the hospitals in the region of Zealand, Denmark, between 2018-2023.

  - `Original_Preprocessing.ipynb` contains python code with an initial pre-processing of all datasets with clinical information.

  - `Intensive_Care.R` contains R code with more thorough analysis of intensive care data.

  - `Diagnoses.R` contains R code performing categorization/grouping of various ICD-10 diagnoses of patients.

  - `Blood_Tests.py` contains python code on imputation of blood tests containing string values not suitable for analysis.

  - `Procedures.R` contains R code on various medical procedures for each individual. Categorization of procedures (SKS-Codes) has been performed + text mining/topic modelling for the characterization of them.

- **Merging folder**

  - `EWS_Blood.py` contains python code on merging of EWS (Early Warning Score) data of individuals with blood tests.

  - `EWS_ITA.py` contains python code on merging EWS + Blood Tests with Intensive Care data

  - `EWS_Blood_ITA_Procedures.R` contains R code on merging EWS + Blood Tests + Intensive Care with Procedures data

  - `EWS_Final_Merging.py` contains python code on the final merging of the datasets (diagnoses included)

- **Modelling folder**

  - `Evaluating_NEWS2_Only.R` contains R code for the validation of NEWS2 system in terms of predictive performance

- **To do list:**

  - Assessment of NEWS2 current system based on predictive performance metrics using data-splitting techniques ✅.

  - Debiasing the dataset with IPW based on intervention scenarios ✅

  - Development of alternative early warning score systems and model comparison ✅

  - Assess performance on various strata of target population / Assess fairness 🔨
