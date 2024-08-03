import polars as pl
import os

# Specify the path
path = "/Users/jkv465/Desktop/Work_EWS/New_Data"
# Specify the working directory
os.chdir(path)
print("Current working directory: ", os.getcwd()) # And here we can check it

# Open the dataset of EWS

ews = pl.scan_parquet("Summarized_EWS_data.parquet")

# Rename some columns for merging
# Cast CSN as string

ews = (
    ews
    .rename({"Hospitalization_No":"CSN",
              "ptID":"PT_ID"})
    .with_columns(pl.col("CSN").cast(pl.String,strict = False))
)


################################################
###### Open the dataset of blood tests #########
################################################

blt = pl.scan_parquet("blood_tests_imputed.parquet")

blt = (
    blt
    .drop("EnterpriseID")
    .rename({"Hæmoglobin;B_imputed":"Hemoglobin",
             "Leukocytter;B_imputed":"Leukocytes",
             "Trombocytter;B_imputed": "Trombocytes",
             "Kreatinin;P_imputed": "Kreatinin",
             "Alanintransaminase [ALAT];P_imputed" : "ALAT",
             "Laktatdehydrogenase [LDH];P_imputed":"LDH",
             "Albumin;P_imputed":"Albumin",
             "C-reaktivt protein [CRP];P_imputed":"CRP",
             "Laktat;P(aB)_imputed":"Laktak_ab",
             "Troponin T;P_imputed":"Troponin",
             "Laktat;P(vB)_imputed":"Laktat_vb"})
    .drop("Blood_Test_Status")
)


########################################################
###### Now we need to join these two dataframes ########
######################  blt and ews ####################


ews_blood = (ews
        .join(blt, on="PT_ID", how="left")
        .with_columns([
            pl.col("Blood_Test_End").cast(pl.Datetime("us")), # cast into similar datetime format
            pl.col("recorded_time").cast(pl.Datetime("us")) # cast into similar datetime format
        ])
        .with_columns([
            pl.when(pl.col("Blood_Test_End") > pl.col("recorded_time")) # NA blood tests after recorded time
            .then(None) # As they cannot be used for prediction
            .otherwise(pl.col(col))
            .alias(col)
            for col in ["Hemoglobin", "Leukocytes", "Trombocytes", "Kreatinin",
                        "ALAT", "LDH", "Albumin", "CRP", "Laktak_ab", "Troponin", "Laktat_vb"]
        ])
        .drop(["Blood_Test_End", "Blood_Test_Start"])
)

# Sink parquet now

ews_blood.sink_parquet("EWS_Blood.parquet")
