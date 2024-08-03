import polars as pl
import os

# Specify the path
path = "/Users/jkv465/Desktop/Work_EWS/New_Data"
# Specify the working directory
os.chdir(path)
print("Current working directory: ", os.getcwd()) # And here we can check it

# Scan the EWS + Blood tests data

ews = pl.scan_parquet("EWS_Blood.parquet")

# Compute the average of each biomarker per hospitalization (per patient)

ews = (
    ews
    .with_columns([
        pl.col("Hemoglobin").mean().over("Identifier").alias("Hemoglobin"),
        pl.col("Leukocytes").mean().over("Identifier").alias("Leukocytes"),
        pl.col("Trombocytes").mean().over("Identifier").alias("Trombocytes"),
        pl.col("Kreatinin").mean().over("Identifier").alias("Kreatinin"),
        pl.col("ALAT").mean().over("Identifier").alias("ALAT"),
        pl.col("LDH").mean().over("Identifier").alias("LDH"),
        pl.col("Albumin").mean().over("Identifier").alias("Albumin"),
        pl.col("CRP").mean().over("Identifier").alias("CRP"),
        pl.col("Laktak_ab").mean().over("Identifier").alias("Laktak_ab"),
        pl.col("Troponin").mean().over("Identifier").alias("Troponin"),
        pl.col("Laktat_vb").mean().over("Identifier").alias("Laktat_vb")
    ])
    .unique(subset=["Identifier"])
)

# Sort the dataframe now

ews = ews.sort(by = ["PT_ID","Identifier"])


# Rename department column

ews = (
    ews.rename({"ADT_DEPARTMENT_ID":"DEPARTMENT_ID"})
)

################################################
########## Open the ITA dataset ################
################################################

ita = pl.scan_parquet("intensive_care.parquet")

ita = ita.sort(by = ["PT_ID","Respiration_Num"])

# Remove EnterpriseID 

ita = ita.drop("EnterpriseID")

# Remove also Department ID

ita = ita.drop("DEPARTMENT_ID")

# 
ews_bl_ita = (ews
    .with_columns([
        pl.col("DEPARTMENT_ID").cast(pl.String,strict=False)
    ])
    .join(ita, on=["PT_ID"], how="left")
    .with_columns([
        pl.col("HOSP_DISCH_TIME").cast(pl.Datetime("us"))
    ])
    .with_columns(
        pl.when((pl.col("ITA_Start") > pl.col("recorded_time")) & 
                (pl.col("ITA_Start") <= pl.col("HOSP_DISCH_TIME")))
        .then(pl.lit("ITA"))
        .otherwise(pl.lit("NO_ITA"))
        .alias("ITA_Indicator")
    )
    .unique(subset=["Identifier"])
    .sort(by = ["PT_ID","Identifier"])
)


# Save as parquet now

ews_bl_ita.collect().write_parquet("EWS_Blood_ITA.parquet")
