import polars as pl
import os

# Specify the path
path = "/Users/jkv465/Desktop/Work_EWS/New_Data"
# Specify the working directory
os.chdir(path)
print("Current working directory: ", os.getcwd()) # And here we can check it

# Open the EWS dataset

ews = pl.scan_parquet("EWS_Blood_ITA_Procedures.parquet")

# Open the diagnoses dataset

diagnoses = pl.scan_parquet("diagnoses_grouped.parquet")

# Let's merge the two dataframes now

diagnoses = diagnoses.drop(["EnterpriseID","PT_ID","Department ID","Aktionsdiagnose kode","Aktionsdiagnose","SKS_group"]).rename({"category":"SKS_Category"})

diagnoses = diagnoses.sort(by="CSN")


# Merge now

ews_diagnoses = ews.join(diagnoses,how="left",on="CSN")


# Now let's save as parquet two different files

# First the EWS + Blood Tests + Procedures + ITA + Diagnoses (including CSNs with multiple diagnoses)

ews_diagnoses.sink_parquet("EWS_Final_Mult.parquet")

# Then the EWS + Blood Tests + Procedures + ITA + Diagnoses (keeping only one diagnosis per CSN)

ews_diagnoses_un = ews_diagnoses.group_by("Identifier").agg(pl.all().first())

ews_diagnoses_un = ews_diagnoses_un.sort(by = ["PT_ID","Identifier"])

ews_diagnoses_un.collect().write_parquet("EWS_Final_Unique.parquet")
