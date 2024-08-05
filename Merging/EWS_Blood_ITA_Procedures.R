library(arrow)
library(data.table)
library(tidytable)

# Set up working directory

setwd("/Users/jkv465/Desktop/Work_EWS/New_Data")

# Open EWS + Blood Tests + ITA

ews <- read_parquet("EWS_Blood_ITA.parquet")

# Open procedures

procedures <- read_parquet("procedures_newest.parquet")

# Further modification

procedures <- procedures |> 
  select(-EnterpriseID,-SKS_Code,-Procedurenavn)

# Count the number of distinct procedures each individual (per hospitalization number, i.e. CSN) has been exposed to

csns_interventions <- procedures |> 
  group_by(CSN) |> 
  distinct(SKS_Group) |> 
  arrange(CSN)

# Individuals that have received Anesthesia or Intensive Care or Surgical Operations

csns_interventions <- csns_interventions |> 
  filter(SKS_Group == "Anesthesia or Intensive Care" | SKS_Group == "Surgical Operations")

csns_interventions <- na.omit(csns_interventions)


# Now we need to go back to the EWS data

# If we have individuals that have been exposed to Anesthesia/ITA or Surgical Operations, we consider that there has been an intervention.

ews <- ews |> 
  mutate(Interventions = if_else(CSN %in% csns_interventions$CSN, 1, 0))


# Now let's save this as parquet

write_parquet(ews,"EWS_Blood_ITA_Procedures.parquet")
