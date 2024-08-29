# Set working directory to where the data is

setwd("/Users/jkv465/Desktop/Work_EWS")

# Working directory for the rest should be found in the S-drive

#########################################
############# Packages ##################
#########################################

library(tidyverse) # For data analysis
library(data.table) # For fast data reading
library(doParallel) # Parallel processing
library(Publish) # For table 1 summary
library(arrow) # For parquet files


####################################################
########### Pre-processing of data #################
####################################################


#  Original data
table_df <- data.table::fread("scoreData_first20_clean_v2.csv")


# Gathering data from hospitals
hospitals <- data.table::fread("hospital_department_mapping_full.csv")


# We also need some data from the areas of the hospitals and the specific department (more thorough)

areas <- read_csv("df_areas.csv")


# Translate them into a factor for the analysis

areas <- areas |> 
  mutate_at(vars(AFSNIT_SPECIALE), as.factor)


# Keep only the columns we need
areas <- areas |>
  select(ptID, hospital_area_id, AFSNIT_SPECIALE)


# Merge the areas/departments with the rest of the dataframe

table_df <- table_df |> 
  left_join(areas, by = "ptID")


# Hospitals will be erased from the original dataframe
# We are going to use a different dataset which is more thorough

table_df <- table_df |> 
  select(-Hospital)


# Now we need some general reshaping to make variables in English

# For Oxygen Supplement
table_df <- table_df |> 
  mutate(Ilttilskud = if_else(Ilttilskud == 0, "Air", "Oxygen")) |>
  mutate_at(vars(Ilttilskud),as.factor) |> 
  rename(Oxygen_Supplement = Ilttilskud)


# For Consciousness
table_df <- table_df |> 
  mutate(Consciousness = case_when(Bevidsthed == "A" ~ "A",
                                   Bevidsthed == "P" ~ "VPU",
                                   Bevidsthed == "U" ~ "VPU",
                                   Bevidsthed == "V" ~ "VPU")) |> 
  mutate_at(vars(Consciousness),as.factor) |> 
  dplyr::select(-Bevidsthed)


# For the Sex variable
table_df <- table_df |> 
  mutate(Sex = case_when(sex == "Mand" ~ "Male", 
                         sex == "Kvinde" ~ "Female",
                         .default = NA)) |> 
  mutate_at(vars(Sex),as.factor) |> 
  dplyr::select(-sex)

# Restrict to age 105

table_df <- table_df |> 
  mutate(Age = if_else(age > 105, 105, age))


# Add the hospitals in the dataframe
table_df <- table_df |> 
  left_join(hospitals,by = "ADT_DEPARTMENT_ID")


table_df <- table_df |> 
  mutate(Hospital = if_else(Hospital == "",NA, Hospital)) |> 
  mutate_at(vars(Hospital),as.factor)


# Let's redo the hospitals based on area code id

table_df <- table_df |>
  mutate(Hospital = case_when(hospital_area_id == "5" ~ "Amager and Hvidovre Hospital",
                              hospital_area_id == "7" ~ "Bornholm's Hospital",
                              hospital_area_id == "3" ~ "Bispebjerg and Frederiksberg Hospitals",
                              hospital_area_id == "2" ~ "Rigshospitalet",
                              hospital_area_id == "13" ~ "Nykøbing Sygehus",
                              hospital_area_id == "14" ~ "Næstved, Slagelse and Ringsted Hospitals",
                              hospital_area_id ==  "9" ~ "Zealands University Hospital",
                              hospital_area_id == "1" ~  "HGH, Hervel and Gentofte Hospital",
                              hospital_area_id == "6" ~ "NOH, Hospital of North Zealand",
                              hospital_area_id == "12" ~ "Holbæk Sygehus",
                              .default = NA
  )) |>
  mutate_at(vars(Hospital),as.factor)


# Respiration Rate
table_df <- table_df |> 
  rename(Respiration_Rate = Resp_frekvens)


# Pulse

table_df <- table_df |> 
  rename(Pulse = Puls)


# Now remove the redundant variables

table_df <- table_df |> 
  select(-age,-Bevidsthed.Fak)


# Keep on the variables we want

table_df <- table_df |> 
  select(ptID,Hospital,mort7D,mort24H,mort48H,mort30D,recorded_time,AFSNIT_SPECIALE,
         ADT_DEPARTMENT_ID,EWS_score,Age,Sex,Pulse,followUpDays,eventIndicator,Hosp_spent_days,
         Respiration_Rate,Temperature,Saturation,Oxygen_Supplement,HOSP_DISCH_TIME,deathDate,
         Blood_Pressure.Dia,Hospital,Blood_Pressure.Sys,Consciousness,indexCase)


# Now we will keep the departments and transform the NULL to NAs 

table_df <- table_df |> 
  mutate(Department_Name = if_else(AFSNIT_SPECIALE == "NULL", NA, AFSNIT_SPECIALE)) |>
  mutate_at(vars(Department_Name), as.factor)

table_df <- table_df |>  
  select(-AFSNIT_SPECIALE)


# Order on ptID

table_df <- table_df |> 
  arrange(ptID,recorded_time)

table_df <- as.data.frame(table_df)


# Load the hosp data

hosp_df <- data.table::fread("df_june.csv")

hosp_df <- hosp_df |> 
  filter(ptID %in% table_df$ptID)

hosp_df <- hosp_df |> 
  select(ptID,recorded_time,ADT_DEPARTMENT_ID,Hospitalization_No)

table_df <- table_df |> 
  left_join(hosp_df, by = c("ptID", "recorded_time", "ADT_DEPARTMENT_ID"))


# Remove the NA hospitalizations

table_df <- table_df |> 
  tidylog::filter(!is.na(Hospitalization_No)) 


# Compute the number of previous hospitalizations for each person at recording time

table_df <- table_df |>
  group_by(ptID) |>
  mutate(Previous_Hosp = cumsum(!duplicated(Hospitalization_No)) - 1) |>
  ungroup()

# Compute the number of measurements per person and per hospitalization

table_df <- table_df |>
  group_by(ptID,Hospitalization_No) |>
  mutate(Measurements = row_number()) |>
  ungroup()

# Create risk groups based on NEWS2 guidelines (at initial measurement)

#table_df <- table_df |> 
  #mutate(Risk_Groups_EWS = case_when(
    #EWS_score >= 7 ~ "High",
    #EWS_score >= 5 & EWS_score <= 6 ~ "Medium",
    #(Respiration_Rate <= 8 | Respiration_Rate >=25) | (Saturation <= 91) | 
      #(Pulse <= 40 | Pulse >= 131) | (Consciousness == "VPU") | (Temperature <= 35) | 
      #(Blood_Pressure.Sys <= 90 | Blood_Pressure.Sys >= 220) ~ "Low-Medium",
    #EWS_score >= 0 & EWS_score <= 4 ~ "Low")) |> 
  #mutate(Risk_Groups_EWS = as.factor(Risk_Groups_EWS))

# We also need a variable that gives different IDs if the hospitalization number is different.

table_df <- table_df %>%
  group_by(ptID) |> 
  mutate(Identifier = paste0(ptID, '_', cumsum(Hospitalization_No != lag(Hospitalization_No, default = first(Hospitalization_No))))) |> 
  ungroup() |> 
  relocate(Identifier,.after = ptID)

table_df <- table_df |>  
  relocate(Hospitalization_No,.after = Identifier)

table_df <- table_df |> 
  group_by(Identifier) |> 
  mutate(Mean_NEWS = mean(EWS_score), Max_NEWS = max(EWS_score), Var_NEWS = var(EWS_score)) |>
  mutate(Var_NEWS = if_else(is.na(Var_NEWS),0,Var_NEWS)) |> 
  ungroup() |> 
  relocate(Mean_NEWS,.after = EWS_score) |> 
  relocate(Max_NEWS, .after = Mean_NEWS) |> 
  relocate(Var_NEWS, .after = Max_NEWS) |> 
  filter(!duplicated(Identifier))

# Select the additional data

setwd("/Users/jkv465/Desktop/Work_EWS/New_Data")

data <- read_parquet("df_august.parquet")

new_col_data <- data |> 
  select(Hemoglobin,Leukocytes,Trombocytes,Kreatinin,ALAT,LDH,Albumin,CRP,Laktak_ab,Laktat_vb,ITA_Start,ITA_Indicator,Interventions,SKS_Category,Department_Name_Fac,Previous_Hosp_Fac)

table_df <- bind_cols(table_df,new_col_data)


# We need this df to be saved

setwd("/Users/jkv465/Desktop/Work_EWS/New_Data/Pre-Processed")

write_parquet(table_df,"table1_ews.parquet")


# Creation of Table 1 dataframe

table_df <- table_df |>
    mutate(Status30D = if_else(mort30D == 1, "Deceased", "Alive")) |>
    mutate_at(vars(Status30D), as.factor)

# Now create Table 1

table1 <- summary(utable(Status30D ~  Age + Sex + Consciousness + Oxygen_Supplement +
                                      Respiration_Rate + Pulse + Temperature + Saturation +
                                      Hemoglobin + Leukocytes + Trombocytes + Kreatinin + ALAT + LDH + Albumin + CRP + 
                                      Laktak_ab + Laktat_vb + ITA_Indicator + Interventions + SKS_Category + 
                                      Blood_Pressure.Sys + Blood_Pressure.Dia, data = table_df))


# Print the table1

table1
