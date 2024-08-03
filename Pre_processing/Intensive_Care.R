library(arrow)
library(tidyverse)

# Opening the intensive care dataset

setwd("/Users/jkv465/Desktop/Work_EWS/New_Data")

ic <- open_dataset("intensive_care.parquet")

ic <- ic |> 
  rename("Respirator_Start" = "Respirator start",
          "Respirator_End" = "Respirator slut",
          "ITA_Start" = "ITA start",
          "ITA_End" = "ITA slut",
          "ITA_Department" = "Ita Afsnit",
          "Respiration_Num" = "Antal respiratorbehandlinger") 

# Save the dataset

write_parquet(ic,"intensive_care.parquet")
