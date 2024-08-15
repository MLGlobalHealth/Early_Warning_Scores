# Pre-processing of diagnoses for EWS purposes

library(arrow)
library(tidyverse)

setwd("/Users/jkv465/Desktop/Work_EWS/New_Data")


# Open the dataset

diagnoses <- open_dataset("diagnoses_newest_with_sks_group.parquet")

# Create a new category based on the Aktionsdiagnose kode

diagnoses <- diagnoses %>%
  mutate(`Aktionsdiagnose kode` = str_sub(`Aktionsdiagnose kode`,2)) |> 
  mutate(category = case_when(
    `Aktionsdiagnose kode` >= "A00" & `Aktionsdiagnose kode` <= "B99" ~ "Certain infectious and parasitic diseases",
    `Aktionsdiagnose kode` >= "C00" & `Aktionsdiagnose kode` <= "D49" ~ "Neoplasms",
    `Aktionsdiagnose kode` >= "D50" & `Aktionsdiagnose kode` <= "D89" ~ "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
    `Aktionsdiagnose kode` >= "E00" & `Aktionsdiagnose kode` <= "E89" ~ "Endocrine, nutritional and metabolic diseases",
    `Aktionsdiagnose kode` >= "F01" & `Aktionsdiagnose kode` <= "F99" ~ "Mental, Behavioral and Neurodevelopmental disorders",
    `Aktionsdiagnose kode` >= "G00" & `Aktionsdiagnose kode` <= "G99" ~ "Diseases of the nervous system",
    `Aktionsdiagnose kode` >= "H00" & `Aktionsdiagnose kode` <= "H59" ~ "Diseases of the eye and adnexa",
    `Aktionsdiagnose kode` >= "H60" & `Aktionsdiagnose kode` <= "H95" ~ "Diseases of the ear and mastoid process",
    `Aktionsdiagnose kode` >= "I00" & `Aktionsdiagnose kode` <= "I99" ~ "Diseases of the circulatory system",
    `Aktionsdiagnose kode` >= "J00" & `Aktionsdiagnose kode` <= "J99" ~ "Diseases of the respiratory system",
    `Aktionsdiagnose kode` >= "K00" & `Aktionsdiagnose kode` <= "K95" ~ "Diseases of the digestive system",
    `Aktionsdiagnose kode` >= "L00" & `Aktionsdiagnose kode` <= "L99" ~ "Diseases of the skin and subcutaneous tissue",
    `Aktionsdiagnose kode` >= "M00" & `Aktionsdiagnose kode` <= "M99" ~ "Diseases of the musculoskeletal system and connective tissue",
    `Aktionsdiagnose kode` >= "N00" & `Aktionsdiagnose kode` <= "N99" ~ "Diseases of the genitourinary system",
    `Aktionsdiagnose kode` >= "O00" & `Aktionsdiagnose kode` <= "O9A" ~ "Pregnancy, childbirth and the puerperium",
    `Aktionsdiagnose kode` >= "P00" & `Aktionsdiagnose kode` <= "P96" ~ "Certain conditions originating in the perinatal period",
    `Aktionsdiagnose kode` >= "Q00" & `Aktionsdiagnose kode` <= "Q99" ~ "Congenital malformations, deformations and chromosomal abnormalities",
    `Aktionsdiagnose kode` >= "R00" & `Aktionsdiagnose kode` <= "R99" ~ "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
    `Aktionsdiagnose kode` >= "S00" & `Aktionsdiagnose kode` <= "T88" ~ "Injury, poisoning and certain other consequences of external causes",
    `Aktionsdiagnose kode` >= "V00" & `Aktionsdiagnose kode` <= "Y99" ~ "External causes of morbidity",
    `Aktionsdiagnose kode` >= "Z00" & `Aktionsdiagnose kode` <= "Z99" ~ "Factors influencing health status and contact with health services",
    `Aktionsdiagnose kode` >= "U00" & `Aktionsdiagnose kode` <= "U85" ~ "Codes for special purposes",
    TRUE ~ "Unknown category"
  ))

# Now save the diagnoses category

arrow::write_parquet(diagnoses,"diagnoses_grouped.parquet")

# Let's look at the top-10 categories

diagnoses_plot <- diagnoses |> 
  group_by(category) |> 
  count(sort = T) |>
  head(10) |> 
  collect() |> 
  ggplot(aes(y = n, x = fct_reorder(category,-n), fill = category)) +
  geom_col() + 
  scale_x_discrete(labels = scales::label_wrap(width = 20)) + 
  theme_gray(base_size = 12) +
  theme(legend.position = "topleft") + 
  labs(x = NULL, y = NULL)

# Save the plot 

ggsave(diagnoses_plot,dpi = 500,filename = "SKS_By_Frequency.tiff",height = 10, width = 20)
