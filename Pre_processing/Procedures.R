library(arrow)
library(tidyverse)
library(tidytext)
library(stm)

setwd("/Users/jkv465/Desktop/Work_EWS/New_Data")

procedures <- open_dataset("procedures_newest.parquet") # Do not overwrite this

procedures <- procedures |> 
  mutate(SKS_Group = case_when(str_starts(SKS_Code,"A") ~ "Administrative Matters",
                                           str_starts(SKS_Code,"B") ~ "Treatment Care",
                                           str_starts(SKS_Code,"D") ~ "Diseases Health Conditions",
                                           str_starts(SKS_Code,"E") ~ "External Causes Injury",
                                           str_starts(SKS_Code,"F") ~ "Functional Ability",
                                           str_starts(SKS_Code,"K") ~ "Surgical Operations",
                                           str_starts(SKS_Code,"M") ~ "Drug Substance ATC",
                                           str_starts(SKS_Code,"N") ~ "Anesthesia or Intensive Care",
                                           str_starts(SKS_Code,"R") ~ "Results report",
                                           str_starts(SKS_Code,"U") ~ "Clinical Examinations / Radiological",
                                           str_starts(SKS_Code,"W") ~ "Clinical Physiological Examinations",
                                           str_starts(SKS_Code,"Z") ~ "Various Procedures",
                                           TRUE ~ NA_character_)) |> 
  relocate(SKS_Group,.after = SKS_Code)

# Save procedures

# write_parquet(procedures,"procedures_newest.parquet")

procedures_plot <- procedures |> 
  group_by(SKS_Group) |> 
  count(SKS_Group,sort = T) |> 
  head(8) |> 
  collect() |> 
  ggplot(aes(x = fct_reorder(SKS_Group,-n), y = n, fill = SKS_Group)) + 
  geom_col() + 
  theme_gray(base_size = 12) + 
  labs(x = NULL, y = NULL) + 
  scale_x_discrete(labels = scales::label_wrap(width = 20)) +
  theme(legend.position = "topleft")

# ggsave(plot = procedures_plot,filename = "procedures_frequency.tiff",dpi = 500, width = 15, height = 10)


# There is more to that data, so we need to do some text mining

# Some functions needed

################################

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}


scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}

######################################

procedures |> head(10) |> collect()

procedures_tok <- procedures |> 
  head(2000000) |> 
  collect() |> 
  mutate(line = row_number()) |> 
  unnest_tokens(word,Procedurenavn) |> 
  anti_join(get_stopwords(language = "da")) |> 
  filter(word != "og", word != "af", word != "eller",word != "cm", word != "med", word != "til", word != "ikke", word != "ved", word != "melding", word != "observation")

procedures_tok |> 
  count(word,sort = TRUE)

procedures_tf_idf <- procedures_tok |> 
    count(SKS_Group, word, sort = TRUE) %>%
    bind_tf_idf(word, SKS_Group, n) %>%
    arrange(-tf_idf) %>%
    group_by(SKS_Group) %>%
    top_n(10) %>%
    ungroup

procedures_plot_tf <- procedures_tf_idf %>%
    mutate(word = reorder_within(word, tf_idf, SKS_Group)) %>%
    ggplot(aes(word, tf_idf, fill = SKS_Group)) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ SKS_Group, scales = "free",ncol = 2) +
    scale_x_reordered() +
    coord_flip() +
    theme_gray(base_size = 12) + 
    labs(x = NULL, y = "Term Frequency - Inverse Document Frequency (TF-IDF)")

# ggsave(filename = "TermFrequency_Procedures.tiff",procedures_plot_tf,width = 25,height = 15,dpi = 500)

# Topic modelling

procedures_dfm <- procedures_tok |> 
  count(SKS_Group, word, sort = TRUE) |> 
  cast_dfm(SKS_Group, word, n)

# 6 topics

topic_model <- stm(procedures_dfm, K = 6, verbose = FALSE, init.type = "Spectral")

td_beta <- tidy(topic_model)

topics_plot <- td_beta %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    mutate(topic = paste0("Topic ", topic),
           term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = as.factor(topic))) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    theme_gray(base_size = 12) +
    labs(x = NULL, y = expression(beta),
         title = "Highest word probabilities for each topic",
         subtitle = "Different words are associated with different topics")

# ggsave(filename = "Topics_Procedures.tiff",topics_plot,width = 25,height = 15,dpi = 500)

td_gamma <- tidy(topic_model, matrix = "gamma", document_names = rownames(procedures_dfm))

ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
    geom_histogram(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ topic, ncol = 3) +
    labs(title = "Distribution of document probabilities for each topic",
         y = "Number of SKS groups", x = expression(gamma))
