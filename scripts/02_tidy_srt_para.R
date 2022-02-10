# 02 - a script to pull the data for para only in the sentence reading task

library(here)
library(tidyverse)
library(janitor)
library(fs)

para_data <- dir_ls(here("data", "raw"), regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source", col_types = cols(.default = "c")) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  select(c(1, 2, 8:10)) %>% 
  filter(para == "para") %>% 
  filter(duration_2 != "N/A") %>% 
  rename(col1 = users_kyleparrish_documents_git_hub_andres_rojas_consult_data_raw_p1e_sp1_post_csv) %>% 
  mutate(col1 = 
           str_remove(col1, "/Users/kyleparrish/Documents/GitHub/andres_rojas_consult/data/raw/")) %>% 
  mutate(col1 = str_remove(col1, ".csv")) %>% 
  separate(col1, into = c("participant", "level", "time"))

para_data %>% 
  write.csv(here("data", "tidy", "para_data.csv"))

