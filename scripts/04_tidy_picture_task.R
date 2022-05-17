# 04 tidy imagen 
library(here)
library(tidyverse)
library(janitor)
library(fs)

imagen_df <- dir_ls(here("data", "raw"), regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source", col_types = cols(.default = "c")) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  select(c(1:7)) %>% 
  filter(sentence == "Imagen A" | sentence == "Imagen B" |
           sentence == "Imagen C" | sentence == "Imagen D" |
           sentence == "Imagen E" | sentence == "Imagen F" |
           sentence == "Imagen G" | sentence == "Imagen H") %>% 
  rename(col1 = users_kyleparrish_documents_git_hub_andres_rojas_consult_data_raw_p1e_sp102_post_csv) %>% 
  mutate(col1 = 
           str_remove(col1, "/Users/kyleparrish/Documents/GitHub/andres_rojas_consult/data/raw/")) %>% 
  mutate(col1 = str_remove(col1, ".csv")) %>% 
  separate(col1, into = c("participant", "level", "time")) %>% 
  mutate(is_trill = case_when(!is.na(duration_taps) ~ "tap",
                              !is.na(duration_trills) ~ "trill")) %>% 
  unite("duration", duration_taps:duration_trills) %>%
  unite("token", tap:trill) %>% 
  mutate(duration = str_remove(duration, "NA_")) %>% 
  mutate(duration = str_remove(duration, "_NA")) %>% 
  mutate(token = str_remove(token, "_NA")) %>% 
  mutate(token = str_remove(token, "NA_")) %>% 
  filter(duration != "N/A") %>%   
  filter(duration != "NA") %>% 
  filter(duration != "N/P") 
  

imagen_df = imagen_df %>% 
  mutate(no_occlusions = substr(occlusions_cat, 1, 1)) 

imagen_df = imagen_df %>% 
  mutate("level_2" = level) %>% 
  unite("participant", level, participant, sep = "_") %>% 
  rename(level = level_2) %>%
  mutate(level = replace(level, level == "sp203" | level == "sp204", "sp203/204")) 

x = imagen_df %>% 
  group_by(participant, time) %>% 
  summarize(n = n())

imagen_df %>% 
  write.csv(here("data", "tidy", "image_df.csv"))



