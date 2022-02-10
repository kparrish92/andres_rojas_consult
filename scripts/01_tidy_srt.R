# A script to tidy individual participant data for the sentence reading task

# Libraries 

# 01 - a script to pull the data for para only in the sentence reading task

library(here)
library(tidyverse)
library(janitor)
library(fs)

list_of_files <- list.files(path = here("data", "raw"), recursive = TRUE,
                            pattern = "\\.csv$", 
                            full.names = TRUE) %>% 
  as.data.frame()

df_f <- character()
for(thisRun in 1:nrow(list_of_files))
{
  df <- read.csv(list_of_files$.[thisRun]) %>% 
    row_to_names(row_number = 1) %>% 
    clean_names() %>% 
    select(c(1:6)) %>% 
    slice(1:158) %>% 
    mutate("id" = list_of_files$.[thisRun]) %>% 
    mutate(col = 1:158)
  
  colnames(df) <- c('sentence','token1','token2', 'duration1','duration2',
                    'occlusions', 'id', 'trial')
  df_f <- rbind(df_f, df)  
}



empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}


srt_tidy <- df_f %>% 
  mutate_each(funs(empty_as_na)) %>% 
  mutate(is_trill = case_when(!is.na(duration1) ~ "tap",
                              !is.na(duration2) ~ "trill")) %>% 
  unite("duration", duration1:duration2) %>%
  unite("token", token1:token2) %>% 
  mutate(duration = str_remove(duration, "NA_")) %>% 
  mutate(duration = str_remove(duration, "_NA")) %>% 
  mutate(token = str_remove(token, "_NA")) %>% 
  mutate(token = str_remove(token, "NA_")) %>%  
  filter(!is.na(duration)) %>% 
  mutate(id = 
           str_remove(id, "/Users/kyleparrish/Documents/GitHub/andres_rojas_consult/data/raw/")) %>% 
  mutate(id = str_remove(id, ".csv")) %>% 
  separate(id, into = c("participant", "level", "time"))


srt_tidy %>% 
  group_by(participant, time, is_trill) %>% 
  summarize(n = n())

srt_tidy %>% 
  write.csv(here("data", "tidy", "srt_data.csv"))


