# 05 load data 

library(here)

srt <- read.csv(here("data", "tidy", "srt_data.csv"))
para <- read.csv(here("data", "tidy", "para_data.csv"))
lectura <- read.csv(here("data", "tidy", "lectura_tidy.csv"))
image <- read.csv(here("data", "tidy", "image_df.csv"))