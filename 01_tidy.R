# A script to tidy individual participant data 

# Libraries 

library(here)
library(tidyverse)
library(janitor)


# get sentence reading task

# extract 
p1_pre <- read.csv(here("data", "P1E_sp1_PRE.csv"), na.strings=c("","NA")) %>% 
  select(c(1:6)) %>%
  row_to_names(row_number = 1) %>% 
  clean_names() 

p1_pre_srt <- p1_pre[c(1:158),]

p1_pre_srt_tap <- p1_pre_srt %>% 
  filter(!is.na(duration)) %>% 
  filter(duration != "N/A") %>% 
  filter(sentence != 60)

p1_post <- read.csv(here("data", "P1E_sp1_POST.csv"), na.strings=c("","NA")) %>% 
  select(c(1:6)) %>%
  row_to_names(row_number = 1) %>% 
  clean_names() 

p1_post_srt <- p1_post[c(1:158),] 

p1_post_srt_tap <- p1_post_srt %>% 
  filter(!is.na(duration)) %>% 
  filter(duration != "N/A")


# t.test 

p1_post_srt_tap$duration = as.numeric(p1_post_srt_tap$duration)
p1_pre_srt_tap$duration = as.numeric(p1_pre_srt_tap$duration)

t.test(p1_post_srt_tap$duration, p1_pre_srt_tap$duration, paired = TRUE)



