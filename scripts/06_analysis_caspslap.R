library(here)
library(tidyverse)
library(janitor)
library(fs)
library(lme4)

# load tidy data 
source(here("scripts", "05_load_data.R"))

# Run t-test using helper function (see code in `00_helpers.R`. 

p1_tap_srt = run_t_test(df = srt, p_id = "P1E", segment = "tap") %>% 
  mutate(task = "srt")
p1_trill_srt = run_t_test(df = srt, p_id = "P1E", segment = "trill") %>% 
  mutate(task = "srt")

p3_tap_srt = run_t_test(df = srt, p_id = "P3E", segment = "tap") %>% 
  mutate(task = "srt")
p3_trill_srt = run_t_test(df = srt, p_id = "P3E", segment = "trill") %>% 
  mutate(task = "srt")

p5_tap_srt = run_t_test(df = srt, p_id = "P5E", segment = "tap") %>% 
  mutate(task = "srt")
p5_trill_srt = run_t_test(df = srt, p_id = "P5E", segment = "trill") %>% 
  mutate(task = "srt")

p6_tap_srt = run_t_test(df = srt, p_id = "P6E", segment = "tap") %>% 
  mutate(task = "srt")
p6_trill_srt = run_t_test(df = srt, p_id = "P6E", segment = "trill") %>% 
  mutate(task = "srt")

# combine output into df for visualizations and reporting

srt_t_df <- rbind(p1_tap_srt, p1_trill_srt, 
      p3_tap_srt, p3_trill_srt, 
      p5_tap_srt, p5_trill_srt,
      p6_tap_srt, p6_trill_srt)

srt_t_df %>% 
  write.csv(here("data", "tidy", "srt_t_tests.csv"))

# descriptive stuff
srt %>% 
  filter(!duration == "N/A" & !duration == "N/A /d/") %>% 
  group_by(participant, is_trill, time) %>% 
  summarize(mean = mean(as.numeric(duration)))

# plots 
srt %>% 
  filter(!duration == "N/A" & !duration == "N/A /d/") %>%
  ggplot(aes(x = as.numeric(duration), y = time, color = is_trill)) + geom_boxplot() + 
  facet_wrap(~participant)

  
