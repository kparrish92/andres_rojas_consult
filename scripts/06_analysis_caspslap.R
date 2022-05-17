library(here)
library(tidyverse)
library(janitor)
library(fs)
library(lme4)

# load tidy data 
source(here("scripts", "00_helpers.R"))
source(here("scripts", "05_load_data.R"))

# Run t-test using helper function (see code in `00_helpers.R`. 

srt_sp1 = srt %>% filter(level == "sp1")
srt_sp2 = srt %>% filter(level == "sp102") 
srt_sp3 = srt %>% filter(level == "sp203") %>% 
  filter(duration != "23_126") 

# Spanish 1 t-tests
p1_tap_srt = run_t_test(df = srt_sp1, p_id = "P1E", segment = "tap") %>% 
  mutate(task = "srt")
p1_trill_srt = run_t_test(df = srt_sp1, p_id = "P1E", segment = "trill") %>% 
  mutate(task = "srt")

p3_tap_srt = run_t_test(df = srt_sp1, p_id = "P3E", segment = "tap") %>% 
  mutate(task = "srt")
p3_trill_srt = run_t_test(df = srt_sp1, p_id = "P3E", segment = "trill") %>% 
  mutate(task = "srt")

p5_tap_srt = run_t_test(df = srt_sp1, p_id = "P5E", segment = "tap") %>% 
  mutate(task = "srt")
p5_trill_srt = run_t_test(df = srt_sp1, p_id = "P5E", segment = "trill") %>% 
  mutate(task = "srt")

p6_tap_srt = run_t_test(df = srt_sp1, p_id = "P6E", segment = "tap") %>% 
  mutate(task = "srt")
p6_trill_srt = run_t_test(df = srt_sp1, p_id = "P6E", segment = "trill") %>% 
  mutate(task = "srt")


p7_tap_srt = run_t_test(df = srt_sp1, p_id = "P7E", segment = "tap") %>% 
  mutate(task = "srt")
p7_trill_srt = run_t_test(df = srt_sp1, p_id = "P7E", segment = "trill") %>% 
  mutate(task = "srt")

p8_tap_srt = run_t_test(df = srt_sp1, p_id = "P8E", segment = "tap") %>% 
  mutate(task = "srt")
p8_trill_srt = run_t_test(df = srt_sp1, p_id = "P8E", segment = "trill") %>% 
  mutate(task = "srt")

# Spanish 2 t-tests

p1_tap_srt_2 = run_t_test(df = srt_sp2, p_id = "P1E", segment = "tap") %>% 
  mutate(task = "srt")
p1_trill_srt_2 = run_t_test(df = srt_sp2, p_id = "P1E", segment = "trill") %>% 
  mutate(task = "srt")

p2_tap_srt_2 = run_t_test(df = srt_sp2, p_id = "P2E", segment = "tap") %>% 
  mutate(task = "srt")
p2_trill_srt_2 = run_t_test(df = srt_sp2, p_id = "P2E", segment = "trill") %>% 
  mutate(task = "srt")

p3_tap_srt_2 = run_t_test(df = srt_sp2, p_id = "P3E", segment = "tap") %>% 
  mutate(task = "srt")
p3_trill_srt_2 = run_t_test(df = srt_sp2, p_id = "P3E", segment = "trill") %>% 
  mutate(task = "srt")

# Spanish 3 t-tests

p1_tap_srt_3 = run_t_test(df = srt_sp3, p_id = "P1E", segment = "tap") %>% 
  mutate(task = "srt")
p1_trill_srt_3 = run_t_test(df = srt_sp3, p_id = "P1E", segment = "trill") %>% 
  mutate(task = "srt")

p2_tap_srt_3 = run_t_test(df = srt_sp3, p_id = "P2E", segment = "tap") %>% 
  mutate(task = "srt")
p2_trill_srt_3 = run_t_test(df = srt_sp3, p_id = "P2E", segment = "trill") %>% 
  mutate(task = "srt")

# combine output into df for visualizations and reporting

srt_t_df <- rbind(p1_tap_srt, p1_trill_srt, 
      p3_tap_srt, p3_trill_srt, 
      p5_tap_srt, p5_trill_srt,
      p6_tap_srt, p6_trill_srt,
      p7_tap_srt, p7_trill_srt,
      p8_tap_srt, p8_trill_srt)


srt_t_df %>% 
  write.csv(here("data", "tidy", "srt_t_tests.csv"))

# Spanish 2 

srt_t_df_2 = rbind(p1_tap_srt_2, p1_trill_srt_2,
                   p2_tap_srt_2, p2_trill_srt_2,
                   p3_tap_srt_2, p3_trill_srt_2)

srt_t_df_2 %>% 
  write.csv(here("data", "tidy", "srt_t_tests_2.csv"))

# Spanish 3

srt_t_df_3 = rbind(p1_tap_srt_3, p1_trill_srt_3, 
      p2_tap_srt_3, p2_trill_srt_3)

srt_t_df_3 %>% 
  write.csv(here("data", "tidy", "srt_t_tests_3.csv"))

  
