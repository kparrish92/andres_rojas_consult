library(here)
library(tidyverse)
library(janitor)
library(fs)
library(lme4)

# load tidy data 
source(here("scripts", "00_helpers.R"))
source(here("scripts", "05_load_data.R"))

### srt_tidy trills 

srt_tidy_trill = srt %>% 
  filter(is_trill == "trill") %>% 
  filter(no_occlusions == 0 | no_occlusions == 1 | 
           no_occlusions == 2 | no_occlusions == 3 | 
           no_occlusions == 4)


srt_tidy_trill$time <- relevel(as.factor(srt_tidy_trill$time), ref = "PRE")


null_mod_trill_srt = lmer(as.numeric(no_occlusions) ~ 1 + 
                            (time | participant), 
                          data = srt_tidy_trill)

time_mod_trill_srt = lmer(as.numeric(no_occlusions) ~ time + 
                            (time | participant), 
                          data = srt_tidy_trill)

group_mod_trill_srt = lmer(as.numeric(no_occlusions) ~ time + level + 
                             (time | participant), 
                           data = srt_tidy_trill)

int_mod_trill_srt = lmer(as.numeric(no_occlusions) ~ time + level + 
                           time:level +
                           (time | participant), 
                         data = srt_tidy_trill)

nmc_trill_cont = anova(null_mod_trill_srt, time_mod_trill_srt, 
      group_mod_trill_srt, int_mod_trill_srt)

nmc_trill_cont %>% 
  write.csv(here("data", "tidy", "nmc_trill_cont.csv"))

int_mod_trill_srt %>% 
  write_rds(here("data", "models", "srt_trill_cont.rds"))


### Lectura trills 


lectura$time <- relevel(as.factor(lectura$time), ref = "PRE")

null_mod_trill_lec = lmer(as.numeric(no_occlusions) ~ 1 + 
                            (time | participant), 
                          data = lectura %>% filter(is_trill == "trill"))

time_mod_trill_lec = lmer(as.numeric(no_occlusions) ~ time + 
                            (time | participant), 
                          data = lectura %>% filter(is_trill == "trill"))

group_mod_trill_lec = lmer(as.numeric(no_occlusions) ~ time + level + 
                             (time | participant), 
                           data = lectura %>% filter(is_trill == "trill"))

int_mod_trill_lec = lmer(as.numeric(no_occlusions) ~ time + level + 
                           time:level +
                           (time | participant), 
                         data = lectura %>% filter(is_trill == "trill"))

nmc_trill_cont_lec = anova(null_mod_trill_lec, time_mod_trill_lec, 
      group_mod_trill_lec, int_mod_trill_lec)

nmc_trill_cont_lec %>% 
  write.csv(here("data", "tidy", "nmc_trill_cont_lec.csv"))

int_mod_trill_lec %>% 
  write_rds(here("data", "models", "int_mod_trill_cont_lec.rds"))


### PCT trills 

image_df_trill = image_df %>% 
  filter(is_trill == "trill") %>% 
  filter(!is.na(no_occlusions))

image_df_trill$time <- relevel(as.factor(image_df_trill$time), ref = "PRE")

null_mod_trill_pct = lmer(as.numeric(no_occlusions) ~ 1 + 
                            (time | participant), 
                          data = image_df_trill)


time_mod_trill_pct = lmer(as.numeric(no_occlusions) ~ time + 
                            (time | participant), 
                          data = image_df_trill)

group_mod_trill_pct = lmer(as.numeric(no_occlusions) ~ time + level + 
                             (time | participant), 
                           data = image_df_trill)

int_mod_trill_pct = lmer(as.numeric(no_occlusions) ~ time + level + 
                           time:level +
                           (time | participant), 
                         data = image_df_trill)

nmc_pct_cont_trill = anova(null_mod_trill_pct, time_mod_trill_pct, 
      group_mod_trill_pct, int_mod_trill_pct)


nmc_pct_cont_trill %>% 
  write.csv(here("data", "tidy", "nmc_pct_cont_trill.csv"))

int_mod_trill_pct %>% 
  write_rds(here("data", "models", "int_mod_trill_pct_cont.rds"))



