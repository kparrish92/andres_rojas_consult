library(here)
library(tidyverse)
library(janitor)
library(fs)
library(lme4)
library(lmerTest)

# load tidy data 
source(here("scripts", "00_helpers.R"))
source(here("scripts", "05_load_data.R"))

srt = srt %>% 
  filter(!duration == "N/A" & !duration == "N/A /d/")

srt$duration = as.numeric(srt$duration)

srt_tap = srt %>%
  filter(is_trill == "tap") %>% 
  filter(!is.na(duration)) 

srt_trill = srt %>% 
  filter(is_trill == "trill") %>% 
  filter(!is.na(duration))

srt_tap$time <- relevel(as.factor(srt_tap$time), ref = "PRE")
srt_trill$time <- relevel(as.factor(srt_trill$time), ref = "PRE")


# Do nested model comparisons to determine SRT  
# whether a main effect for time exists 
null_mod_tap_srt = lmer(duration ~ 1 + 
                          (time | participant), 
                        data = srt_tap)

time_mod_tap_srt = lmer(duration ~ time + 
                          (time | participant), 
                        data = srt_tap)

group_mod_tap_srt = lmer(duration ~ time + level + 
                          (time | participant), 
                        data = srt_tap)

int_mod_tap_srt = lmer(duration ~ time + level + time:level +
                           (time | participant), 
                         data = srt_tap)

nmc_srt_tap = anova(null_mod_tap_srt, time_mod_tap_srt, group_mod_tap_srt,
      int_mod_tap_srt)

nmc_srt_tap %>% 
  write.csv(here("data", "tidy", "nmc_srt_tap.csv"))

int_mod_tap_srt %>% 
  write_rds(here("data", "models", "srt_tap.rds"))


######

null_mod_trill_srt = lmer(duration ~ 1 + 
                          (time | participant), 
                        data = srt_trill)

time_mod_trill_srt = lmer(duration ~ time + 
                          (time | participant), 
                        data = srt_trill)

group_mod_trill_srt = lmer(duration ~ time + level + 
                           (time | participant), 
                         data = srt_trill)

int_mod_trill_srt = lmer(duration ~ time + level + time:level +
                         (time | participant), 
                       data = srt_trill)

nmc_srt_trill = anova(null_mod_trill_srt, time_mod_trill_srt, 
      group_mod_trill_srt, int_mod_trill_srt)

qqnorm(resid(int_mod_trill_srt))

nmc_srt_trill %>% 
  write.csv(here("data", "tidy", "nmc_srt_trill.csv"))

int_mod_trill_srt %>% 
  write_rds(here("data", "models", "srt_trill.rds"))



lectura_tap = lectura %>%
  filter(is_trill == "tap") %>% 
  filter(!is.na(duration)) 

lectura_trill = lectura %>% 
  filter(is_trill == "trill") %>% 
  filter(!is.na(duration))

lectura_tap$time <- relevel(as.factor(lectura_tap$time), ref = "PRE")
lectura_trill$time <- relevel(as.factor(lectura_trill$time), ref = "PRE")


# Do nested model comparisons to determine lectura  
# whether a main effect for time exists 
null_mod_tap_lectura = lmer(duration ~ 1 + 
                              (time | participant), 
                            data = lectura_tap)

time_mod_tap_lectura = lmer(duration ~ time + 
                              (time | participant), 
                            data = lectura_tap)

group_mod_tap_lectura = lmer(duration ~ time + level + 
                               (time | participant), 
                             data = lectura_tap)
int_mod_tap_lectura = lmer(duration ~ time + level + time:level +
                             (time | participant), 
                           data = lectura_tap)

nmc_lectura_tap = anova(null_mod_tap_lectura, time_mod_tap_lectura, group_mod_tap_lectura,
                        int_mod_tap_lectura)

nmc_lectura_tap %>% 
  write.csv(here("data", "tidy", "nmc_lectura_tap.csv"))

int_mod_tap_lectura %>% 
  write_rds(here("data", "models", "lectura_tap.rds"))

qqnorm(resid(int_mod_tap_lectura))

######

null_mod_trill_lectura = lmer(duration ~ 1 + 
                                (time | participant), 
                              data = lectura_trill)

time_mod_trill_lectura = lmer(duration ~ time + 
                                (time | participant), 
                              data = lectura_trill)

group_mod_trill_lectura = lmer(duration ~ time + level + 
                                 (time | participant), 
                               data = lectura_trill)

int_mod_trill_lectura = lmer(duration ~ time + level + time:level +
                               (time | participant), 
                             data = lectura_trill)

nmc_lectura_trill = anova(null_mod_trill_lectura, time_mod_trill_lectura, 
                          group_mod_trill_lectura, int_mod_trill_lectura)

qqnorm(resid(int_mod_trill_lectura))

nmc_lectura_trill %>% 
  write.csv(here("data", "tidy", "nmc_lectura_trill.csv"))

int_mod_trill_lectura %>% 
  write_rds(here("data", "models", "lectura_trill.rds"))


#### PC

image_df_tap = image_df %>%
  filter(is_trill == "tap") %>% 
  filter(!is.na(duration)) 

image_df_trill = image_df %>% 
  filter(is_trill == "trill") %>% 
  filter(!is.na(duration))


image_df_tap$time <- relevel(as.factor(image_df_tap$time), ref = "PRE")
image_df_trill$time <- relevel(as.factor(image_df_trill$time), ref = "PRE")

# Do nested model comparisons to determine image_df  
# whether a main effect for time exists 
null_mod_tap_image_df = lmer(duration ~ 1 + 
                               (1 + time | participant), 
                             data = image_df_tap)

time_mod_tap_image_df = lmer(duration ~ time + 
                               (1 + time | participant), 
                             data = image_df_tap)

group_mod_tap_image_df = lmer(duration ~ time + level + 
                                (1 + time | participant), 
                              data = image_df_tap)

int_mod_tap_image_df = lmer(duration ~ time + level + time:level +
                              (1 + time | participant), 
                            data = image_df_tap)

nmc_image_df_tap = anova(null_mod_tap_image_df, time_mod_tap_image_df, group_mod_tap_image_df,
                         int_mod_tap_image_df)

nmc_image_df_tap %>% 
  write.csv(here("data", "tidy", "nmc_image_df_tap.csv"))

int_mod_tap_image_df %>% 
  write_rds(here("data", "models", "image_df_tap.rds"))

qqnorm(resid(int_mod_tap_image_df))

######

null_mod_trill_image_df = lmer(duration ~ 1 + 
                                 (time | participant), 
                               data = image_df_trill)

time_mod_trill_image_df = lmer(duration ~ time + 
                                 (time | participant), 
                               data = image_df_trill)

group_mod_trill_image_df = lmer(duration ~ time + level + 
                                  (time | participant), 
                                data = image_df_trill)

int_mod_trill_image_df = lmer(duration ~ time + level + time:level +
                                (time | participant), 
                              data = image_df_trill)

nmc_image_df_trill = anova(null_mod_trill_image_df, time_mod_trill_image_df, 
                           group_mod_trill_image_df, int_mod_trill_image_df)

qqnorm(resid(int_mod_trill_image_df))

nmc_image_df_trill %>% 
  write.csv(here("data", "tidy", "nmc_image_df_trill.csv"))

int_mod_trill_image_df %>% 
  write_rds(here("data", "models", "image_df_trill.rds"))