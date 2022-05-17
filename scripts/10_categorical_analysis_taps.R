library(here)
library(tidyverse)
library(janitor)
library(fs)
library(lme4)
library(ordinal)

# load tidy data 
source(here("scripts", "00_helpers.R"))
source(here("scripts", "05_load_data.R"))

### srt_tidy taps 

### Include this - most taps were realized as apporximates or with 1 
### occlusion 

srt_tidy_tap %>% 
  group_by(no_occlusions) %>% 
  summarize(n = n())

srt_tidy_tap = srt %>% 
  filter(is_trill == "tap") %>% 
  filter(no_occlusions == 1 | no_occlusions == "a") %>% 
  mutate(no_occlusions = replace(no_occlusions, no_occlusions == "a", 
                                 "approximate")) 




srt_tidy_tap$time <- relevel(as.factor(srt_tidy_tap$time), ref = "PRE")

null_mod_tap_srt = glmer(as.factor(no_occlusions) ~ 1 + 
                            (time | participant), family = "binomial"(link = "logit"), 
                          data = srt_tidy_tap)

time_mod_tap_srt = glmer(as.factor(no_occlusions) ~ time + 
                            (time | participant), family = "binomial"(link = "logit"),
                          data = srt_tidy_tap)

group_mod_tap_srt = glmer(as.factor(no_occlusions) ~ time + level + 
                             (time | participant), family = "binomial"(link = "logit"), 
                           data = srt_tidy_tap)

int_mod_tap_srt = glmer(as.factor(no_occlusions) ~ time + level + 
                           time:level +
                           (time | participant), family = "binomial"(link = "logit"),
                         data = srt_tidy_tap)

nmc_tap_cont = anova(null_mod_tap_srt, time_mod_tap_srt, 
                       group_mod_tap_srt, int_mod_tap_srt)

nmc_tap_cont %>% 
  write.csv(here("data", "tidy", "nmc_tap_cont.csv"))

int_mod_tap_srt %>% 
  write_rds(here("data", "models", "srt_tap_cont.rds"))

### lectura_tidy taps 

### Include this - most taps were realized as approximates or with 1 
### occlusion 

lectura %>% 
  filter(is_trill == "tap") %>% 
  group_by(no_occlusions) %>% 
  summarize(n = n())

lectura_tidy_tap = lectura %>% 
  filter(is_trill == "tap") %>% 
  filter(no_occlusions == 1 | no_occlusions == "a") %>% 
  mutate(no_occlusions = replace(no_occlusions, no_occlusions == "a", 
                                 "approximate")) 

lectura_tidy_tap$time <- relevel(as.factor(lectura_tidy_tap$time), ref = "PRE")


null_mod_tap_lectura = glmer(as.factor(no_occlusions) ~ 1 + 
                               (time | participant), family = "binomial"(link = "logit"), 
                             data = lectura_tidy_tap)

time_mod_tap_lectura = glmer(as.factor(no_occlusions) ~ time + 
                               (time | participant), family = "binomial"(link = "logit"),
                             data = lectura_tidy_tap)

group_mod_tap_lectura = glmer(as.factor(no_occlusions) ~ time + level + 
                                (time | participant), family = "binomial"(link = "logit"), 
                              data = lectura_tidy_tap)

int_mod_tap_lectura = glmer(as.factor(no_occlusions) ~ time + level + 
                              time:level +
                              (time | participant), family = "binomial"(link = "logit"),
                            data = lectura_tidy_tap)

nmc_tap_cont_lectura = anova(null_mod_tap_lectura, time_mod_tap_lectura, 
                             group_mod_tap_lectura, int_mod_tap_lectura)


nmc_tap_cont_lectura %>% 
  write.csv(here("data", "tidy", "nmc_tap_cont_lectura.csv"))

int_mod_tap_lectura %>% 
  write_rds(here("data", "models", "int_mod_tap_lectura.rds"))


### image_df_tidy taps 

### Include this - most taps were realized as apporximates or with 1 
### occlusion 

image_df %>% 
  filter(is_trill == "tap") %>% 
  group_by(no_occlusions) %>% 
  summarize(n = n())

image_df_tidy_tap = image_df %>% 
  filter(is_trill == "tap") %>% 
  filter(no_occlusions == 1 | no_occlusions == "a") %>% 
  mutate(no_occlusions = replace(no_occlusions, no_occlusions == "a", 
                                 "approximate")) 


image_df_tidy_tap$time <- relevel(as.factor(image_df_tidy_tap$time), ref = "PRE")


null_mod_tap_image_df = glmer(as.factor(no_occlusions) ~ 1 + 
                                (time | participant), family = "binomial"(link = "logit"), 
                              data = image_df_tidy_tap)

time_mod_tap_image_df = glmer(as.factor(no_occlusions) ~ time + 
                                (time | participant), family = "binomial"(link = "logit"),
                              data = image_df_tidy_tap)

group_mod_tap_image_df = glmer(as.factor(no_occlusions) ~ time + level + 
                                 (time | participant), family = "binomial"(link = "logit"), 
                               data = image_df_tidy_tap)

#  convergence issue - using non int model
int_mod_tap_image_df = glmer(as.factor(no_occlusions) ~ time + level + 
                               time:level +
                               (time | participant), family = "binomial"(link = "logit"),
                             data = image_df_tidy_tap)

nmc_tap_cont_image_df = anova(null_mod_tap_image_df, time_mod_tap_image_df, 
                              group_mod_tap_image_df)


nmc_tap_cont_image_df %>% 
  write.csv(here("data", "tidy", "nmc_tap_cont_image_df.csv"))

group_mod_tap_image_df %>% 
  write_rds(here("data", "models", "int_mod_tap_image_df.rds"))



