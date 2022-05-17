# 05 load data 

library(here)
library(readr)

srt <- read.csv(here("data", "tidy", "srt_data.csv"))
para <- read.csv(here("data", "tidy", "para_data.csv"))
lectura <- read.csv(here("data", "tidy", "lectura_tidy.csv"))
image_df <- read.csv(here("data", "tidy", "image_df.csv"))

# load nmc 
nmc_srt_tap = read.csv(here("data", "tidy", "nmc_srt_tap.csv"))
nmc_srt_trill = read.csv(here("data", "tidy", "nmc_srt_trill.csv"))
nmc_lectura_tap = read.csv(here("data", "tidy", "nmc_lectura_tap.csv"))
nmc_lectura_trill = read.csv(here("data", "tidy", "nmc_lectura_trill.csv"))
nmc_image_df_tap = read.csv(here("data", "tidy", "nmc_image_df_tap.csv"))
nmc_image_df_trill = read.csv(here("data", "tidy", "nmc_image_df_trill.csv"))

# nmc trill
nmc_trill_cont = read.csv(here("data", "tidy", "nmc_trill_cont.csv"))
nmc_trill_cont_lec = read.csv(here("data", "tidy", "nmc_trill_cont_lec.csv"))
nmc_trill_cont_pct = read.csv(here("data", "tidy", "nmc_pct_cont_trill.csv"))

# nmc tap 
nmc_tap_cont = read.csv(here("data", "tidy", "nmc_tap_cont.csv"))
nmc_tap_cont_lectura = read.csv(here("data", "tidy", "nmc_tap_cont_lectura.csv"))
nmc_tap_cont_pct = read.csv(here("data", "tidy", "nmc_tap_cont_image_df.csv"))



# load continuous tap/trill models 
srt_tap_mod = read_rds(here("data", "models", "srt_tap.rds"))
srt_trill_mod = read_rds(here("data", "models", "srt_trill.rds"))
lectura_tap_mod = read_rds(here("data", "models", "lectura_tap.rds"))
lectura_trill_mod = read_rds(here("data", "models", "lectura_trill.rds"))
image_df_tap_mod = read_rds(here("data", "models", "image_df_tap.rds"))
image_df_trill_mod = read_rds(here("data", "models", "image_df_trill.rds"))

# load occlusion trill models 
srt_trill_cat = read_rds(here("data", "models", "srt_trill_cont.rds"))
lectura_trill_cat = read_rds(here("data", "models", "int_mod_trill_cont_lec.rds"))
pct_trill_cat = read_rds(here("data", "models", "int_mod_trill_pct_cont.rds"))

# load occlusion tap models 
srt_tap_cat = read_rds(here("data", "models", "srt_tap_cont.rds"))
lectura_tap_cat = read_rds(here("data", "models", "int_mod_tap_lectura.rds"))
pct_tap_cat = read_rds(here("data", "models", "int_mod_tap_image_df.rds"))



# load pc models