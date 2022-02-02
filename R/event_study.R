#-------------------------------------------------------------
# PROJECT: Deforestation and Biodiversity
# PURPOSE: Event Studies
# AUTHOR: Raahil Madhok
#-------------------------------------------------------------

### SET-UP
# Directories
rm(list=ls())
READ <- '/Users/rmadhok/Dropbox/def_biodiv/data'
setwd(READ)

# Load Packages
require(tidyverse)
require(lubridate)
require(fixest)
require(did)
require(did2s)

#-------------------------------------------------------------
# Prep Data
#-------------------------------------------------------------

# read
df <- read_csv('./csv/fc_dym_s2_v02.csv') %>%
  select(ends_with('code_2011'), year_month, year, month,
         dist_f_cum_km2, tree_cover_km2) %>%
  arrange(c_code_2011, year_month) %>%
  group_by(c_code_2011) %>%
  mutate(time = row_number())

# Event
df <- df %>%
  group_by(c_code_2011) %>%
  mutate(cohort = min(ifelse(dist_f_cum_km2 != lag(dist_f_cum_km2), time, NA), na.rm=T),
         cohort = ifelse(cohort == Inf, 0, cohort),
         running = time - cohort,
         running = ifelse(cohort == 0, -99, running),
         treated = ifelse(cohort == 0, 0, 1),
         treatment = ifelse(running > 0 & treated == 1, 1, 0))

# Standard TWFE
summary(feols(log(tree_cover_km2) ~ treatment | state_code_2011^year + c_code_2011 + month, data = df))

# Naive
b = feols(log(tree_cover_km2) ~ i(running, treated, ref = c(-1,-99))| state_code_2011^year + c_code_2011 + month, data = df)
iplot(b)

# Sun-Abraham
s = feols(log(tree_cover_km2) ~ sunab(cohort, time)| time + c_code_2011, data = df)
iplot(s)
iplot(list(b,s,nb))


  