#---------------------------------------------
# PROJECT: Deforestation and Biodiversity
# PURPOSE: Estimate different standard errors
# AUTHOR: Raahil Madhok
#---------------------------------------------
# Directories
rm(list=ls())
DIR <- '/Users/rmadhok/Dropbox/def_biodiv/'
setwd(DIR)

# Packages
require(tidyverse)
require(fixest)

#---------------------------------------------
# Load Data
#---------------------------------------------

# Read
df <- haven::read_dta('./data/dta/fc_ebd_udt_v02.dta')
df <- filter(df, year > 2014)
df <- filter(df, n_trips <= quantile(df$n_trips, probs = 0.99))
df <- df %>%
  rename(User=uid,
         Year=year,
         District=c_code_2011,
         State=state_code_2011,
         Month=month)

# Missing Flags
df <- df %>%
  mutate(ln_rad_sum_flag = ifelse(is.na(ln_rad_sum), 1, 0),
         ln_rad_sum = ifelse(ln_rad_sum_flag == 1, mean(ln_rad_sum, na.rm=T), ln_rad_sum))

# Add district centroids
coords <-  read_csv('./data/csv/dist_coords.csv') %>%
  rename(District = c_code_11)
df <- left_join(df, coords, by = 'District')

#---------------------------------------------
# Estimate Main Regression
#---------------------------------------------
df <- rename(df, Infrastructure=dist_f_cum_km2)
formula <- sr ~ Infrastructure + temp + rain + ln_duration + ln_distance + 
  ln_exp_idx + ln_coverage_udym + ln_group_size + traveling + ln_hour + ln_rad_sum + ln_rad_sum_flag | 
  User^Year + District + State^Month

main_spec1 <- feols(formula, cluster='biome', fixef.rm='singleton', data = df)
main_spec2 <- feols(formula, cluster = 'District', fixef.rm='singleton', data = df)
main_spec3 <- feols(formula, cluster = 'State', fixef.rm='singleton', data = df)
main_spec4 <- feols(formula, vcov = conley(100, distance = "spherical"), fixef.rm='singleton', data = df)
main_spec5 <- feols(formula, vcov = conley(200, distance = "spherical"), fixef.rm='singleton', data = df)
main_spec6 <- feols(formula, vcov = conley(600, distance = "spherical"), fixef.rm='singleton', data = df)
main_spec7 <- feols(formula, vcov = conley(1000, distance = "spherical"), fixef.rm='singleton', data = df)

signifCode <- list(`0.01` = "***",`0.05` = "**",`0.1` = "*",`1` = "")
etable(main_spec1, main_spec2, main_spec3, main_spec4, main_spec5, main_spec6, main_spec7,
       keep='Infrastructure', digits = 'r3', depvar=F, digits.stats='r3',
       style.df = style.df(depvar.title='', fixef.title=''),
       headers=list(':_:' = list('Standard Error Boundary' = 3, 'Conley Spatial Error Cutoff' = 4),
                    ':_:' = list('Biome'=1, 'District'=1, 'State'=1, '100km'=1, '200km'=1, '600km'=1, '1000km'=1)),
       signif.code = c("***"=0.01, "**"=0.05, "*"=0.10), tex=T, 
       file='./docs/jmp/tex_doc/v5/tables/robustness_se.tex', replace = T)





