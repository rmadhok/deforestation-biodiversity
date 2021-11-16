# PROJECT: Deforestation and Biodiversity
# PURPOSE: Process Temperature/Precipitation
# AUTHOR: Raahil Madhok

# ----------- SET-UP -----------------------------------------------------
# Directories
rm(list=ls())
READ.DIR <- '/Volumes/Backup Plus/research/data/def_biodiv/weather/era5_update/'
SAVE.DIR <- '/Users/rmadhok/Dropbox/def_biodiv/data/csv/'
SHP <- '/Users/rmadhok/Dropbox/IndiaPowerPlant/data/'

# Packages
require(ncdf4)
require(raster)
require(tidyverse)
require(sf)
require(exactextractr)
require(data.table)

# Set weather (t2m = temperature, tp=precipitation)
var <- 'tp'
setwd(READ.DIR)
# ------------------------------------------------------------------------

# read map
india_dist <- st_read(paste(SHP, 'maps/india-district', sep=''), 
                      'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F) %>%
  dplyr::select(c_code_11) %>%
  rename(c_code_2011 = c_code_11)

# Read netCDF
r <- stack('temp_era5.nc', varname = var)

# projection
crs(r) <- crs(india_dist)

# Extract monthly means
df <- cbind(st_drop_geometry(india_dist), exact_extract(r, india_dist, 'mean'))

# Long panel
df_long <- df %>%
  pivot_longer(cols=contains('mean'),
               names_to = 'yearmonth',
               values_to = 'mean_era',
               names_prefix = 'mean.X') %>%
  mutate(yearmonth = str_replace(substr(yearmonth,1,7),'\\.', '-'))

# Save
setwd(SAVE.DIR)
if(var == 't2m'){
  df_long <- df_long %>% rename(temp_era = mean_era)
  df_long$temp_era <- df_long$temp_era - 273.15 # celsius
  write_csv(df_long, 'india_temperature_era.csv')
}
if(var == 'tp'){
  df_long <- df_long %>% rename(rain_era = mean_era)
  df_long$rain_era <- df_long$rain_era * 1000 # mm/day 
  write_csv(df_long, 'india_rain_era.csv')
}


