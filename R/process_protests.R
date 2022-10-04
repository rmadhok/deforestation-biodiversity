# PROJECT: Deforestation and Biodiversity
# PURPOSE: Attach district codes to protest locations
# AUTHOR: Raahil Madhok

### SET-UP
# Directories
rm(list=ls())
DATA <- '/Users/rmadhok/Dropbox/def_biodiv/'
SHP <- '/Users/rmadhok/Dropbox/IndiaPowerPlant/data/'
setwd(DATA)

# Load packages
require(tidyverse)
require(sf)

#-----------------------------------------------
# Process
#-----------------------------------------------

# Read Protests
df <- read_csv('./data/raw/india_protests.csv')

# Read district map
india_dist <- st_read(paste(SHP, "maps/india-district", sep=""), 
                      'SDE_DATA_IN_F7DSTRBND_2011') %>%
  dplyr::select(c_code_11) %>% 
  rename(c_code_2011=c_code_11)

# Get district code of protest
df$c_code_2011 <- st_join(st_as_sf(df, 
                                   coords = c('longitude', 'latitude'), 
                                   crs = 4326), 
                             india_dist, 
                             join = st_intersects)$c_code_2011

# clean
df_clean <- df %>%
  select(event_date, year, event_type, sub_event_type, 
         contains('actor'), contains('inter'),
         source, notes, c_code_2011)

# Save
write_csv(df_clean, './data/csv/india_protests_code.csv')