# PROJECT: Deforestation and Biodiversity
# PURPOSE: Prepare eBird for Analysis
# AUTHOR: Raahil Madhok

### SET-UP
# Directories
rm(list=ls())
DIR <- '/Users/rmadhok/Dropbox/def_biodiv/'
setwd(DIR)

# Load Packages
require(tidyverse)
require(stringdist)

# Read ebird species list
sp_list <- read_csv('./data/csv/species_list.csv') %>%
  mutate(scientific_name = tolower(scientific_name))

# Read IUCN list
iucn <- read_csv('./data/csv/birdlife_iucn.csv') %>%
  mutate(scientific_name = tolower(`Scientific name`),
         iucn = `2021 IUCN Red List category`) %>%
  dplyr::select(scientific_name, iucn)

# Manual merge
ebird_iucn <- left_join(sp_list, iucn, by='scientific_name')

# Export for manual NA filling
write_csv(filter(ebird_iucn, is.na(iucn)), './data/csv/ebird_iucn_fill.csv')


