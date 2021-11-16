# PROJECT: eBird
# PURPOSE: Biome clusters
# DATA SOURCE: http://worldmap.harvard.edu/data/geonode:wwf_terr_ecos_oRn
# AUTHOR: Raahil Madhok

# Directories
rm(list=ls())
READ.DIR <- '/Volumes/Backup Plus/research/def_biodiv/biomes/wwf_terr_ecos_oRn'
SAVE.DIR <- '/Users/rmadhok/Dropbox/def_biodiv/data'
SHP <- '/Users/rmadhok/Dropbox/IndiaPowerPlant/data/maps/district2011'
setwd(READ.DIR)

# Load Packages
require(tidyverse)
require(sf)
require(sp)
require(rgdal)
require(raster)
#---------------------------------------------
# PROCESS CLUSTERS
#---------------------------------------------

# Read maps
ecoz <- st_read(READ.DIR, 'wwf_terr_ecos_oRn', stringsAsFactors = F)
dist <- st_read(SHP, 'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F) %>%
  mutate(dist_area = st_area(geometry)/1000000)

# Crop biome
ecoz <- st_crop(ecoz, dist)

# Union biomes
biome <- ecoz %>%
  group_by(BIOME) %>%
  summarize(geometry = st_union(geometry))

# Area of overlap b/w district and biome
int <- st_intersection(dist[, c('c_code_11', 'dist_area')], biome)

# Proportion of district covered by each biome
int_cover <- int %>%
   mutate(int_area = st_area(geometry)/1000000,
         int_pct = int_area/dist_area) %>%
  st_drop_geometry()

# Biome most covered by each district
clust <- int_cover %>%
  group_by(c_code_11) %>% 
  slice(which.max(int_pct)) %>%
  rename(c_code_2011 = c_code_11, 
         biome = BIOME)

# Save
write_csv(clust[, c('c_code_2011', 'biome')],
          paste(SAVE.DIR, '/csv/biome_cluster.csv', sep=''))
