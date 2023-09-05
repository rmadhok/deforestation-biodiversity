# PROJECT: Deforestation and Biodiversity
# PURPOSE: Process Tree Cover
# AUTHOR: Raahil Madhok

# ----------- SET-UP -----------------------------------------------------
# Directories
rm(list=ls())
READ.DIR <- '/Volumes/Backup Plus 1/research/data/def_biodiv/forest_cover/'
SAVE.DIR <- '/Users/rmadhok/Dropbox/def_biodiv/data/csv/'
SHP <- '/Users/rmadhok/Dropbox/IndiaPowerPlant/data/'
setwd(READ.DIR)

# Load Packages
require(tidyverse)
require(sf)
require(raster)
require(exactextractr)
# ------------------------------------------------------------------------

#---------------------------------------------
# PROCESS RASTERS
#---------------------------------------------

# Load district map
india_dist <- st_read(paste(SHP, "maps/india-district", sep=""),
                      'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F) %>%
  dplyr::select(c_code_11) %>%
  rename(c_code_2011=c_code_11)

# Stack Rasters
tifs <- list.files()
rstack <- stack(tifs)

# Remove non-tree cells
for(i in 1:nlayers(rstack)){
  print(paste('layer: ', i, sep=''))
  rstack[[i]][rstack[[i]] > 100] <- NA
}

# layer names
names(rstack) <- c('tree_cover_2014', 'tree_cover_2015', 'tree_cover_2016', 
                   'tree_cover_2017', 'tree_cover_2018', 'tree_cover_2019',
                   'tree_cover_2020')

# Resample to lower res (2km x 2km)
# Raw: 250m x 250m
#r <- raster::aggregate(rstack, fact = 10, fun=mean, na.rm=T)

#---------------------------------------------
# EXTRACT STATISTICS OVER DISTRICT BORDERS
#---------------------------------------------

# Mean forest cover in district 
# weighted by cell coverage fraction
df1 <- cbind(st_drop_geometry(india_dist), exact_extract(rstack, india_dist, 'mean'))

# Long Panel
df1_long <- df1 %>%
  pivot_longer(cols = contains('tree'),
               names_to = 'year',
               values_to = 'tree_cover_pct',
               names_prefix = 'mean.tree_cover_')

# Total forest cover in district 
# cell value x cell area x cell coverage fraction
a <- area(rstack)
rstack <- rstack / 100
df2 <- cbind(st_drop_geometry(india_dist), 
             exact_extract(rstack, india_dist, 
                           fun='weighted_sum', weights = a))

# Long Panel
df2_long <- df2 %>%
  pivot_longer(cols = contains('tree'),
               names_to = 'year',
               values_to = 'tree_cover_km2',
               names_prefix = 'weighted_sum.tree_cover_')

# Final panel
df_long <- merge(df1_long, df2_long, by=c('c_code_2011','year'))

# Write
setwd(SAVE.DIR)
write_csv(df_long, 'forest_cover.csv')
