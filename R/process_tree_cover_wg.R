# PROJECT: Deforestation and Biodiversity
# PURPOSE: Process Tree Cover
# AUTHOR: Raahil Madhok
# DATE: Oct 4 2019

# ----------- SET-UP -----------------------------------------------------
# Directories
path_head <- '/home/rmadhok/projects/def-sgulati/rmadhok/def_biodiv/data'

# Load Packages
require(tidyverse)
require(sf)
require(raster)
require(rgdal)
# ------------------------------------------------------------------------

# Load district map
india_districts <- st_read(paste(path_head, "/india-district", sep=""),
                               'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

# Initiate master
df <- data.frame()

# Process TIFFs
setwd(paste(path_head, '/tree_cover', sep=''))
tif_list <- list.files()

for(tif in tif_list) {
 
  print(paste('Processing: ', tif, sep=''))
    
  # read raster
  r <- raster(tif)

  # Convert to df
  r_df <- as.data.frame(r, xy=T)
  names(r_df) <- c('lon', 'lat', 'tree_cover')
  
  print('Overlaying district code...')
  
  # Overlay district code 
  r_df$c_code_2011 <- as.data.frame(st_join(st_as_sf(r_df, 
                                                     coords = c('lon', 'lat'), 
                                                     crs = 4326), 
                                            india_districts, join = st_intersects))$c_code_11
  
  print('Completed district overlay...')
  
  # Crop
  r_df <- filter(r_df, !is.na(c_code_2011))
      
  # Aggregate
  r_df <- r_df %>%
    mutate(tree_cover=replace(tree_cover,tree_cover>100,NA)) %>%
    filter(!is.na(tree_cover)) %>%
    mutate(tree_cover_log = log(tree_cover+1), tree_cover_ihs=sinh(tree_cover)) %>%
    group_by(c_code_2011) %>%
    summarize(tree_cover_mean = mean(tree_cover),
              tree_cover_ihs = sum(tree_cover_ihs),
              tree_cover_log = sum(tree_cover_log))
    
  r_df$year <- substr(tif, 34,37)

  # Append
  df <- rbind(df, r_df)
}

# Write
write.csv(df, 
          paste(path_head, '/forest_cover.csv', sep=""),
          row.names = F)