# PROJECT: Deforestation and Biodiversity
# PURPOSE: Process Forest Cover
# AUTHOR: Raahil Madhok
# DATE: Sept 27 2019

### SET-UP
# Directories
path_head <- '/home/rmadhok/projects/def-sgulati/rmadhok/def_biodiv/data'

# Load Packages
require(tidyverse)
require(sf)
require(raster)
require(rgdal)

# Load districts
india_districts <- st_read(paste(path_head, "/india-district", sep=""),
                               'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

# If file exists, read; else create empty data frame
forest_cover <- paste(path_head, '/forest_cover.csv', sep='')
if(file.exists(forest_cover)) {
  df <- read.csv(forest_cover, header=T)
} else {
  df <- data.frame()
}

# Process Tiffs
year_list <- list.files(paste(path_head, '/tree_cover', sep=''))
for(year in year_list) {
  
  # Go to year folder
  setwd(paste(path_head, '/tree_cover/', year, sep=''))
  
  # Get list of tifs
  tif_list <- list.files()
  
  # Crop tiff to india dataframe
  for(tif in tif_list) {
    
    print(paste('Processing: Year-', year, '-Tile-', tif, sep=''))
    
    # read raster
    r <- raster(tif)
  
    # Sinusoidal --> WGS84; Nearest-neighbor interpolation
    r <- projectRaster(r, crs=crs(india_districts), method='ngb')
  
    # Convert to df
    r_df <- as.data.frame(r, xy=T)
    names(r_df) <- c('lon', 'lat', 'tree_cover')
  
    # Overlay district code 
    r_df$c_code_2011 <- as.data.frame(st_join(st_as_sf(r_df, 
                                                     coords = c('lon', 'lat'), 
                                                     crs = 4326), 
                                            india_districts, join = st_intersects))$c_code_11
  
    # Crop
    r_df <- filter(r_df, !is.na(c_code_2011))
    
    # Append
    if(nrow(r_df)>0) {
      
      # Aggregate
      r_df <- r_df %>%
        mutate(tree_cover=replace(tree_cover,tree_cover>100,NA)) %>%
        filter(!is.na(tree_cover)) %>%
        group_by(c_code_2011) %>%
        summarize(tree_cover_mean = mean(tree_cover),
                  tree_cover_ihs = sum(sinh(tree_cover)))
      
      r_df$year <- year
      r_df$tile <- tif
      df <- rbind(df, r_df)
      
      # Write
      write.csv(df, 
                paste(path_head, '/forest_cover.csv', sep=""),
                row.names = F)
    } else {
      print('No overlap between tile and country border...')
      next
    }
  }
}
