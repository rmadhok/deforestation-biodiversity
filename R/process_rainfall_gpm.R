# PROJECT: Deforestation and Biodiversity
# PURPOSE: Process Rain Data [Source: GPM, Level 3, IMERG, 0.1 x 0.1 res]
# AUTHOR: Raahil Madhok

# ----------- SET-UP -----------------------------------------------------
# Directories
rm(list=ls())
READ.DIR <- '/Volumes/Backup Plus/research/data/def_biodiv/weather/'
SAVE.DIR <- '/Users/rmadhok/Dropbox/def_biodiv/data/csv/'
SHP <- '/Users/rmadhok/Dropbox/IndiaPowerPlant/data/'

# Packages
require(raster)
require(tidyverse)
require(sf)
require(exactextractr)
require(data.table)
# ------------------------------------------------------------------------

# Load Districts
india_dist <- st_read(paste(SHP, "maps/india-district", sep=""),
                      'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F) %>%
  dplyr::select(c_code_11) %>%
  rename(c_code_2011=c_code_11)

# Convert raw netCDFs to geoTIFFS (one time run - 2 hours)
{
nc <- list.files(paste(READ.DIR, 'rain_gpm', sep=''), full.names=T)
crop_slice <- function(i){
  
  # read
  r <- raster(nc[i], varname='precipitationCal')
  print(paste('cropping and saving: ', i , ' as Tiff', sep=''))
 
   # date
  date <- str_replace_all(getZ(r), '-', '_')
  
  # projection
  crs(r) <- crs(india_dist)
  
  # transpose
  r <- t(r)
  
  # crop
  r <- crop(r, india_dist)
  
  # save
  writeRaster(r, paste(READ.DIR, 'rain_gpm_tif/india_gpm_', date, '.tif', sep=''), format='GTiff', overwrite=T)
}
idx <- 1:length(nc)
lapply(idx, crop_slice)
}

# Stack Tifs
setwd(paste(READ.DIR, './rain_gpm_tif', sep=''))
tifs <- list.files()

# Function to extract mean
get_mean <- function(i) {
  
  # read raster
  r <- raster(tifs[i])
  
  # date
  date <- str_replace_all(substr(names(r), 11,20), '_', '-')
  print(paste('processing: ', date, sep=''))
  
  # Extract mean
  df <- cbind(st_drop_geometry(india_dist), exact_extract(r, india_dist, 'mean'))
  names(df)[2] <- 'rain_gpm'
  df$date <- date
  
  return(df)
  
}
idx <- 1:length(tifs)
results <- rbindlist(lapply(idx, get_mean))

# Long panel
df <- results %>%
  mutate(month = sprintf("%02s", month(date)),
         year = year(date),
         yearmonth = paste(year, month, sep='-')) %>%
  group_by(c_code_2011, yearmonth) %>%
  summarize(rain_gpm = mean(rain_gpm, na.rm=T))

# Write
setwd(SAVE.DIR)
write_csv(df, 'india_rainfall_gpm_update2020.csv')
