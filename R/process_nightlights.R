# PROJECT: Deforestation and Biodiversity
# PURPOSE: Process Nightlight Data 
# DATA SOURCE: https://eogdata.mines.edu/products/vnl/
# AUTHOR: Raahil Madhok
# NOTES: units in nanoWatts/cm2/sr

# ----------- SET-UP -----------------------------------------------------
# Directories
rm(list=ls())
READ.DIR <- '/Volumes/Backup Plus 1/research/data/def_biodiv/nightlights/'
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

# Convert raw tiles to cropped tifs (one run - 1.5 hrs)
{
raw_files <- list.files(paste(READ.DIR, 'raw', sep=''), full.names=T)
crop_slice <- function(i) {
  
  # read
  r <- raster(raw_files[[i]])
  
  # date
  date <- paste(substr(names(r), 11,14), substr(names(r), 15,16), sep='-')
  print(paste('cropping and saving viirs for date: ', date, sep=''))
  
  # Crop
  crs(r) <- crs(india_dist)
  r <- crop(r, india_dist)
  
  # Save
  writeRaster(r, paste(READ.DIR, 'processed/india-viirs-', date, '.tif', sep=''), format='GTiff', overwrite=T)
  
}
idx <- 1:length(raw_files)
lapply(idx, crop_slice)
}

# Stack Tifs (~500m resolution)
setwd(paste(READ.DIR, '/processed', sep=''))
tifs <- list.files()

# Function to extract mean
get_mean <- function(i) {
  
  # read raster
  r <- raster(tifs[i])
  
  # date
  date <- str_replace(substr(names(r), 13, 19), '\\.', '-')
  print(paste('processing: ', date, sep=''))
  
  # Extract statistics
  df <- cbind(st_drop_geometry(india_dist), exact_extract(r, india_dist, c('mean', 'sum')))
  df$date <- date
  
  return(df)
  
}
idx <- 1:length(tifs)
results <- rbindlist(lapply(idx, get_mean))

# Long Panel
df <- results %>%
  rename(yearmonth = date, 
         rad_mean = mean,
         rad_sum = sum) %>%
  arrange(c_code_2011, yearmonth)

# Save
setwd(SAVE.DIR)
write_csv(df, 'india_nightlights.csv')