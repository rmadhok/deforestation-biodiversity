# PROJECT: Deforestation and Biodiversity
# PURPOSE: Process Rain Data [Source: GPM, Level 3, IMERG, 0.1 x 0.1 res]
# AUTHOR: Raahil Madhok
# DATE: Nov 13 2019 [created]
# Notes - Takes ~5 hours. 6 districts missing.

### SET-UP
# ----------- SET-UP -----------------------------------------------------
# Directories
read_dir <- '/Volumes/Backup Plus/research/def_biodiv/weather/rainfall_gpm'
save_dir <- '/Users/rmadhok/Dropbox (Personal)/def_biodiv/data/'
dist_shp <- '/Users/rmadhok/Dropbox (Personal)/IndiaPowerPlant/data/'
setwd(read_dir)

# Packages
require(ncdf4)
require(raster)
require(tidyverse)
require(sf)
require(rgdal) 
require(sp)
# ------------------------------------------------------------------------

# Load Districts
india_districts <- readOGR(paste(dist_shp, "maps/india-district", sep=""),
                           'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

# File List
files <- list.files()

# Master
df_rain <- data.frame()

# Loop through daily raster
for (file in files) {

  year <- substr(file, 22,25)
  month <- substr(file, 26,27)
  day <- substr(file, 28,29)
  
  print(paste('Processing raster: ', year, month, day, sep='-'))
  
  # Read as raster
  r <- raster(file)
  
  # Crop to India
  extent(r) <- extent(india_districts) # sync extents
  proj4string(r) <- proj4string(india_districts) # sync projection
  r <- crop(r, extent(india_districts))
  r <- mask(r, india_districts)
  
  # Get district of cell centroid (point-in-poly overlay)
  df_d <- as.data.frame(r, xy=T)
  df_d$c_code_2011 <- over(SpatialPoints(df_d[,1:2], proj4string = CRS(proj4string(india_districts))), india_districts)$c_code_11
  df_d <- df_d[!is.na(df_d$c_code_2011),]
  
  # Aggregate to district
  df_d <- df_d %>% 
    group_by(c_code_2011) %>% 
    summarize(rainfall_mm = mean(Daily.accumulated.precipitation..combined.microwave.IR..estimate, na.rm=T))
  
  # format dates
  df_d$date <- paste(year, month, day, sep='-')
  df_d$yearmonth <- paste(year, month, sep='-')
  
  # Append
  df_rain <- rbind(df_rain, df_d)
  
}

# Aggregate to district month
df_ym <- df_rain %>%
  group_by(c_code_2011, yearmonth) %>%
  summarize(rainfall_mm = mean(rainfall_mm, na.rm=T))

# Write
write.csv(df_ym,
          paste(save_dir, 'csv/india_rainfall_gpm.csv', sep=""),
          row.names = F)

