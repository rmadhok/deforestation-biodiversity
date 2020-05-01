# PROJECT: Deforestation and Biodiversity
# PURPOSE: Process Nightlight Data 
# DATA SOURCE: https://www.ngdc.noaa.gov/eog/viirs/download_dnb_composites.html#NTL_2015
# AUTHOR: Raahil Madhok
# DATE: Apr 16 2019 [created]
# NOTES: units in nanoWatts/cm2/sr

### SET-UP
# Directories
read_path <- '/Volumes/FreeAgent GoFlex Drive/Data/def_biodiv/'
save_path <- '/Users/rmadhok/Documents/ubc/research/def_biodiv/data/'
dist_shpf <- '/Users/rmadhok/Dropbox (CID)/IndiaPowerPlant/data/'
setwd(read_path)

# Packages
require(raster)
require(rgdal) 
require(sp)
require(dplyr)

# Load india 2011 districts
india.districts.2011 <- readOGR(paste(dist_shpf, "maps/india-district", sep=""), 
                                'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

# Load TIFF list
file_list <- list.files('nightlights')

# Initiate Master DF
df.nightlights <- data.frame()

# Populate Dataframe
for (i in 1:length(file_list)){
  
  print(paste("Processing file: ", file_list[i]))
  
  # Read File
  r <- raster(paste(read_path, 'nightlights/', file_list[i], sep=''))

  # Sync Projection
  proj4string(r) <- proj4string(india.districts.2011)
  
  # Resample to higher resolution (4km x 4km)
  #-- Note: Data is in 15 arc-seconds = 0.004 deg
  r <- aggregate(r, fact = 10)
  
  # Crop to India Borders
  r <- crop(r, extent(india.districts.2011))
  r <- mask(r, india.districts.2011)
  
  # Raster to Dataframe
  df.month <- as.data.frame(r, xy=TRUE)
  colnames(df.month)[3] <- 'mean_radiance'
  
  # Overlay District
  proj <- proj4string(india.districts.2011)
  df.month$c_code_2011 <- over(SpatialPoints(df.month[,1:2], proj4string=CRS(proj)), india.districts.2011)$c_code_11
  df.month <- df.month[!is.na(df.month$c_code_2011),]
  
  # Aggregate to District
  df.month <- df.month %>% 
    group_by(c_code_2011) %>% 
    summarise(mean_lights = mean(mean_radiance, na.rm=T))
  
  # Timestamp
  year <- substr(file_list[i], 11, 14)
  month <- substr(file_list[i], 15, 16)
  df.month$yearmonth <- paste(year, month, sep='-')
  
  # Append to Master
  df.nightlights <- rbind(df.nightlights, df.month)

}

# Write
write.csv(df.nightlights, 
          paste(save_path, 'csv/india_nightlights.csv', sep=""),
          row.names = F)


