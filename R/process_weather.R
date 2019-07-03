# PROJECT: Deforestation and Biodiversity
# PURPOSE: Process Weather Data [Source: ERA-Interim]
# AUTHOR: Raahil Madhok
# DATE: Feb 2 2019 [created]

### SET-UP
# Directories
path_head <- '/Users/rmadhok/Documents/ubc/research/def_biodiv/data/'
dist_shpf <- '/Users/rmadhok/Dropbox (CID)/IndiaPowerPlant/data/'
setwd(path_head)

# Packages
require(ncdf4) 
require(raster)
require(rgdal) 
require(sp)
require(dplyr)
require(geosphere)

# Load india 2011 districts
india.districts.2011 <- readOGR(paste(dist_shpf, "maps/india-district", sep=""), 
                                'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

# Load District Data
dist.data <- india.districts.2011@data

### Process Weather
weather <- list('temperature', 'precipitation')

# Initiate Master dataframes
df.temperature <- data.frame()
df.precipitation <- data.frame()

# Populate Dataframes
for (i in weather) {
  
  for (year in 2014:2019) {
    
    print(paste('Processing ', i, ': ', year, sep=''))
    
    # Read NetCDF (0.125 x 0.125 degrees)
    stack <- stack(paste(path_head, "netcdf/", i, '_' , year, '.nc', sep=""))

    # Projection
    proj <- proj4string(india.districts.2011)
    proj4string(stack) <- proj  #sync coordinate systems of grid and original vector data
    
    # Crop Borders
    stack <- crop(stack, extent(india.districts.2011))
    stack <- mask(stack, india.districts.2011)
    
    months <- nlayers(stack)
    for (month in 1:months){
      
      # Dataframe
      df.month <- as.data.frame(stack[[month]], xy=TRUE)
      colnames(df.month)[3] <- 'mean'
      
      # Get District Codes
      proj <- proj4string(india.districts.2011)
      df.month$c_code_2011 <- over(SpatialPoints(df.month[,1:2], proj4string = CRS(proj)), india.districts.2011)$c_code_11
      df.month <- df.month[!is.na(df.month$c_code_2011),]
  
      # Aggregate
      df.month <- df.month %>% 
        group_by(c_code_2011) %>% 
        summarise(mean = mean(mean))
      
      # Timestamp
      mth <- sprintf("%02d", month)
      df.month$yearmonth <- paste(year, mth, sep='-')
      
      # Append
      if (i == 'temperature') {
       
         df.temperature <- rbind(df.temperature, df.month)
      
         }
      else{
        
        df.precipitation <- rbind(df.precipitation, df.month)
      }
      
    }
    
  }
  
}

# Convert Kelvin to Celsius
colnames(df.temperature)[2] <- 'temperature_mean'
df.temperature$temperature_mean <- df.temperature$temperature_mean - 273.15

# Save
write.csv(df.temperature, 
          paste(path_head, 'csv/india_temperature.csv', sep=""),
          row.names = F)

# Convert rainfall from meter to millimeters
colnames(df.precipitation)[2] <- 'precipitation_mean'
df.precipitation$precipitation_mean <- df.precipitation$precipitation_mean * 1000

# Save
write.csv(df.precipitation, 
          paste(path_head, 'csv/india_precipitation.csv', sep=""),
          row.names = F)

