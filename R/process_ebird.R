# PROJECT: Deforestation and Biodiversity
# PURPOSE: Random Sample from eBird India (2010-2018)
# AUTHOR: Raahil Madhok
# DATE: Dec 26, 2018 [created]

### SET-UP
# Directories
read_path_head <- '/Volumes/FreeAgent GoFlex Drive/Data/def_biodiv/'
save_path_head <- '/Users/rmadhok/Documents/ubc/research/def_biodiv/data/'
dist_shpf <- '/Users/rmadhok/Dropbox (CID)/IndiaPowerPlant/data/'
setwd(read_path_head)

# Load Packages
require(data.table)
require(rgdal)
require(rgeos)
require(sp)
require(raster)
require(splitstackshape)

#### SET PROPORTION TO RANDOMLY SAMPLE
proportion <- .05
####

# Load india 2011 districts
india.districts.2011 <- readOGR(paste(dist_shpf, "maps/india-district", sep=""), 
                                'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

# Write Attribute Data
dist.data <- india.districts.2011@data
write.csv(dist.data,
          paste(save_path_head,'csv/2011_india_dist.csv',sep=''),
          row.names = F)

# Read eBird Data
df <- fread('ebd_IN_200001_201810_relOct-2018.txt')

# Keep 2014-2018
df$YEAR <- as.numeric(strftime(df[['OBSERVATION DATE']], format = "%Y"))
df$YEARMONTH <- strftime(df[['OBSERVATION DATE']], format = "%Y-%m")
df <- df[df$YEAR > 2013, ]

### Identify District Census Code
proj <- proj4string(india.districts.2011)
df$c_code_2011 <- over(SpatialPoints(df[, 27:26], proj4string=CRS(proj)), india.districts.2011)$c_code_11
df <- df[!is.na(df$c_code_2011),]

# Sample from each district-yearmonth
set.seed(12345)
df.sample <- stratified(df, c("c_code_2011", "YEARMONTH"), proportion)

# Drop unnecessary columns
df.sample <- df.sample[,-c(1,2,10,11,14,16,18,19,20,21,22,24,29,34,39,41,42,43,44,45,46,47)]
rm(list='df')

# Write
sample.prop <- proportion*100
write.csv(df.sample, 
          paste(save_path_head, 'csv/ebird_sample_', sample.prop, '.csv', sep=""),
          row.names = F)

### Spatial Coverage

# Initialize Grid
grid <- raster(extent(india.districts.2011))
res(grid) <- .1 # grid resolution in degrees lat-lon
proj4string(grid) <- proj4string(india.districts.2011) #sync coordinate systems of gridded district and original vector data

# Clip to Shapefile Extent
india.grid.crop <- crop(grid, extent(india.districts.2011))
india.grid <- mask(india.grid.crop, india.districts.2011)

# Compute Spatial Coverage
df.sample$YEARMONTH <- as.factor(df.sample$YEARMONTH)
bird.count <- data.frame()
for(ym in levels(df.sample$YEARMONTH)){
  print(ym)
  coords <- df.sample[df.sample$YEARMONTH == ym, 15:14]
  india.grid <- rasterize(coords, india.grid, fun='count')
  count.df <- as.data.frame(india.grid, xy=TRUE)
  count.df$bird.obs <- count.df[count.df$count > 0 & !is.na(count.df$count)]
  count.df$c_code_2011 <- over(SpatialPoints(count.df[,1:2], proj4string=CRS(proj)), india.districts.2011)$c_code_11
  row <- aggregate(count.df[,4:5], by=list(count.df$c_code_2011), FUN=mean)
  row$yearmonth <- ym
  bird.count <- rbind(bird.count, row)
}

names(bird.count) <- c('lon', 'lat', 'count', 'yearmonth')

# Get District Codes
bird.count$c_code_2011 <- over(SpatialPoints(bird.count[,1:2], proj4string=CRS(proj)), india.districts.2011)$c_code_11
bird.count <- bird.count[!is.na(bird.count$c_code_2011),]
bird.count$count[is.na(bird.count$count)] <- 0

# Write
grid.res <- res(grid)[1]*100
write.csv(bird.count, 
          paste(save_path_head, 'csv/count_grid_', grid.res,'km_sample_', sample.prop, '.csv', sep=""),
          row.names = F)