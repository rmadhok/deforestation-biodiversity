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
require(splitstackshape)

#### SET PROPORTION TO RANDOMLY SAMPLE
proportion <- .05
####

# Read Data
df <- fread('ebd_IN_200001_201810_relOct-2018.txt')

# Keep 2014-2018
df$YEAR <- as.numeric(strftime(df[['OBSERVATION DATE']], format = "%Y"))
df$YEARMONTH <- strftime(df[['OBSERVATION DATE']], format = "%Y-%m")
df <- df[df$YEAR > 2013, ]

# Load india 2011 districts
india.districts.2011 <- readOGR(paste(dist_shpf, "maps/india-district", sep=""), 
                                'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

### Identify District Census Code

# Overlay eBird on District Polygons
coords <- df[, 27:26]
proj <- proj4string(india.districts.2011)
df$c_code_2011 <- over(SpatialPoints(coords, proj4string=CRS(proj)), india.districts.2011)$c_code_11
df <- df[!is.na(df$c_code_2011),] 

# Sample from each district-year
set.seed(12345)
df.sample <- stratified(df, c("c_code_2011", "YEARMONTH"), proportion)

# Drop unnecessary columns
df.sample <- df.sample[,-c(1,2,10,11,14,16,18,19,20,21,22,24,29,34,39,41,42,43,44,45,46,47)]

# Write
sample.prop <- proportion*100
write.csv(df.sample, 
          paste(save_path_head, 'csv/ebird_sample_', sample.prop, '.csv', sep=""),
          row.names = F)

