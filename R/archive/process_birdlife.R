# PROJECT: Deforestation and Biodiversity
# PURPOSE: Extract India Birdlife Data
# AUTHOR: Raahil Madhok
# DATE: April 2019 [created]

### SET-UP
# Directories
read_path_head <- '/Volumes/FreeAgent GoFlex Drive/Data/def_biodiv/birdlife/'
save_path_head <- '/Users/rmadhok/Documents/ubc/research/def_biodiv/data/'
dist_shpf <- '/Users/rmadhok/Dropbox (CID)/IndiaPowerPlant/data/'
setwd(read_path_head)

### Load Packages
require(rgdal)
require(rgeos)
require(raster)
require(geosphere)
#require(sf)
require(sp)

### Load india 2011 districts
india.districts.2011 <- readOGR(paste(dist_shpf, "maps/india-district", sep=""), 
                                'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

### Load BirdLife File Geodatabse
fgdb <- paste(read_path_head, 'BOTW/BOTW.gdb', sep='')

# List Layer Names
subset(ogrDrivers(), grepl("GDB", name))
layers <- ogrListLayers(fgdb)
print(layers)

# Read Birdlife File Geodatabse
bl.world <- readOGR(dsn = fgdb, layer = "All_Species", stringsAsFactors = F)
proj4string(bl.world) <- proj4string(india.districts.2011) # Sync projections

# Crop India
bl.india <- crop(bl.world, extent(india.districts.2011))
rm(list='bl.world') # Save space

# Export Shapefile
writeOGR(obj = bl.india, 
         dsn = paste(save_path_head, 'gis/birdlife', sep = ''),
         layer = 'india-bird-range',
         driver = 'ESRI Shapefile')

