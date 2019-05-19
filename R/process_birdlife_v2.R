# PROJECT: Deforestation and Biodiversity
# PURPOSE: Extract India Birdlife Data
# AUTHOR: Raahil Madhok, revised by Patrick Baylis
# DATE: April 2019 [created]

# Directories
read_path_head <- '/Volumes/FreeAgent GoFlex Drive/Data/def_biodiv/birdlife/'
save_path_head <- '/Users/rmadhok/Documents/ubc/research/def_biodiv/data/'
dist_shpf <- '/Users/rmadhok/Dropbox (CID)/IndiaPowerPlant/data/'
setwd(read_path_head)

### Load Packages
require(tidyverse)
require(sf)
require(lwgeom)

# Load entire geodatabase. Time consuming but not avoidable, unfortunately.
bl_world <- st_read(paste(read_path_head, "BOTW/BOTW.gdb", sep = ''),
                    layer = "All_Species",
                    query = "SELECT * from All_Species")

# Load india 2011 districts
india_districts <- st_read(paste(dist_shpf, "maps/india-district", sep=""), 
                           'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)
india_districts <- india_districts %>% st_transform(st_crs(bl_world)) # # Sync projections

# Crop a single row in shapefile
crop_by_slice <- function(i) {
  # i <- 15664
  print(i)
  
  # Catch errors from casting
  this_slice <- tryCatch({
    bl_world[i, ] %>% st_cast("MULTIPOLYGON")
  }, error = function(e) {
    message("Error occurred while casting ", i, ".")
    data.frame()
  })
  
  # Catch errors from cropping
  result = tryCatch({
    suppressWarnings(st_crop(this_slice, india_districts) %>% st_cast("MULTIPOLYGON"))
  }, error = function(e) {
    message("Error occurred while cropping, skipping ", i, ".")
    data.frame()
  })
  
  if (nrow(result) > 0) {
    return(result)
  } else {
    return(NULL)
  }
}

# idx <- c(9, 11314:11316, 15664:15670) # DEBUG
idx <- 1:nrow(bl_world)
result_list <- lapply(idx, crop_by_slice)

# Combine results
result <- sf::st_as_sf(data.table::rbindlist(result_list, fill = T))
result <- result %>% rename(geometry = Shape)

st_write(result, "botw_india/botw_india.shp", delete_dsn = T)

# Load cropped shapefile ----

botw_india <- st_read("botw_india/botw_india.shp")

# Plot a random sample of layers
p <- ggplot(botw_india %>% sample_n(100)) + 
  geom_sf(mapping = aes(colour = SCINAME), alpha = 0.5, fill = NA) +
  scale_colour_discrete(guide = FALSE) +
  theme_minimal()
ggsave("botw_india_sample.png", width = 6, height = 6)
