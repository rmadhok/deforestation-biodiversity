# PROJECT: Deforestation and Biodiversity
# PURPOSE: Functions for eBird Processing
# AUTHOR: Raahil Madhok
# DATE: Jan 26, 2019 [created]

# Load Packages
require(data.table)
require(rgdal)
require(rgeos)
require(sp)
require(raster)
require(splitstackshape)
require(dplyr)
require(geosphere)

spatial_coverage <- function(sp, grid) {
  
  # Function: spatial_coverage
  ## Inputs: 
  ### sp: a SpatialPoints object of bird sighting coordinates
  ### grid: a raster of defined projected and extent
  
  ## Outputs:
  ## count.df: grid-cell level data frame with centroid coordinates, district codes, 
  ##cell bird counts and a dummy for whether a bird was seen in that cell
  
  india.grid <- rasterize(sp, grid, fun='count')
  proj4string(india.grid) <- proj4string(india.districts.2011)
  
  # Clip to Shapefile Extent
  india.grid <- crop(india.grid, extent(india.districts.2011))
  india.grid <- mask(india.grid, india.districts.2011)
  
  # Get District Codes
  count.df <- as.data.frame(india.grid, xy=TRUE)
  cell.centroids <- SpatialPoints(count.df[,1:2], proj4string=CRS(proj))
  count.df$c_code_2011 <- over(cell.centroids, india.districts.2011)$c_code_11
  count.df <- count.df[!is.na(count.df$c_code_2011),]
  
  # Spatial Coverage
  count.df$bird.obs <- ifelse(is.na(count.df$layer), 0, 1)
  
  # Return Spatial Coverage
  return(count.df)
  
}