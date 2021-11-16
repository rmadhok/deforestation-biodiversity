# PROJECT: Deforestation and Biodiversity
# PURPOSE: Functions for eBird Processing
# AUTHOR: Raahil Madhok

# Load Packages
require(sp)
require(raster)

spatial_coverage <- function(sp, grid) {
  
  # Function: spatial_coverage
  ## Inputs: 
  ### sp: an sp object of bird sighting coordinates
  ### grid: a raster of defined projection and extent
  
  ## Outputs:
  ## count_df: grid-cell level data frame with centroid coordinates, district codes, 
  ## cell bird counts and a dummy for whether a bird was seen in that cell
  
  # Birds per cell
  india_grid <- rasterize(sp, grid, fun = 'count')
  india_grid <- mask(india_grid, india_districts_shp) # Clip to border
  
  # Grid cell data frame
  count_df <- as.data.frame(india_grid, xy = TRUE)
  
  # Get cell centroids
  cell_centroids <- SpatialPoints(count_df[,1:2], proj4string=crs(grid))
  
  # Identify district code
  count_df$c_code_2011 <- over(cell_centroids, india_districts_shp)$c_code_11
  count_df <- count_df[!is.na(count_df$c_code_2011), ]
  
  # Bird-in-cell dummy
  count_df$bird_obs <- ifelse(is.na(count_df$layer), 0, 1)
  
  # Return Spatial Coverage
  return(count_df)
  
}