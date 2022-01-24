# PROJECT: Deforestation and Biodiversity
# PURPOSE: Estimate spillover effects
# AUTHOR: Raahil Madhok

# ----------- SET-UP -----------------------------------------------------
# Directories
rm(list=ls())
READ <- '/Users/rmadhok/Dropbox/def_biodiv/data/csv/'
SHP <- '/Users/rmadhok/Dropbox/IndiaPowerPlant/data/'
setwd(READ)

# Load Packages
require(tidyverse)
require(sf)
require(spatialreg)
require(spdep)
require(units)
require(data.table)
# ------------------------------------------------------------------------

# Read District Map
india_dist <- st_read(paste(SHP, "maps/india-district", sep=""),
                           'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F) %>%
  select(c_code_11) %>%
  rename(c_code_2011=c_code_11) %>% 
  filter(!is.na(c_code_2011)) %>%
  arrange(c_code_2011)

# Read deforestation
data <- read_csv('fc_dym_s2_v02.csv') %>%
    select(matches('^dist_.*_cum_km2$'), c_code_2011, year_month) %>%
    arrange(year_month, c_code_2011) %>%
    mutate(year_month=as.factor(year_month))

#------------------------------------------------------------------------
# MODEL 1 : Binary Contiguity
#------------------------------------------------------------------------
# 1. Construct contiguity matrix 
# 2. dynamically compute matrix product
# 3. bind to original data

# Binary Contiguity Matrix
nb <- poly2nb(as(india_dist, 'Spatial'), queen = T, row.names=india_dist$c_code_2011)
W <- nb2mat(nb, style='W', zero.policy=T) # row-standardized

slag_bc <- data.frame()
for(ym in levels(data$year_month)){
  
  print(paste('Computing binary contiguity for: ', ym, sep=''))
  
  # Data matrix
  X <- as.matrix(data %>%
                   filter(year_month==ym) %>%
                   select(-c(c_code_2011, year_month)))
  WX <- W %*% X
  
  # Merge to data
  WX <- as.data.frame(WX)
  colnames(WX) <- paste(colnames(WX), "slx_bc", sep = "_")
  WX$year_month <- ym
  WX$c_code_2011 <- row.names(WX)
  slag_bc <- rbind(slag_bc,WX)
}

#------------------------------------------------------------------------
# MODEL 2 : Inverse Distance
#------------------------------------------------------------------------
# 1. Construct inverse distance matrix
# 2. dynamically compute matrix product
# 3. Stack 

# Extract district centroids
centroids <- as.data.frame(st_coordinates(st_centroid(india_dist$geometry))) %>% 
  rename(lon = X, lat = Y)
row.names(centroids) <- india_dist$c_code_2011

# Inverse Distance
inv_dist <- function(i){
  
  # distance cutoff
  W <- st_distance(st_as_sf(centroids, coords = c('lon', 'lat'), crs=4326)) %>% set_units(km)
  if(i == 0) {
    W <- 1/W
    diag(W) <- 0
  }else{
    W[W > set_units(i, km)] <- 0
    W[W > set_units(0, km)] <- 1/W[W > set_units(0, km)]
    diag(W) <- 0
  }

  slag_inv <- data.frame()
  for(ym in levels(data$year_month)){
  
    print(paste('Computing 1/d for:', ym, '; cutoff: ', i,' km'))
  
    # Spatial lag matrix
    X <- as.matrix(data %>%
                   filter(year_month==ym) %>%
                   select(-c(c_code_2011, year_month)))
    WX <- W %*% X

    # Merge to data
    WX <- as.data.frame(WX)
    colnames(WX) <- paste(colnames(WX), "slx_i", i, sep = "_")
    WX$year_month <- ym
    WX$c_code_2011 <- row.names(centroids)
    slag_inv <- rbind(slag_inv,WX)
  }
  return(slag_inv)
}
idx <- c(0, 100, 200, 500)
slag_inv <- lapply(idx, inv_dist) %>% 
  purrr::reduce(inner_join, by=c('c_code_2011', 'year_month'))

# Save
slag <- merge(slag_bc, slag_inv, by=c('c_code_2011', 'year_month'))
write_csv(slag, 'slx_v02.csv')

#------------------------------------------------------------------------
# MODEL 3: Inverse Distance Squared
#------------------------------------------------------------------------

# # Weight matrix
# W <- st_distance(st_as_sf(centroids, coords = c('lon', 'lat'), crs=4326)) %>% set_units(km)
# W <- 1/(W^2)
# diag(W) <- 0
# 
# slag_inv2 <- data.frame()
# for(ym in levels(data$year_month)){
#   
#   print(paste('Computing suqared inverse distance spatial lags in:', ym))
#   
#   # Spatial lag matrix
#   X <- as.matrix(data %>%
#                    filter(year_month==ym) %>%
#                    select(-c(c_code_2011, year_month)))
#   WX <- W %*% X
#   
#   # Merge to data
#   WX <- as.data.frame(WX)
#   colnames(WX) <- paste(colnames(WX), "slx_i2", sep = "_")
#   WX$year_month <- ym
#   WX$c_code_2011 <- row.names(centroids)
#   slag_inv2 <- rbind(slag_inv2,WX)
# }
# 
# slag <- merge(slag_bc, slag_inv, by=c('c_code_2011', 'year_month'))
# slag <- merge(slag, slag_inv2, by=c('c_code_2011', 'year_month'))
# write_csv(slag, 'slx.csv')
