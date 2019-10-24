# PROJECT: Deforestation and Biodiversity
# PURPOSE: Estimate Spatial Econometric Models
# AUTHOR: Raahil Madhok
# DATE: Oct 17 2019

# ----------- SET-UP -----------------------------------------------------
# Directories
path_head <- '/Users/rmadhok/Dropbox (Personal)/def_biodiv/'
dist_shp <- '/Users/rmadhok/Dropbox (Personal)/IndiaPowerPlant/data/'

# Load Packages
require(tidyverse)
require(sf)
require(spatialreg)
require(spdep)
# ------------------------------------------------------------------------

# Read District Map
india_districts <- st_read(paste(dist_shp, "maps/india-district", sep=""),
                           'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F) %>%
  rename(c_code_2011=c_code_11) %>% filter(!is.na(c_code_2011)) %>%
  arrange(c_code_2011)

# Read Data
data <- read_csv(paste(path_head,'/data/csv/fc_dist_ym.csv', sep='')) %>%
  select(matches('^dist_.*_cum_ihs$'), tree_cover_mean_ihs, 
         c_code_2011, year_month) %>%
  arrange(year_month, c_code_2011) %>%
  mutate(year_month=as.factor(year_month))

#------------------------------------------------------------------------
# MODEL 1 : Spatial Lag with Binary Contiguity
#------------------------------------------------------------------------

# Spatial Lags
# -------------------------
# 1. Construct contiguity matrix 
# 2. dynamically compute matrix product
# 3. bind to original data

# Binary Contiguity Matrix
nb <- poly2nb(as(india_districts, 'Spatial'), queen = T, row.names=india_districts$c_code_2011)
W <- nb2mat(nb, style='W', zero.policy=T) # row-standardized

slag_bc <- data.frame()
for(ym in levels(data$year_month)){
  
  print(paste('Computing contiguous spatial lags in:', ym))
  
  # Spatial lag matrix
  X <- as.matrix(data[data$year_month==ym,c(1:25)])
  WX <- W %*% X
  
  # Merge to data
  WX <- as.data.frame(WX)
  colnames(WX) <- paste(colnames(WX), "slx_bc", sep = "_")
  WX$year_month <- ym
  WX$c_code_2011 <- row.names(WX)
  slag_bc <- rbind(slag_bc,WX)
}

#------------------------------------------------------------------------
# MODEL 2 : Spatial Lag with Inverse Distance
#------------------------------------------------------------------------

# Spatial Lags
# -------------------------
# 1. Construct inverse distance matrix
# 2. dynamically compute matrix product
# 3. Stack 

# Extract centroids
coords <- do.call(rbind, st_centroid(st_geometry(india_districts))) %>% 
  as.data.frame() %>% setNames(c("lon","lat"))
row.names(coords) <- india_districts$c_code_2011

# Inverse Distance matrix
W <- as.matrix(dist(coords, method = "euclidean"))
W <- 1 / W
W[W > quantile(W,0.3)] <- 0 # cutoff at 80th percentile 
diag(W) <- 0

slag_inv <- data.frame()
for(ym in levels(data$year_month)){
  
  print(paste('Computing inverse distance spatial lags in:', ym))
  
  # Spatial lag matrix
  X <- as.matrix(data[data$year_month==ym,c(1:25)])
  WX <- W %*% X

  # Merge to data
  WX <- as.data.frame(WX)
  colnames(WX) <- paste(colnames(WX), "slx_i", sep = "_")
  WX$year_month <- ym
  WX$c_code_2011 <- row.names(WX)
  slag_inv <- rbind(slag_inv,WX)
}

slag <- merge(slag_bc, slag_inv, by=c('c_code_2011', 'year_month'))
write_csv(slag, paste(path_head, 'data/csv/slx.csv', sep=''))
