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
require(rgdal)
require(sp)
require(splm)
require(plm)
require(spatialreg)
# ------------------------------------------------------------------------

# Read District Map
india_districts <- st_read(paste(dist_shp, "maps/india-district", sep=""),
                                              'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

# Read Data
data <- read.csv(paste(path_head,'/data/csv/fc_ebd_trip.csv', sep=''))
data <- select(data, s_richness_ihs, district_forest_cum_ihs, 
               district_nonforest_cum_ihs, coverage_ihs, tree_cover_mean_ihs,
               temperature_mean_ihs, precipitation_mean_ihs,
               user_dist, state_month, year)

#------------------------------------------------------------------------
# MODEL 1 : Spatial Lag with Binary Contiguity
#------------------------------------------------------------------------

# Demean on FEs
varlist <- c('s_richness_ihs', 'district_forest_cum_ihs', 'district_nonforest_cum_ihs', 'coverage_ihs', 'tree_cover_mean_ihs','temperature_mean_ihs', 'precipitation_mean_ihs')
data <- data %>%
  group_by(state_month) %>%
  mutate_at(varlist, list(sm=function(x) x-mean(x,na.rm=T))) %>%
  select(ends_with('_sm'), user_dist, year, state_month)

# Balance Panel
data <- make.pbalanced(data, index=c('user_dist', 'year_month'), balance.type=c('fill'))

# Neighbor Matrix
nb <- poly2nb(as(india_districts, 'Spatial'), queen = T, row.names=india_districts$c_code_11)
nb_mat <- nb2mat(nb, style='W', zero.policy=T)
W <- 


# Neighbor Matrix


nb_df <- as.data.frame(nb2mat(nb, style='W', zero.policy=T)) # Row-standardized weight matrix
names(nb_df) <- row.names(nb_df)
nb_df$c_code_2011 <- row.names(nb_df)
data <- merge(data, nb_df, 'c_code_2011')
W <- as.matrix(data[,28:ncol(data)])

# Spatial Lag

# Plot
#coords <- st_coordinates(st_centroid(st_geometry(india_districts)))
#plot(st_geometry(india_districts))
#plot(nb, coords, add=T)

# SLX
model <- s_richness_ihs_u_sy~district_forest_cum_ihs_u_sy+district_nonforest_cum_ihs_u_sy+coverage_ihs_u_sy+tree_cover_mean_ihs_u_sy
slx <- lmSLX(formula=model, data=data, listw=W, Durbin=~district_forest_cum_ihs_u_sy+district_nonforest_cum_ihs_u_sy, zero.policy=T)


