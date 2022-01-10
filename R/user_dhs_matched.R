#-------------------------------------------------------------
# PROJECT: Deforestation and Biodiversity
# PURPOSE: Match User homes to DHS villages
# AUTHOR: Raahil Madhok
#-------------------------------------------------------------

### SET-UP
# Directories
rm(list=ls())
READ <- '/Volumes/Backup Plus/research/data/'
SAVE <- '/Users/rmadhok/Dropbox/def_biodiv/'
SHP <- '/Users/rmadhok/Dropbox/IndiaPowerPlant/data/'
setwd(READ)

# Load Packages
require(tidyverse)
require(sf)
require(raster)
require(exactextractr)

#-------------------------------------------------------------
# LOAD DATA
#-------------------------------------------------------------

# Districts
india_dist <- st_read(paste(SHP, "maps/district2011", sep=""), 
                      'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

# Population density
pop <- raster('./def_biodiv/worldpop/ppp_2015_1km_Aggregated.tiff')
pop <- crop(pop, extent(india_dist))
pop <- mask(pop, india_dist)

# User homes
home <- merge(distinct(read_csv('./def_biodiv/ebird/ebird_trip.csv'), OBSERVER.ID),
              read_csv('./ebird_wtp/user_home_impute.csv'),
              by.x = 'OBSERVER.ID', by.y = 'observer_id')

# DHS clusters
dhs <- st_read('./IndiaPowerPlant/data/nfhs/spatial/IAGE71FL/IAGE71FL.shp', 
               stringsAsFactors=F)

# Clean dhs (n=28,395 villages)
colnames(dhs) <- tolower(colnames(dhs))
dhs <- dhs %>%
  dplyr::select(dhsid, dhsclust, adm1dhs, adm1name, dhsregco, 
                dhsregna, urban_rura, latnum, longnum) %>%
  rename(state_code = adm1dhs,
         state = adm1name,
         district_code = dhsregco,
         district_name = dhsregna,
         ru_dhs = urban_rura,
         lat = latnum,
         lon = longnum) %>%
  filter(lat!=0 & lon!=0)

#-------------------------------------------------------------
# CLASSIFY USERS AS R/U
#-------------------------------------------------------------

# Read urban polygons
city <- st_read('./def_biodiv/grump', 'global_urban_extent_polygons_v1.01', 
                stringsAsFactors = F)
city <- lwgeom::st_make_valid(city) 
city <- st_crop(city, india_dist)

## 3km buffer (i.e. include metro areas)
# cities w boundaries < 3km apart are dissolved
metro <- st_as_sf(st_cast(st_union(st_buffer(city, 0.03)), 'POLYGON'))
metro$metroid <- 1:nrow(metro)

# Match home to metro
home$ru_home <- st_join(st_as_sf(home, coords=c('lon_home', 'lat_home'),crs=4326),
                   metro, join = st_intersects)$metroid

# Classify rural/urban
home <- home %>% mutate(ru_home = ifelse(is.na(ru_home), 'R', 'U'))

#-------------------------------------------------------------
# IDENTIFY DHS CLUSTER MATCHES
#-------------------------------------------------------------

# 10km buffer around rural homes 
home_r_buf <- st_buffer(st_as_sf(filter(home, ru_home == 'R'), 
                                 coords = c('lon_home', 'lat_home'), 
                                 crs = 4326), dist = 0.1)

# 5km buffer around urban homes
home_u_buf <- st_buffer(st_as_sf(filter(home, ru_home == 'U'), 
                                 coords = c('lon_home', 'lat_home'), 
                                 crs = 4326), dist = 0.05)
home_buf <- rbind(home_r_buf, home_u_buf)
rm(list=c('home_u_buf', 'home_r_buf'))

# DHS clusters within buffer
dhs_match <- st_join(home_buf, st_as_sf(dhs, coords = c('lon', 'lat'), crs=4326), 
                     join=st_intersects) %>% 
  dplyr::select(!c('lat', 'lon', 'c_code_2011_home')) %>%
  st_drop_geometry()

# 3485 (20%) users without matches
#dhs_nomatch <- dhs_match %>%
# filter(is.na(dhsid)) %>% dplyr::select('OBSERVER.ID')
#dhs_match <- filter(dhs_match, !is.na(dhsid)) # drop users w/ no nearby clusters

#-------------------------------------------------------------
# KEEP MATCHES WITH SIMILAR POP DENSITY
#-------------------------------------------------------------

# 5km buffer around home
home_pd <- merge(distinct(dhs_match, OBSERVER.ID), home, by='OBSERVER.ID') %>%
  dplyr::select(OBSERVER.ID, lon_home, lat_home)
home_pd <- st_buffer(st_as_sf(home_pd, 
                              coords = c('lon_home', 'lat_home'), 
                              crs = 4326), dist = 0.05)

# Population density around home
home_pd$home_pd <- exact_extract(pop, home_pd, 'mean')
home_pd <- st_drop_geometry(home_pd)
dhs_match <- left_join(dhs_match, home_pd, by = 'OBSERVER.ID')

# 5km buffer around dhs cluster
clust_pd <- merge(distinct(dhs_match, dhsid), dhs, by='dhsid') %>%
  dplyr::select(dhsid, lon, lat)
clust_pd <- st_buffer(st_as_sf(clust_pd, 
                              coords = c('lon', 'lat'), 
                              crs = 4326), dist = 0.05)

# Population density around cluster
clust_pd$clust_pd <- exact_extract(pop, clust_pd, 'mean')
clust_pd <- st_drop_geometry(clust_pd)
dhs_match <- left_join(dhs_match, clust_pd, by = 'dhsid')

# Keep matches if pop densities w/n 25%
# mean pd of matched users = 7541 ppl/km2
# mean pd of unmatched users = 4310 ppl/km2
dhs_match <- dhs_match %>%
  mutate(lower = 0.75*home_pd, upper = 1.25*home_pd,
         similar = ifelse(clust_pd >= lower & clust_pd <= upper, 1 ,0),
         keep = ifelse(similar == 0 | is.na(similar), 0, 1))

# Save
final <- dhs_match %>%
  dplyr::select(!c('lower', 'upper', 'home_pd', 'clust_pd'))

write_csv(final, paste(SAVE, 'data/csv/ebd_dhs_match.csv', sep=''))

