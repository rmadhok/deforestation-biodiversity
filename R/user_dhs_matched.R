#-------------------------------------------------------------
# PROJECT: Deforestation and Biodiversity
# PURPOSE: Match User homes to DHS villages
# AUTHOR: Raahil Madhok
#-------------------------------------------------------------

### SET-UP
# Directories
rm(list=ls())
READ <- '/Volumes/Backup Plus 1/research/data/'
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
home <- merge(distinct(read_csv('./def_biodiv/ebird/ebird_trip.csv'), user_id),
              read_csv(paste(SAVE, 'data/csv/user_home_impute.csv', sep='')),
              by='user_id')

# DHS clusters (n=28,526)
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
# Construct city shape (buffer+dissolve)
{
# Read urban polygons
city <- st_read('./def_biodiv/grump/global_urban_extent_polygons_v1.01.shp', 
                stringsAsFactors = F)
city <- st_make_valid(city) 
city <- st_crop(city, india_dist)

## 3km buffer (i.e. include metro areas)
# cities w boundaries < 3km apart are dissolved
city_buf <- st_as_sf(st_cast(st_union(st_buffer(city, 0.03)), 'POLYGON'))
}

# read buffered city polygon
city_buf <- st_read('./def_biodiv/grump/india_buffer/city_buffer.shp', 
                    stringsAsFactors = F)

# Match home to metro
home$ru_home <- st_join(st_as_sf(home, 
                                 coords=c('lon_home', 'lat_home'), 
                                 crs=4326),
                   city_buf, 
                   join = st_intersects)$FID

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
dhs_match <- st_join(st_as_sf(dhs, coords = c('lon', 'lat'), crs=4326), 
                     home_buf, 
                     join=st_intersects) %>% 
  dplyr::select(!c('lat', 'lon', 'c_code_2011_home')) %>%
  st_drop_geometry()

#-------------------------------------------------------------
# KEEP MATCHES WITH SIMILAR POP DENSITY
#-------------------------------------------------------------

# 5km buffer around home
home_pd <- merge(distinct(dhs_match, user_id), home, by='user_id') %>%
  dplyr::select(user_id, lon_home, lat_home)
home_pd <- st_buffer(st_as_sf(home_pd, 
                              coords = c('lon_home', 'lat_home'), 
                              crs = 4326), dist = 0.05)

# Population density around home
home_pd$home_pd <- exact_extract(pop, home_pd, 'mean')
home_pd <- st_drop_geometry(home_pd)
dhs_match <- left_join(dhs_match, home_pd, by = 'user_id')

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
dhs_match <- dhs_match %>%
  mutate(lower = 0.75*home_pd, upper = 1.25*home_pd,
         similar = ifelse(clust_pd >= lower & clust_pd <= upper, 1 ,0),
         keep = ifelse(similar == 0 | is.na(similar), 0, 1))

# Save
final <- dhs_match %>%
  dplyr::select(!c('lower', 'upper', 'home_pd', 'clust_pd'))

write_csv(final, paste(SAVE, 'data/csv/ebd_dhs_match.csv', sep=''))