# PROJECT: Deforestation and Biodiversity
# PURPOSE: Prepare eBird for Analysis
# AUTHOR: Raahil Madhok

### SET-UP
# Directories
rm(list=ls())
READ <- '/Volumes/Backup Plus 1/research/data/def_biodiv/ebird'
SAVE <- '/Users/rmadhok/Dropbox/def_biodiv/'
SHP <- '/Users/rmadhok/Dropbox/IndiaPowerPlant/data/'
setwd(READ)

# Load Packages
require(data.table)
require(tidyverse)
require(sf)
require(raster)
require(lubridate)
require(stargazer)
require(exactextractr)
require(stringdist)
require(fastDummies)

# Load Maps
india_dist <- st_read(paste(SHP, "maps/india-district", sep=""), 
                      'SDE_DATA_IN_F7DSTRBND_2011') %>%
  dplyr::select(c_code_11) %>% 
  rename(c_code_2011=c_code_11)

#----------------------------------------------------------------
## 1. Load eBird and clean variable names
#----------------------------------------------------------------

# Read (n=21,870,724)
ebird <- fread('ebd_IN_201401_202012_relSep-2021.txt') # 2014-2020
colnames(ebird) <- gsub('\\.', '_', tolower(make.names(colnames(ebird))))

# Format Dates
ebird$observation_date <- ymd(ebird$observation_date)
ebird$year <- year(ebird$observation_date)
ebird$yearmonth <- format(ebird$observation_date, "%Y-%m")
ebird$hour <- as.numeric(substr(ebird$time_observations_started, 1, 2))

# experience since 2014 (n=134,433)
experience <- ebird %>%
  group_by(observer_id, observation_date) %>%
  summarize(trips_day = n_distinct(sampling_event_identifier),
            yearmonth = first(yearmonth)) %>%
  group_by(observer_id) %>%
  arrange(observation_date) %>%
  mutate(exp_idx = cumsum(trips_day)) %>%
  group_by(observer_id, yearmonth) %>%
  arrange(observation_date) %>%
  summarize(exp_idx = last(exp_idx))

# Keep necessary columns
ebird <- ebird %>%
  dplyr::select('taxonomic_order', 'observation_count', 'state', 'county', 
                'locality', 'locality_type', 'latitude', 'longitude', 
                'observation_date', 'observer_id', 'sampling_event_identifier', 
                'protocol_type', 'protocol_code', 'duration_minutes', 'hour', 
                'effort_distance_km', 'number_observers', 'all_species_reported', 
                'group_identifier', 'year', 'yearmonth', 'common_name', 'scientific_name')

# Clean names
ebird <- ebird %>%
  rename(species = common_name,
         species_id = taxonomic_order,
         species_count = observation_count,
         lat = latitude,
         lon = longitude,
         date = observation_date,
         user_id = observer_id,
         trip_id = sampling_event_identifier,
         duration = duration_minutes,
         distance = effort_distance_km,
         group_size = number_observers,
         complete = all_species_reported,
         group_id = group_identifier)

#-----------------------------------------------------
## Appendix:  Homes Coordinates
#-----------------------------------------------------
# Note: compute home on *all* trips (before filtering)
# because filtering will affect gravitational center
{

# Real Home (n=210 users)
user_home <- ebird %>%
  filter(str_detect(str_to_lower(locality), 
                    'my home|my house|my balcony|
                    my terrace|my backyard|my street|
                    my yard|my veranda')) %>%
  group_by(user_id) %>%
  summarize(lon_home_real = mean(lon, na.rm=T),
            lat_home_real = mean(lat, na.rm=T))

# Save
write_csv(user_home, paste(SAVE, 'data/csv/user_home_real.csv', sep=''))

# Imputed Home --------------------------------------
# 1. Estimate gravitational center of all trips
# 2. Compute distance from "home" to each trip
# 3. Remove outliers (e.g. faraway trips - by plane)
# 4. Recompute "home" from remaining trips

# Center of user's trips
user <- distinct(ebird, user_id, lon, lat) %>%
  group_by(user_id) %>%
  mutate(lon_home = mean(lon, na.rm=T),
         lat_home = mean(lat, na.rm=T)) %>% 
  ungroup()

# straight-line distance from center to each site
user$distance <- st_distance(st_as_sf(user, 
                                      coords = c('lon_home', 'lat_home'),
                                      crs=4326), 
                             st_as_sf(user, 
                                      coords = c('lon', 'lat'),
                                      crs=4326), 
                             by_element = T)
user$distance <- as.numeric(user$distance)/1000 #km

# Remove outlier trips
user <- user %>%
  group_by(user_id) %>%
  filter(distance <= quantile(distance, 0.75) + 1.5*IQR(distance))

# Recompute home 
user <- user %>%
  group_by(user_id) %>%
  summarize(lon_home=mean(lon, na.rm=T),
            lat_home=mean(lat, na.rm=T))

# Overlay home districts
# -------------------------------------
# Note: If gravit. center is off coast
# home is the centroid of nearest dist
#--------------------------------------
user$c_code_2011_home <- st_join(st_as_sf(user, 
                                          coords=c('lon_home', 'lat_home'), 
                                          crs=4326), 
                                 india_dist, 
                                 join = st_intersects)$c_code_2011

# Handle Out-of-bounds homes
# For off-coast homes, assign id of nearest district
user_na <- filter(user, is.na(c_code_2011_home))
user_na$c_code_2011_home <- st_join(st_as_sf(user_na, 
                                             coords=c('lon_home', 'lat_home'), 
                                             crs=4326), 
                                    india_dist, 
                                    join = st_nearest_feature)$c_code_2011

# District centroids
centroids <- as.data.frame(st_coordinates(st_centroid(india_dist$geometry))) %>%
  rename(lon_home = X, lat_home = Y)
centroids$c_code_2011_home <- india_dist$c_code_2011
user_na <- merge(user_na[, c('user_id', 'c_code_2011_home')], 
                 centroids, by='c_code_2011_home')

# Bind to main user list 
user <- rbind(filter(user, !is.na(c_code_2011_home)), user_na)
rm(list=c('centroids', 'user_na'))

# Save user list
write_csv(user, paste(SAVE, 'data/csv/user_home_impute.csv', sep=''))
}
#----------------------------------------------------------------
## 2. FILTERING 
#----------------------------------------------------------------

# Filters from eBird manual (n=18,614,262)
ebird <- ebird %>%
  filter(
    complete == 1,
    duration <= 5*60,
    group_size <= 10 
  ) %>%
  dplyr::select(-complete)

# Export protocol list (appendix)
protocol <- ebird %>%
  distinct(trip_id, .keep_all = T) %>%
  group_by(protocol_type) %>%
  summarize(`Num. Trips` = n()) %>%
  mutate(Pct. = round((`Num. Trips`/sum(`Num. Trips`)*100),2)) %>%
  rename(Protocol = protocol_type) %>%
  arrange(desc(Pct.))
stargazer(protocol, summary=F, rownames=F, float=F,
          out = paste(SAVE,'docs/jmp/tex_doc/v3/tables/protocol.tex', sep=''))

# Filter Protocol (stationary, travelling) (n=18,458,042)
ebird <- filter(ebird, protocol_code %in% c('P21', 'P22'))

#----------------------------------------------------------------
## 3. IDENTIFY DISTRICTS AND COASTAL TRIPS
#----------------------------------------------------------------  

# Overlay (point-in-poly) 
ebird$c_code_2011 <- st_join(st_as_sf(ebird, 
                                      coords = c('lon', 'lat'), 
                                      crs = 4326), 
                             india_dist, 
                             join = st_intersects)$c_code_2011

# Remove out-of-bounds birds (n = 18,390,933)
ebird_oob <- ebird %>% filter(is.na(c_code_2011)) # (n=5101 trips, 68 districts)
ebird <- ebird %>% filter(!is.na(c_code_2011))
rm(list='ebird_oob')

#----------------------------------------------------------------
## 3. CATEGORIZE SPECIES (IUCN)
#---------------------------------------------------------------- 
setwd(SAVE)

# species list
sp_list <- ebird %>% 
  ungroup() %>% 
  distinct(species, .keep_all=T) %>%
  dplyr::select(species, scientific_name) %>%
  mutate(scientific_name = tolower(scientific_name))
write_csv(sp_list, './data/csv/species_list.csv')

# Read IUCN list
iucn <- read_csv('./data/csv/birdlife_iucn.csv') %>%
  mutate(scientific_name = tolower(`Scientific name`),
         iucn = `2021 IUCN Red List category`) %>%
  dplyr::select(scientific_name, iucn)

# Manual merge
sp_list <- left_join(sp_list, iucn, by='scientific_name')
  
# Export NA's for manual filling
write_csv(filter(sp_list, is.na(iucn)), './data/csv/ebird_iucn_fill.csv')

# Append filled
fill <- read_csv('./data/csv/ebird_iucn_filled.csv')
sp_list <- rbind(filter(sp_list, !is.na(iucn)), fill)

# Merge back to ebird
ebird <- left_join(ebird, sp_list[, c('species', 'iucn')], by='species')
ebird <- dummy_cols(ebird, select_columns = 'iucn')
rm(list=c('fill', 'iucn', 'sp_list'))
#----------------------------------------------------------------
## 4. CONSTRUCT SPECIES DIVERSITY
#---------------------------------------------------------------- 

# Higher Level Species Richness
# add sr/year, sr/user-ym, sr/u-yr
ebird <- ebird %>%
  group_by(yearmonth) %>%
  mutate(sr_ym = n_distinct(species_id)) %>% # species richness per year-month (all users)
  group_by(c_code_2011, yearmonth) %>%
  mutate(sr_dym = n_distinct(species_id)) %>%
  group_by(user_id, year) %>%
  mutate(n_mon_yr = n_distinct(yearmonth)) %>% # Months per year of birding - 'high ability' users
  group_by(user_id, c_code_2011, yearmonth) %>%
  mutate(sr_udym = n_distinct(species_id)) %>%
  ungroup()

# Lists with no counts (remove?)
nocount <- ebird %>%
  mutate(nocount = ifelse(species_count == 'X', 1, 0),
         species_count2 = as.numeric(na_if(species_count, 'X')),
         r10 = ifelse(species_count2 %% 10, 1, 0)) %>%
  summarize(nocount = mean(nocount, na.rm=T),
            r10 = mean(r10, na.rm=T))

nocount <- ebird %>%
  mutate(nocount = ifelse(species_count == 'X', 1, 0)) %>%
  group_by(trip_id) %>%
  summarize(nocount = max(nocount)) %>%
  ungroup() %>%
  summarize(nocount = mean(nocount, na.rm=T))
# 8.5% of trips have missing counts
# 95% of counts divisible by 10
rm(list='nocount')

# Convert count to numeric
ebird$species_count[ebird$species_count == 'X'] <- NA
ebird$species_count <- as.numeric(ebird$species_count)

# Species diversity per trip
ebird <- ebird %>% 
  group_by(trip_id) %>% 
  mutate(sr = n(),
         sr_cr = sum(iucn_CR, na.rm=T),
         sr_dd = sum(iucn_DD, na.rm=T),
         sr_en = sum(iucn_EN, na.rm=T),
         sr_lc = sum(iucn_LC, na.rm=T),
         sr_nt = sum(iucn_NT, na.rm=T),
         sr_vu = sum(iucn_VU, na.rm=T),
         sr_na = sum(iucn_NA, na.rm=T),
         sh_index = -sum((species_count / sum(species_count, na.rm = T))*
                                log(species_count / sum(species_count, na.rm = T)), na.rm = T),
         si_index = 1 - ((sum(species_count*(species_count - 1), na.rm = T)) /
                                 (sum(species_count, na.rm = T)*(sum(species_count, na.rm = T) - 1))))

#----------------------------------------------------------------
## 5. ALLOCATE TRIPS TO GRID CELLS
#---------------------------------------------------------------- 

# Trip level (n=1,049,930, 17,634 users)
ebird_trip <- ebird %>% 
  distinct(trip_id, .keep_all = T) %>%
  dplyr::select(date, user_id, trip_id, group_id, group_size, 
                year, yearmonth, duration, distance, lat, lon, 
                c_code_2011, protocol_code, n_mon_yr, hour,
                starts_with('sr'), sh_index, si_index) %>%
  mutate(sr_hr = (sr/duration)*60,
         distance = replace(distance, is.na(distance), 0),
         traveling = ifelse(protocol_code == 'P22', 1, 0)) %>%
  left_join(experience, by=c('user_id'='observer_id', 'yearmonth')) %>% # add experience
  ungroup()

# Set India grid
grid <- raster(extent(india_dist))
res(grid) <- .1
crs(grid) <- crs(india_dist)
grid <- setValues(grid, 1:ncell(grid))

# Assign cell id of trip
ebird_trip$cell_id <- raster::extract(grid, 
                                      st_as_sf(ebird_trip, 
                                               coords = c('lon', 'lat'), 
                                               crs = 4326))

# Number of cells per district
n_cells_dist <- cbind(st_drop_geometry(india_dist), exact_extract(grid, india_dist, 'count'))
names(n_cells_dist)[2] <- 'n_cells_dist'
ebird_trip <- left_join(ebird_trip, n_cells_dist, by='c_code_2011')

#----------------------------------------------------------------
## 5. EXPORT FINAL DATASETS
#---------------------------------------------------------------- 
setwd(READ)

# Trip Level
write_csv(ebird_trip, 'ebird_trip.csv')

# User-gridcell-month (n=299,070)
ebird_user_cell <- ebird_trip %>%
  group_by(user_id, cell_id, yearmonth) %>%
  summarize(n_trips=n(),
            sr=mean(sr, na.rm=T),
            sr_cr = mean(sr_cr, na.rm=T),
            sr_dd = mean(sr_dd, na.rm=T),
            sr_en = mean(sr_en, na.rm=T),
            sr_lc = mean(sr_lc, na.rm=T),
            sr_nt = mean(sr_nt, na.rm=T),
            sr_vu = mean(sr_vu, na.rm=T),
            sr_na = mean(sr_na, na.rm=T),
            sr_hr=mean(sr_hr, na.rm=T),
            sr_udym=first(sr_udym),
            sr_ym=first(sr_ym),
            sh_index=mean(sh_index, na.rm=T),
            si_index=mean(si_index, na.rm=T),
            duration=mean(duration, na.rm=T),
            distance=mean(distance,na.rm=T),
            hour=mean(hour, na.rm=T),
            traveling=mean(traveling, na.rm=T)*100,
            group_size=mean(group_size, na.rm=T),
            n_mon_yr=first(n_mon_yr),
            exp_idx=first(exp_idx),
            year=first(year),
            c_code_2011=first(c_code_2011),
            n_cells_dist=first(n_cells_dist))

write_csv(ebird_user_cell, paste(SAVE, 'data/csv/ebird_uct.csv', sep=""))

# User-district-month (n=182,722)
ebird_user <- ebird_trip %>%
  group_by(c_code_2011) %>%
  mutate(coverage_d = n_distinct(cell_id)) %>% # coverage by all users in district
  group_by(c_code_2011, yearmonth) %>% # coverage by all users in district-month
  mutate(coverage_dym = n_distinct(cell_id)) %>%
  group_by(user_id, c_code_2011, yearmonth) %>%
  summarize(n_trips=n(),
            sr=mean(sr, na.rm=T),
            sr_cr=mean(sr_cr, na.rm=T),
            sr_dd=mean(sr_dd, na.rm=T),
            sr_en=mean(sr_en, na.rm=T),
            sr_lc=mean(sr_lc, na.rm=T),
            sr_nt=mean(sr_nt, na.rm=T),
            sr_vu=mean(sr_vu, na.rm=T),
            sr_na=mean(sr_na, na.rm=T),
            sr_hr=mean(sr_hr, na.rm=T),
            sr_udym=first(sr_udym),
            sr_ym=first(sr_ym),
            sh_index=mean(sh_index, na.rm=T),
            si_index=mean(si_index, na.rm=T),
            duration=mean(duration, na.rm=T),
            distance=mean(distance, na.rm=T),
            hour=mean(hour, na.rm=T),
            traveling=mean(traveling, na.rm=T)*100,
            group_size=mean(group_size, na.rm=T),
            n_mon_yr=first(n_mon_yr),
            exp_idx=first(exp_idx),
            year=first(year),
            coverage_udym=n_distinct(cell_id),
            coverage_dym=first(coverage_dym),
            coverage_d=first(coverage_d),
            n_cells_dist=first(n_cells_dist))

write_csv(ebird_user, paste(SAVE, 'data/csv/ebird_udt.csv', sep=""))