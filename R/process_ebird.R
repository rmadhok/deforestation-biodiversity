# PROJECT: Deforestation and Biodiversity
# PURPOSE: Prepare eBird for Analysis
# AUTHOR: Raahil Madhok

### SET-UP
# Directories
rm(list=ls())
READ <- '/Volumes/Backup Plus/research/data/def_biodiv/ebird'
SAVE <- '/Users/rmadhok/Dropbox/def_biodiv/'
SHP <- '/Users/rmadhok/Dropbox/IndiaPowerPlant/data/'
setwd(READ)

# Load Packages
options(collapse_mask = "manip") 
require(collapse)
require(data.table)
require(tidyverse)
require(sf)
require(raster)
require(lubridate)
require(stargazer)
require(exactextractr)
#----------------------------------------------------------------
## 1. LOAD EBIRD + INITIAL CLEANING
#----------------------------------------------------------------

# Read (n=21,870,724)
ebird <- fread('ebd_IN_201401_202012_relSep-2021.txt') # 2014-2020
colnames(ebird) <- gsub('\\.', '_', tolower(make.names(colnames(ebird))))

# Format Dates
ebird$observation_date <- ymd(ebird$observation_date)
ebird$year <- year(ebird$observation_date)
ebird$yearmonth <- format(ebird$observation_date, "%Y-%m")

# cumulative experience since 2014
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
                'protocol_type', 'protocol_code', 'duration_minutes', 
                'effort_distance_km', 'number_observers', 'all_species_reported', 
                'group_identifier', 'year', 'yearmonth')

# Clean names
ebird <- ebird %>%
  rename(species_id = taxonomic_order,
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
stargazer(protocol, 
          summary=F, 
          rownames=F, 
          out=paste(SAVE,'docs/jmp/tex_doc/v3/tables/protocol.tex', sep=''))

# Filter Protocol (stationary, travelling) (n=18,458,042)
ebird <- filter(ebird, protocol_code %in% c('P21', 'P22'))
#----------------------------------------------------------------
## 3. IDENTIFY DISTRICTS AND COASTAL TRIPS
#----------------------------------------------------------------  

# Distric Map
india_dist <- st_read(paste(SHP, "maps/india-district", sep=""), 
                      'SDE_DATA_IN_F7DSTRBND_2011', 
                      stringsAsFactors = F) %>%
  dplyr::select(c_code_11) %>% 
  rename(c_code_2011=c_code_11)

# Overlay (point-in-poly) - 10 mins
ebird$c_code_2011 <- st_join(st_as_sf(ebird, coords = c('lon', 'lat'), crs = 4326), 
                             india_dist, join = st_intersects)$c_code_2011

# Remove out-of-bounds birds (n = 18,390,933)
ebird_oob <- ebird %>% filter(is.na(c_code_2011)) # (n=5101 trips, 68 districts)
ebird <- ebird %>% filter(!is.na(c_code_2011))
rm(list='ebird_oob')

#----------------------------------------------------------------
## 4. CONSTRUCT SPECIES DIVERSITY
#---------------------------------------------------------------- 

# Higher Level Species Richness (~10 mins)
# add sr/year, sr/user-ym 
ebird <- ebird %>%
  group_by(yearmonth) %>%
  mutate(sr_ym = n_distinct(species_id)) %>% # species richness per year-month (all users)
  group_by(user_id, year) %>%
  mutate(n_mon_yr = n_distinct(yearmonth), # Months per year of birding - 'high ability' users
         sr_uyr = n_distinct(species_id)) %>% # species richness per user-year
  group_by(user_id, c_code_2011, yearmonth) %>%
  mutate(sr_udym = n_distinct(species_id))

# Convert count to numeric
ebird$species_count[ebird$species_count == 'X'] <- NA
ebird$species_count <- as.numeric(ebird$species_count)

# Species diversity per trip
ebird <- ebird %>% 
  group_by(trip_id) %>% 
  mutate(sr = n(),
         sh_index = -sum((species_count / sum(species_count, na.rm = T))*
                                log(species_count / sum(species_count, na.rm = T)), na.rm = T),
         si_index = 1 - ((sum(species_count*(species_count - 1), na.rm = T)) /
                                 (sum(species_count, na.rm = T)*(sum(species_count, na.rm = T) - 1))))

#----------------------------------------------------------------
## 5. ALLOCATE TRIPS TO GRID CELLS
#---------------------------------------------------------------- 

# Trip level (n=1,04,930, 17,634 users)
ebird_trip <- ebird %>% 
  ungroup() %>%
  distinct(trip_id, .keep_all = T) %>%
  dplyr::select(date, user_id, trip_id, group_id, group_size, year,
                yearmonth, duration, distance, lat, lon, c_code_2011, 
                protocol_code, n_mon_yr, starts_with('sr'), sh_index, si_index) %>%
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
ebird_trip$cell_id <- extract(grid, st_as_sf(ebird_trip, coords = c('lon', 'lat'), crs = 4326))

# Number of cells per district
n_cells_dist <- cbind(st_drop_geometry(india_dist), exact_extract(grid, india_dist, 'count'))
names(n_cells_dist)[2] <- 'n_cells_dist'
ebird_trip <- left_join(ebird_trip, n_cells_dist, by='c_code_2011')

#----------------------------------------------------------------
## 5. EXPORT FINAL DATASETS
#---------------------------------------------------------------- 

# Trip Level
write_csv(ebird_trip, 'ebird_trip.csv')

# User-gridcell-month
ebird_user_cell <- ebird_trip %>%
  group_by(user_id, cell_id, yearmonth) %>%
  summarize(n_trips=n(),
            sr=mean(sr, na.rm=T),
            sr_hr=mean(sr_hr, na.rm=T),
            sr_udym=first(sr_udym),
            sr_uyr=first(sr_uyr),
            sr_ym=first(sr_ym),
            sh_index=mean(sh_index, na.rm=T),
            si_index=mean(si_index, na.rm=T),
            duration=mean(duration, na.rm=T),
            distance=mean(distance,na.rm=T),
            traveling = mean(traveling, na.rm=T)*100,
            group_size=mean(group_size, na.rm=T),
            n_mon_yr=first(n_mon_yr),
            exp_idx=first(exp_idx),
            year=first(year),
            c_code_2011=first(c_code_2011),
            n_cells_dist=first(n_cells_dist))

write_csv(ebird_user_cell, paste(SAVE, 'data/csv/ebird_uct.csv', sep=""))

# User-district-month
ebird_user <- ebird_trip %>% 
  group_by(c_code_2011) %>%
  mutate(coverage_d = n_distinct(cell_id)) %>%
  group_by(c_code_2011, yearmonth) %>% 
  mutate(coverage_dym = n_distinct(cell_id)) %>%
  group_by(user_id, c_code_2011, yearmonth) %>%
  summarize(n_trips=n(),
            sr=mean(sr, na.rm=T),
            sr_hr=mean(sr_hr, na.rm=T),
            sr_udym=first(sr_udym),
            sr_uyr=first(sr_uyr),
            sr_ym=first(sr_ym),
            sh_index=mean(sh_index, na.rm=T),
            si_index=mean(si_index, na.rm=T),
            duration=mean(duration, na.rm=T),
            distance=mean(distance, na.rm=T),
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