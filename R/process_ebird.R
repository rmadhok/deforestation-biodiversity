# PROJECT: Deforestation and Biodiversity
# PURPOSE: Prepare eBird for Analysis
# AUTHOR: Raahil Madhok
# DATE: Dec 26, 2018 [created]

### SET-UP
# Directories
rm(list=ls())
read_path_head <- '/Volumes/Backup Plus/research/data/def_biodiv/ebird'
save_path_head <- '/Users/rmadhok/Dropbox/def_biodiv/'
dist_shp <- '/Users/rmadhok/Dropbox/IndiaPowerPlant/data/'
setwd(read_path_head)

# Load Packages
require(data.table)
require(tidyverse)
require(sf)
require(raster)
require(lubridate)
require(sp)
require(rgdal)
require(stargazer)

# Read Custom Functions
source(paste(save_path_head, 'scripts/R/ebird_functions.R', sep=''))

## 1. LOAD EBIRD -----------------------------------------------------------------------

# Read (n=21,870,724)
ebird <- fread('ebd_IN_201401_202012_relSep-2021.txt') # 2014-2020
colnames(ebird) <- make.names(colnames(ebird))

# Format Dates
ebird$OBSERVATION.DATE <- ymd(ebird$OBSERVATION.DATE)
ebird$YEAR <- year(ebird$OBSERVATION.DATE)
ebird$YEARMONTH <- format(ebird$OBSERVATION.DATE, "%Y-%m")

# Experience from 2014
experience <- ebird %>%
  group_by(OBSERVER.ID, OBSERVATION.DATE) %>%
  summarize(trips_day = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            YEARMONTH = first(YEARMONTH)) %>%
  group_by(OBSERVER.ID) %>%
  arrange(OBSERVATION.DATE) %>%
  mutate(exp_idx = cumsum(trips_day)) %>%
  group_by(OBSERVER.ID, YEARMONTH) %>%
  arrange(OBSERVATION.DATE) %>%
  summarize(exp_idx = last(exp_idx))

# Keep necessary columns
ebird <- ebird %>% dplyr::select(-c(1,2,4,6:8,10:15,17,19,20:23,25,30,35,42:48))

## 2. INITIAL FILTERING --------------------------------------------------------------

# Filters from eBird manual (n=11,434,882)
ebird <- ebird %>%
  filter(
    YEAR > 2014,
    ALL.SPECIES.REPORTED == 1,
    DURATION.MINUTES <= 5*60,
    NUMBER.OBSERVERS <= 10
  )

# Export protocol list for SM (MOVE HIGHER?)
protocol <- ebird %>%
  distinct(SAMPLING.EVENT.IDENTIFIER, .keep_all = T) %>%
  group_by(PROTOCOL.TYPE) %>%
  summarize(`Num. Trips` = n()) %>%
  mutate(Pct. = round((`Num. Trips`/sum(`Num. Trips`)*100),2)) %>%
  rename(Protocol = PROTOCOL.TYPE) %>%
  arrange(desc(Pct.))
stargazer(protocol, 
          summary=F, 
          rownames=F, 
          out=paste(save_path_head,'docs/jmp/tex_doc/v3/tables/protocol.tex', sep=''))

# Filter Protocol (stationary, travelling) (n=11,375,851)
ebird <- filter(ebird, PROTOCOL.CODE %in% c('P21', 'P22'))

# Drop Duplicates (n=8,564,865; n=14,458,040)
#ebird$GROUP.IDENTIFIER[ebird$GROUP.IDENTIFIER == ''] <- NA
#ebird_m <- ebird %>% filter(is.na(GROUP.IDENTIFIER))
#ebird_nm <- ebird %>% filter(!is.na(GROUP.IDENTIFIER))
#ebird_dd <- distinct(ebird_nm, GROUP.IDENTIFIER, TAXONOMIC.ORDER, .keep_all = T)
#ebird <- rbind(ebird_m, ebird_dd)
#rm(list=c('ebird_dd', 'ebird_m', 'ebird_nm', 'protocol'))
  
## 2. OVERLAY DISTRICT CENSUS CODES -----------------------------------------------------

# Load Distric Map
india_dist <- st_read(paste(dist_shp, "maps/india-district", sep=""), 
                      'SDE_DATA_IN_F7DSTRBND_2011', 
                      stringsAsFactors = F)

# Overlay (point-in-poly) - 10 mins
ebird$c_code_2011 <- as.data.frame(st_join(st_as_sf(ebird, 
                                                    coords = c('LONGITUDE', 'LATITUDE'), 
                                                    crs = 4326), 
                                           india_dist, join = st_intersects))$c_code_11

# Remove out-of-bounds birds (n = 11,319,749)
ebird_oob <- ebird %>% filter(is.na(c_code_2011)) # (n=47,611, 813 users, 3311 trips, 62 districts)
ebird <- ebird %>% filter(!is.na(c_code_2011))
rm(list='ebird_oob')

## 3. CONSTRUCT VARS -------------------------------------------------------------------

# Higher Level Species Richness (~10 mins)
ebird <- ebird %>%
  group_by(YEARMONTH) %>%
  mutate(sr_ym = n_distinct(TAXONOMIC.ORDER)) %>% # species richness per year-month (all users)
  group_by(YEAR) %>%
  mutate(sr_yr = n_distinct(TAXONOMIC.ORDER)) %>% # species richness per year
  group_by(OBSERVER.ID, YEAR) %>%
  mutate(n_mon_yr = n_distinct(YEARMONTH), # Months per year of birding - 'high ability' users
         sr_uyr = n_distinct(TAXONOMIC.ORDER)) %>% # species richnes per user-year
  group_by(OBSERVER.ID, YEARMONTH) %>%
  mutate(sr_uym = n_distinct(TAXONOMIC.ORDER)) %>% # species richness per user-year-month
  group_by(OBSERVER.ID, c_code_2011, YEARMONTH) %>%
  mutate(sr_udym = n_distinct(TAXONOMIC.ORDER))

## 4. OTHER DIVERSITY INDICES ------------------------------------------------------------------

# Convert count to numeric
ebird$OBSERVATION.COUNT[ebird$OBSERVATION.COUNT == 'X'] <- NA
ebird$OBSERVATION.COUNT <- as.numeric(ebird$OBSERVATION.COUNT)

# ----- Species diversity per trip ----------
ebird <- ebird %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  mutate(sr = n(),
         sh_index = -sum((OBSERVATION.COUNT / sum(OBSERVATION.COUNT, na.rm = T))*
                                log(OBSERVATION.COUNT / sum(OBSERVATION.COUNT, na.rm = T)), na.rm = T),
         si_index = 1 - ((sum(OBSERVATION.COUNT*(OBSERVATION.COUNT - 1), na.rm = T)) /
                                 (sum(OBSERVATION.COUNT, na.rm = T)*(sum(OBSERVATION.COUNT, na.rm = T) - 1))))

## 4. TRIP LEVEL ------------------------------------------------------------------

# Collapse (n=552,117 trips, 12,452 users)
ebird_trip <- ebird %>% 
  distinct(SAMPLING.EVENT.IDENTIFIER, .keep_all = T) %>%
  dplyr::select(OBSERVATION.DATE, OBSERVER.ID, SAMPLING.EVENT.IDENTIFIER,
                GROUP.IDENTIFIER, NUMBER.OBSERVERS, YEAR, YEARMONTH, 
                DURATION.MINUTES, EFFORT.DISTANCE.KM, LATITUDE, LONGITUDE,
                c_code_2011, n_mon_yr, starts_with('sr'), sh_index, si_index) %>%
  mutate(sr_hr = (sr/DURATION.MINUTES)*60, # species diversity per hr
         sh_index_hr = (sh_index/DURATION.MINUTES)*60,
         si_index_hr = (si_index/DURATION.MINUTES)*60) %>%
  left_join(experience, by=c('OBSERVER.ID', 'YEARMONTH')) %>% # add experience
  ungroup()

# Grid cell
grid <- raster(extent(india_dist))
res(grid) <- .1
crs(grid) <- crs(india_dist)
grid <- setValues(grid, 1:ncell(grid))
ebird_trip$cell <- extract(grid, st_as_sf(as.data.frame(ebird_trip %>% ungroup()), 
                                          coords = c('LONGITUDE', 'LATITUDE'), 
                                          crs = 4326))
write.csv(ebird_trip, 'ebird_trip.csv', row.names = F)

# ----- Additional Filtering ----------

# User-gridcell-month
ebird_user_cell <- ebird_trip %>%
  mutate(distance = replace(EFFORT.DISTANCE.KM, is.na(EFFORT.DISTANCE.KM), 0)) %>%
  group_by(OBSERVER.ID, cell, YEARMONTH) %>%
  summarize(n_trips=n(),
            sr=mean(sr, na.rm=T),
            sr_hr=mean(sr_hr, na.rm=T),
            sr_udym=first(sr_udym),
            sr_uym=first(sr_uym),
            sr_uyr=first(sr_uyr),
            sr_ym=first(sr_ym),
            sr_yr=first(sr_yr),
            sh_index=mean(sh_index, na.rm=T),
            sh_index_hr=mean(sh_index_hr, na.rm=T),
            si_index=mean(si_index,na.rm=T),
            si_index_hr=mean(si_index_hr, na.rm=T),
            duration=mean(DURATION.MINUTES, na.rm=T),
            distance=mean(distance,na.rm=T),
            group_size=mean(NUMBER.OBSERVERS, na.rm=T),
            n_mon_yr=first(n_mon_yr),
            exp_idx=first(exp_idx),
            year=first(YEAR),
            c_code_2011=first(c_code_2011))

write_csv(ebird_user_cell,
          paste(save_path_head, 'data/csv/ebird_user_cell.csv', sep=""))

# User-district-month (n=123,898)
ebird_user <- ebird_trip %>%
  mutate(distance = replace(EFFORT.DISTANCE.KM, is.na(EFFORT.DISTANCE.KM), 0)) %>%
  group_by(OBSERVER.ID, c_code_2011, YEARMONTH) %>%
  summarize(n_trips=n(),
            sr=mean(sr, na.rm=T),
            sr_hr=mean(sr_hr, na.rm=T),
            sr_udym=first(sr_udym),
            sr_uym=first(sr_uym),
            sr_uyr=first(sr_uyr),
            sr_ym=first(sr_ym),
            sr_yr=first(sr_yr),
            sh_index=mean(sh_index, na.rm=T),
            sh_index_hr=mean(sh_index_hr, na.rm=T),
            si_index=mean(si_index,na.rm=T),
            si_index_hr=mean(si_index_hr, na.rm=T),
            duration=mean(DURATION.MINUTES, na.rm=T),
            distance=mean(distance,na.rm=T),
            group_size=mean(NUMBER.OBSERVERS, na.rm=T),
            n_mon_yr=first(n_mon_yr),
            exp_idx=first(exp_idx),
            year=first(YEAR))

write_csv(ebird_user,
          paste(save_path_head, 'data/csv/ebird_user.csv', sep=""))

## 5. SPATIAL COVERAGE ----------------------------------------------------------

# Initialize Grid
india_districts_shp <- readOGR(paste(dist_shp, "maps/india-district", sep=""),
                                'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

grid <- raster(extent(india_districts_shp))
res(grid) <- .05 # (or .01)
grid_res <- res(grid)[1]*100
proj4string(grid) <- proj4string(india_districts_shp)

# Cell Counts of Birds
bird_coords <- SpatialPoints(ebird[, 9:8], proj4string = crs(grid))
count_df <- spatial_coverage(bird_coords, grid)
 
# Aggregate to District
coverage_all <- count_df %>% 
  group_by(c_code_2011) %>% 
  summarize(coverage_all = mean(bird_obs, na.rm=T)*100) 
  
# Write
write.csv(coverage_all, 
        paste(save_path_head, 'data/csv/coverage_dist_grid_', grid_res,'km.csv', sep=""),
        row.names = F)

# Compute Monthly Spatial Coverage
ebird$YEARMONTH <- as.factor(ebird$YEARMONTH)
coverage_ym <- data.frame()
for(ym in levels(ebird$YEARMONTH)){
  
  print(paste('Computing Spatial Coverage of Date:', ym))
  
  # Get Bird Coordinates of year-month
  bird_coords <- SpatialPoints(ebird[ebird$YEARMONTH == ym, 9:8], 
                               proj4string=crs(grid))
  
  # Run My Coverage Function
  count_df <- spatial_coverage(bird_coords, grid)
  
  # Aggregate to District
  dist_ym_coverage <- count_df %>%
    group_by(c_code_2011) %>%
    summarize(coverage = mean(bird_obs, na.rm=T)*100)
  
  # Time
  dist_ym_coverage$yearmonth <- ym
  
  # Append to Master
  coverage_ym <- rbind(coverage_ym, dist_ym_coverage)
}

# Write
write.csv(coverage_ym, 
          paste(save_path_head, 'data/csv/coverage_ym_grid_', grid_res,'km.csv', sep=""),
          row.names = F)