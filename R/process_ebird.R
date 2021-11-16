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

# Read (n=13,481,001; n=21,870,724)
ebird <- fread('ebd_IN_201401_202012_relSep-2021.txt') # 2014-2020
#ebird <- fread('ebd_IN_201401_201904_relApr-2019.txt') # 2014-2018
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

ebird <- filter(ebird, YEAR > 2014) # n = 12,839,677, 13484 users, 726,420 trips
# (update: n=21,180,223, 20,845 users, 1,230,630 trips)

## 2. INITIAL FILTERING --------------------------------------------------------------

# Keep necessary columns
ebird <- ebird %>% dplyr::select(-c(1,2,4,6:8,10:15,17,19,20:23,25,30,35,42:48))

# All species reported (n=12,380,423) (COMMENT OUT FOR MAIN ANALYSIS)
#ebird <- filter(ebird, ALL.SPECIES.REPORTED == 1)

# Export protocol list for SM
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
          out=paste(save_path_head,'docs/manuscript/tables/protocol.tex', sep=''))

# Filter Protocol (stationary, travelling, banding, random) (n=12,380,423)
ebird <- filter(ebird, PROTOCOL.CODE %in% c('P21', 'P22', 'P33', 'P48'))
# (update: n=20,396,974)

# Drop Duplicates (n=8,564,865; n=14,458,040)
ebird$GROUP.IDENTIFIER[ebird$GROUP.IDENTIFIER == ''] <- NA
ebird_m <- ebird %>% filter(is.na(GROUP.IDENTIFIER))
ebird_nm <- ebird %>% filter(!is.na(GROUP.IDENTIFIER))
ebird_dd <- distinct(ebird_nm, GROUP.IDENTIFIER, TAXONOMIC.ORDER, .keep_all = T)
ebird <- rbind(ebird_m, ebird_dd)
rm(list=c('ebird_dd', 'ebird_m', 'ebird_nm', 'protocol'))

# Drop if species richness = 1 (n=8,514,319, 11,745 users; n=14,383,209, 17,851 users)
ebird <- ebird %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>%
  mutate(sr = n()) %>%
  filter(sr > 1) %>%
  ungroup()
  
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

# Remove out-of-bounds birds (n = 8,479,171; n=14,332,717)
ebird_oob <- ebird %>% filter(is.na(c_code_2011)) # (n=35,148, 805 users, 2570 trips, 64 districts; n=50,492, 1195 users, 4135 trips, 68 districts)
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
  mutate(sh_index = -sum((OBSERVATION.COUNT / sum(OBSERVATION.COUNT, na.rm = T))*
                                log(OBSERVATION.COUNT / sum(OBSERVATION.COUNT, na.rm = T)), na.rm = T),
         si_index = 1 - ((sum(OBSERVATION.COUNT*(OBSERVATION.COUNT - 1), na.rm = T)) /
                                 (sum(OBSERVATION.COUNT, na.rm = T)*(sum(OBSERVATION.COUNT, na.rm = T) - 1))))

# Trip-level (n=583,057 trips, 11,722 users; n=1,012,574 trips, 17,816)
ebird_trip <- ebird %>% 
  distinct(SAMPLING.EVENT.IDENTIFIER, .keep_all = T) %>%
  dplyr::select(OBSERVATION.DATE, OBSERVER.ID, SAMPLING.EVENT.IDENTIFIER,
                GROUP.IDENTIFIER, NUMBER.OBSERVERS, YEAR, YEARMONTH, 
                ALL.SPECIES.REPORTED, DURATION.MINUTES, EFFORT.DISTANCE.KM, 
                c_code_2011, n_mon_yr, starts_with('sr'), sh_index, si_index)

# Add experience
ebird_trip <- left_join(ebird_trip, experience, by=c('OBSERVER.ID', 'YEARMONTH'))
write.csv(ebird_trip, 'ebird_trip_update2020.csv', row.names = F)

# ----- Additional Filtering ----------

# User-district-month (n=114,392; n=191,515) w additional filtering
ebird_user <- ebird_trip %>%
  mutate(distance = replace(EFFORT.DISTANCE.KM, is.na(EFFORT.DISTANCE.KM), 0)) %>%
  group_by(OBSERVER.ID, c_code_2011, YEARMONTH) %>%
  summarize(n_trips=n(),
            sr=mean(sr, na.rm=T),
            sr_udym=first(sr_udym),
            sr_uym=first(sr_uym),
            sr_uyr=first(sr_uyr),
            sr_ym=first(sr_ym),
            sr_yr=first(sr_yr),
            sh_index=mean(sh_index, na.rm=T),
            si_index=mean(si_index,na.rm=T),
            duration=mean(DURATION.MINUTES, na.rm=T),
            distance=mean(distance,na.rm=T),
            all_species=mean(ALL.SPECIES.REPORTED,na.rm=T),
            group_size=mean(NUMBER.OBSERVERS, na.rm=T),
            n_mon_yr=first(n_mon_yr),
            exp_idx=first(exp_idx),
            year=first(YEAR))

write.csv(ebird_user,
          paste(save_path_head, 'data/csv/ebird_user_update2020.csv', sep=""),
          row.names = F)

## 5. SPATIAL COVERAGE ----------------------------------------------------------

# Initialize Grid
india_districts_shp <- readOGR(paste(dist_shp, "maps/india-district", sep=""),
                                'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

grid <- raster(extent(india_districts_shp))
res(grid) <- .05 # grid resolution in degrees lat-lon (set .01 or .05)
grid_res <- res(grid)[1]*100
proj4string(grid) <- proj4string(india_districts_shp)

# Cell Counts of Birds
bird_coords <- SpatialPoints(ebird[, 9:8], proj4string = crs(grid))
count_df <- spatial_coverage(bird_coords, grid)
 
# Aggregate to District
coverage_all <- count_df %>% 
  group_by(c_code_2011) %>% 
  summarize(coverage_all = mean(bird_obs, na.rm=T)) 
  
# Write
write.csv(coverage_all, 
        paste(save_path_head, 'data/csv/coverage_dist_grid_', grid_res,'km_update2020.csv', sep=""),
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
    summarize(coverage = mean(bird_obs, na.rm=T))
  
  # Time
  dist_ym_coverage$yearmonth <- ym
  
  # Append to Master
  coverage_ym <- rbind(coverage_ym, dist_ym_coverage)
}

# Write
write.csv(coverage_ym, 
          paste(save_path_head, 'data/csv/coverage_ym_grid_', grid_res,'km_update2020.csv', sep=""),
          row.names = F)