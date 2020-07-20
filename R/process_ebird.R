# PROJECT: Deforestation and Biodiversity
# PURPOSE: Prepare eBird for Analysis
# AUTHOR: Raahil Madhok
# DATE: Dec 26, 2018 [created]

### SET-UP
# Directories
read_path_head <- '/Volumes/Backup Plus/research/def_biodiv/ebird'
save_path_head <- '/Users/rmadhok/Dropbox (Personal)/def_biodiv/'
dist_shp <- '/Users/rmadhok/Dropbox (Personal)/IndiaPowerPlant/data/'
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

# Read (n=13,481,001)
ebird <- fread('ebd_IN_201401_201904_relApr-2019.txt')
colnames(ebird) <- make.names(colnames(ebird))

# Format Dates
ebird$OBSERVATION.DATE <- ymd(ebird$OBSERVATION.DATE)
ebird$YEAR <- year(ebird$OBSERVATION.DATE)
ebird$YEARMONTH <- format(ebird$OBSERVATION.DATE, "%Y-%m")
ebird_14 <- filter(ebird, YEAR == 2014)
ebird <- filter(ebird, YEAR > 2014) # n = 12,839,677

usage14 <- ebird_14 %>% 
  group_by(YEARMONTH) %>% 
  summarize(users = n_distinct(OBSERVER.ID),
            trips = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            districts = n_distinct(COUNTY),
            mean = users/sum(users))

usage15 <- ebird %>% 
  filter(YEAR == 2015) %>%
  group_by(YEARMONTH) %>% 
  summarize(users = n_distinct(OBSERVER.ID),
            trips = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            districts = n_distinct(COUNTY))

## 2. OVERLAY DISTRICT CENSUS CODES -----------------------------------------------------

# Load Distric Map
india_districts <- st_read(paste(dist_shp, "maps/india-district", sep=""), 
                           'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

# Overlay (point-in-poly)
ebird$c_code_2011 <- as.data.frame(st_join(st_as_sf(ebird, 
                                                    coords = c('LONGITUDE', 'LATITUDE'), 
                                                    crs = 4326), 
                                           india_districts, join = st_intersects))$c_code_11

# Handle out-of-bounds birds
ebird_oob <- ebird %>% filter(is.na(c_code_2011)) %>% dplyr::select(-c_code_2011) # Birds ouside district
ebird_oob$ID <- st_nearest_feature(st_as_sf(ebird_oob, 
                                            coords = c('LONGITUDE', 'LATITUDE'), 
                                            crs = 4326), india_districts) # ID of nearest district
ebird_oob <- merge(ebird_oob, st_drop_geometry(india_districts)[ ,c('ID','c_code_11')], 
                   by = 'ID', all.x = TRUE) # Get census code of nearest district
ebird_oob <- ebird_oob %>% dplyr::select(-ID) %>% rename(c_code_2011 = c_code_11)
ebird <- filter(ebird, !is.na(c_code_2011))
ebird <- rbind(ebird, ebird_oob) 

# Keep necessary columns (n=12,839,677) # ADD START/END TIME
ebird <- ebird %>% dplyr::select(-c(1,2,10:14,16:22,24,29,34,41:47))
rm(list='ebird_oob')

## 3. FILTER DATA ----------------------------------------------------------------------

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
pcol_stat <- stargazer(protocol, 
                       type = 'html', 
                       summary=F, rownames=F, 
                       out=paste(save_path_head, 'docs/manuscript/tables/protocol.doc', sep=''))

# Protocol (stationary, travelling, banding, random) (n=12,252,132)
ebird <- filter(ebird, PROTOCOL.CODE %in% c('P21', 'P22', 'P33', 'P48'))

# Drop Duplicates (n=8,564,865)
ebird$GROUP.IDENTIFIER[ebird$GROUP.IDENTIFIER == ''] <- NA
ebird_m <- ebird %>% filter(is.na(GROUP.IDENTIFIER))
ebird_nm <- ebird %>% filter(!is.na(GROUP.IDENTIFIER))
ebird_dd <- distinct(ebird_nm, GROUP.IDENTIFIER, TAXONOMIC.ORDER, .keep_all = T)
ebird <- rbind(ebird_m, ebird_dd)
rm(list=c('ebird_dd', 'ebird_m', 'ebird_nm'))

## 4. CONSTRUCT VARS -------------------------------------------------------------------

# Higher Level Species Richness (~10 mins)
ebird <- ebird %>%
  group_by(YEARMONTH) %>%
  mutate(s_richness_ym = n_distinct(TAXONOMIC.ORDER)) %>% # species richness per year-month (all users)
  group_by(YEAR) %>%
  mutate(s_richness_yr = n_distinct(TAXONOMIC.ORDER)) %>% # species richness per year
  group_by(OBSERVER.ID, YEAR) %>%
  mutate(n_mon_yr = n_distinct(YEARMONTH), # Months per year of birding - 'high ability' users
         s_richness_uyr = n_distinct(TAXONOMIC.ORDER)) %>% # species richnes per user-year
  group_by(OBSERVER.ID, YEARMONTH) %>%
  mutate(s_richness_uym = n_distinct(TAXONOMIC.ORDER)) %>% # species richness per user-year-month
  group_by(OBSERVER.ID, c_code_2011, YEARMONTH) %>%
  mutate(s_richness_udym = n_distinct(TAXONOMIC.ORDER)) # species richness per user-district-year-month

## 4. DIVERSITY INDICES ------------------------------------------------------------------

# Convert count to numeric
ebird$OBSERVATION.COUNT[ebird$OBSERVATION.COUNT == 'X'] <- NA
ebird$OBSERVATION.COUNT <- as.numeric(ebird$OBSERVATION.COUNT)

# ----- Species diversity per trip ----------
ebird <- ebird %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  mutate(s_richness = n(),
         sh_index = -sum((OBSERVATION.COUNT / sum(OBSERVATION.COUNT, na.rm = T))*
                                log(OBSERVATION.COUNT / sum(OBSERVATION.COUNT, na.rm = T)), na.rm = T),
         si_index = 1 - ((sum(OBSERVATION.COUNT*(OBSERVATION.COUNT - 1), na.rm = T)) /
                                 (sum(OBSERVATION.COUNT, na.rm = T)*(sum(OBSERVATION.COUNT, na.rm = T) - 1))))

# Trip-level (n=636,211 trips, 12,606 users)
ebird_trip <- ebird %>% 
  distinct(SAMPLING.EVENT.IDENTIFIER, .keep_all = T) %>%
  dplyr::select(OBSERVATION.DATE, OBSERVER.ID, SAMPLING.EVENT.IDENTIFIER,
                GROUP.IDENTIFIER, NUMBER.OBSERVERS, YEAR, YEARMONTH, 
                ALL.SPECIES.REPORTED, DURATION.MINUTES, EFFORT.DISTANCE.KM, 
                c_code_2011, n_mon_yr, starts_with('s_richness'), 
                sh_index, si_index)

write.csv(ebird_trip,
          paste(save_path_head, 'data/csv/ebird_trip.csv', sep=""),
          row.names = F)

# Number of Trips before Jan 2018
ebird_j18 <- ebird_trip %>%
  filter(YEAR < 2018) %>%
  group_by(OBSERVER.ID) %>%
  summarize(n_trips_18 = n())
ebird_trip <- merge(ebird_trip, ebird_j18, by='OBSERVER.ID', all=T)
rm(list='ebird_j18')

# User-district-month (n=120,423)
ebird_user <- ebird_trip %>%
  group_by(OBSERVER.ID, c_code_2011, YEARMONTH) %>%
  summarize(n_trips=n(),
            s_richness=mean(s_richness, na.rm=T),
            s_richness_udym=first(s_richness_udym),
            s_richness_uym=first(s_richness_uym),
            s_richness_uyr=first(s_richness_uyr),
            s_richness_ym=first(s_richness_ym),
            s_richness_yr=first(s_richness_yr),
            sh_index=mean(sh_index, na.rm=T),
            si_index=mean(si_index,na.rm=T),
            duration=mean(DURATION.MINUTES, na.rm=T),
            distance=mean(EFFORT.DISTANCE.KM,na.rm=T),
            all_species=mean(ALL.SPECIES.REPORTED,na.rm=T),
            group_size=mean(NUMBER.OBSERVERS, na.rm=T),
            n_trips_18=first(n_trips_18),
            n_mon_yr=first(n_mon_yr),
            year=first(YEAR))

write.csv(ebird_user,
          paste(save_path_head, 'data/csv/ebird_user.csv', sep=""),
          row.names = F)

## 5. SPATIAL COVERAGE ----------------------------------------------------------

# Initialize Grid
india_districts_shp <- readOGR(paste(dist_shp, "maps/india-district", sep=""),
                                'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

grid <- raster(extent(india_districts_shp))
res(grid) <- .05 # grid resolution in degrees lat-lon
grid_res <- res(grid)[1]*100
proj4string(grid) <- proj4string(india_districts_shp)

# Cell Counts of Birds
bird_coords <- SpatialPoints(ebird[, 12:11], proj4string = crs(grid))
count_df <- spatial_coverage(bird_coords, grid)
 
# Aggregate to District
coverage_all <- count_df %>% 
  group_by(c_code_2011) %>% 
  summarize(coverage_all = mean(bird_obs)) 
  
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
  bird_coords <- SpatialPoints(ebird[ebird$YEARMONTH == ym, 12:11], 
                               proj4string=crs(grid))
  
  # Run My Coverage Function
  count_df <- spatial_coverage(bird_coords, grid)
  
  # Aggregate to District
  dist_ym_coverage <- count_df %>%
    group_by(c_code_2011) %>%
    summarize(coverage = mean(bird_obs))
  
  # Time
  dist_ym_coverage$yearmonth <- ym
  
  # Append to Master
  coverage_ym <- rbind(coverage_ym, dist_ym_coverage)
}

# Write
write.csv(coverage_ym, 
          paste(save_path_head, 'data/csv/coverage_ym_grid_', grid_res,'km.csv', sep=""),
          row.names = F)