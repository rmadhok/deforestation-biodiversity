# PROJECT: Deforestation and Biodiversity
# PURPOSE: Prepare eBird for Heckman Model
# AUTHOR: Raahil Madhok
# DATE: Oct 31 2019 [created]

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
ebird <- filter(ebird, YEAR > 2014)

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

# Keep necessary columns (n=12,839,677)
ebird <- ebird %>% dplyr::select(-c(1,2,10:14,16:22,24,29,34,41:47))
rm(list='ebird_oob')

## 3. Indicators -----------------------------------------------------------------

# Drop Duplicates (n=8,918-836)
ebird$GROUP.IDENTIFIER[ebird$GROUP.IDENTIFIER == ''] <- NA
ebird_m <- ebird %>% filter(is.na(GROUP.IDENTIFIER))
ebird_nm <- ebird %>% filter(!is.na(GROUP.IDENTIFIER))
ebird_dd <- distinct(ebird_nm, GROUP.IDENTIFIER, TAXONOMIC.ORDER, .keep_all = T)
ebird <- rbind(ebird_m, ebird_dd)
rm(list=c('ebird_dd', 'ebird_m', 'ebird_nm'))

# Species diversity per trip
ebird$OBSERVATION.COUNT[ebird$OBSERVATION.COUNT == 'X'] <- NA
ebird$OBSERVATION.COUNT <- as.numeric(ebird$OBSERVATION.COUNT)
ebird <- ebird %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  mutate(s_richness = n(),
         sh_index = -sum((OBSERVATION.COUNT / sum(OBSERVATION.COUNT, na.rm = T))*
                           log(OBSERVATION.COUNT / sum(OBSERVATION.COUNT, na.rm = T)), na.rm = T),
         si_index = 1 - ((sum(OBSERVATION.COUNT*(OBSERVATION.COUNT - 1), na.rm = T)) /
                           (sum(OBSERVATION.COUNT, na.rm = T)*(sum(OBSERVATION.COUNT, na.rm = T) - 1))))

# Months per year
ebird <- ebird %>%
  group_by(OBSERVER.ID, YEAR) %>%
  mutate(n_months = n_distinct(YEARMONTH))

# Trip-level (n=686,897, 13,426 users)
ebird_trip <- ebird %>% 
  distinct(SAMPLING.EVENT.IDENTIFIER, .keep_all = T) %>%
  dplyr::select(OBSERVER.ID, SAMPLING.EVENT.IDENTIFIER, YEAR,
                DURATION.MINUTES, EFFORT.DISTANCE.KM, YEARMONTH, 
                NUMBER.OBSERVERS, PROTOCOL.CODE, n_months, 
                ALL.SPECIES.REPORTED, c_code_2011, s_richness, 
                sh_index, si_index) %>%
  rename(distance = EFFORT.DISTANCE.KM, duration = DURATION.MINUTES)

write.csv(ebird_trip,
          paste(save_path_head, 'data/csv/ebird_trip_full.csv', sep=""),
          row.names = F)




