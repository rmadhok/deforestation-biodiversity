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

## 2. ATTACH DISTRICT CENSUS CODES -----------------------------------------------------

# Load districts
india_districts <- st_read(paste(dist_shp, "maps/india-district", sep=""), 
                           'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

# Get district code of bird (Point-in-Poly)
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

## 3. FILTER DATA ----------------------------------------------------------------------

# ebird <- subset(ebird, DURATION.MINUTES >= 5 & DURATION.MINUTES <= 240)

# All species reported (n=12,281,846)
ebird <- filter(ebird, ALL.SPECIES.REPORTED == 1)
# Protocol (stationary, travelling, historical, banding, random) (n=12,252,132)
ebird <- filter(ebird, PROTOCOL.CODE %in% c('P21', 'P22', 'P62', 'P33', 'P48'))

# Drop Duplicates (n=8,468,361)
ebird$GROUP.IDENTIFIER[ebird$GROUP.IDENTIFIER == ''] <- NA
ebird_m <- ebird %>% filter(is.na(GROUP.IDENTIFIER))
ebird_nm <- ebird %>% filter(!is.na(GROUP.IDENTIFIER))
ebird_dd <- distinct(ebird_nm, GROUP.IDENTIFIER, TAXONOMIC.ORDER, .keep_all = T)
ebird <- rbind(ebird_m, ebird_dd)
rm(list=c('ebird_dd', 'ebird_m', 'ebird_nm'))

# Keep Veterans (n=7,082,470; 3155 users)
ebird <- ebird %>%
  group_by(YEAR, OBSERVER.ID) %>%
  mutate(n_months = n_distinct(YEARMONTH))
ebird_vet1 <- ebird %>% filter(YEAR < 2019, n_months >= 5)
ebird_vet2 <- ebird %>% filter(YEAR == 2019, n_months >= 2)
ebird <- rbind(ebird_vet1, ebird_vet2)
rm(list=c('ebird_vet1', 'ebird_vet2'))

# Plot
# ggplot() +
#   geom_polygon(data = india.districts.2011, aes(x=long,y=lat,group=group),fill= 'white', color = 'grey', size=.5) + 
#   geom_point(data=ebird, aes(x=LONGITUDE,y=LATITUDE), color='red', alpha=.03) +
#   coord_equal() + theme_minimal() + 
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         axis.line = element_blank(), 
#         axis.text.x = element_blank(), 
#         axis.text.y = element_blank(),  
#         axis.ticks = element_blank(), 
#         axis.title.x = element_blank(),  
#         axis.title.y = element_blank(),
#         legend.position = 'bottom',
#         legend.title=element_blank())
# ggsave(paste(save_path_head, 'docs/tex_doc/fig/ebird_all.png', sep=''), width=8,height=6)

## 4. DIVERSITY INDICES ------------------------------------------------------------------

# ----- Species diversity per trip
ebird$OBSERVATION.COUNT[ebird$OBSERVATION.COUNT == 'X'] <- NA
ebird$OBSERVATION.COUNT <- as.numeric(ebird$OBSERVATION.COUNT)
ebird <- ebird %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  mutate(s_richness = n(),
         sh_index = -sum((OBSERVATION.COUNT / sum(OBSERVATION.COUNT, na.rm = T))*
                                log(OBSERVATION.COUNT / sum(OBSERVATION.COUNT, na.rm = T)), na.rm = T),
         si_index = 1 - ((sum(OBSERVATION.COUNT*(OBSERVATION.COUNT - 1), na.rm = T)) /
                                 (sum(OBSERVATION.COUNT, na.rm = T)*(sum(OBSERVATION.COUNT, na.rm = T) - 1))))

# Trip-level (n=510,991 trips)
ebird_trip <- distinct(ebird, SAMPLING.EVENT.IDENTIFIER, .keep_all = T)
ebird_trip <- ebird_trip %>% 
  dplyr::select(OBSERVER.ID,SAMPLING.EVENT.IDENTIFIER, PROTOCOL.TYPE, DURATION.MINUTES, 
                EFFORT.DISTANCE.KM, YEARMONTH, c_code_2011, s_richness, sh_index, si_index) %>%
  rename(distance = EFFORT.DISTANCE.KM, duration = DURATION.MINUTES)

ebird_ntrips <- ebird_trip %>% 
  group_by(c_code_2011, YEARMONTH) %>% 
  summarize(n_trips = n(), n_birders = n_distinct(OBSERVER.ID))

# Save 
write.csv(ebird_trip,
          paste(save_path_head, 'data/csv/ebird_triplevel.csv', sep=""),
          row.names = F)

# Aggregate
ebird_trip <- ebird_trip %>%
  group_by(c_code_2011, YEARMONTH) %>%
  summarise_at(vars(s_richness, sh_index, si_index, duration, distance), 
               list(mean = mean, md = median), na.rm = T)
ebird_trip <- merge(ebird_trip, ebird_ntrips, by=c('c_code_2011','YEARMONTH'))
rm(list='ebird_ntrips')

# ------ Species diversity across all users in district-month

# Species counts
ebird_dist <- ebird %>%
  group_by(c_code_2011, YEARMONTH, TAXONOMIC.ORDER) %>%
  summarize(count = sum(OBSERVATION.COUNT, na.rm = T))

# Diversity indices
ebird_didx <- ebird_dist %>%
  group_by(c_code_2011, YEARMONTH) %>%
  summarize(s_richness_d = n(),
            sh_index_d = -sum((count / sum(count, na.rm = T))*log(count / sum(count, na.rm = T)), na.rm = T),
            si_index_d = 1 - ((sum(count*(count - 1), na.rm = T)) / (sum(count, na.rm = T)*(sum(count, na.rm = T) - 1))))
ebird_all <- merge(ebird_trip, ebird_didx, by=c('c_code_2011', 'YEARMONTH'))

# Write
write.csv(ebird_all,
          paste(save_path_head, 'data/csv/ebird_all.csv', sep=""),
          row.names = F)
rm(list=c('ebird_trip','ebird_all', 'ebird_didx', 'ebird_dist'))

## 5. SPATIAL COVERAGE ------------------------------------

# Initialize Grid
india_districts_shp <- readOGR(paste(dist_shp, "maps/india-district", sep=""),
                                'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)
grid <- raster(extent(india_districts_shp))
res(grid) <- .08 # grid resolution in degrees lat-lon
proj4string(grid) <- proj4string(india_districts_shp)

# Cell Counts of Birds
bird_coords <- SpatialPoints(ebird[, 12:11], proj4string = crs(grid))
#bird_coords <- st_as_sf(ebird[, 12:11], coords = c('LONGITUDE', 'LATITUDE'), crs = 4326)
count_df <- spatial_coverage(bird_coords, grid)

# Aggregate to District
coverage_all <- count_df %>% 
  group_by(c_code_2011) %>% 
  summarize(coverage_all = mean(bird_obs),
            n_cells = n(), 
            n_birds = sum(layer, na.rm=T)) 

# Write
grid_res <- res(grid)[1]*100
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
    summarize(coverage = mean(bird_obs),
              n_birds_ym = sum(layer, na.rm=T))
  dist_ym_coverage$yearmonth <- ym
  
  # Append to Master
  coverage_ym <- rbind(coverage_ym, dist_ym_coverage)
}

# Write
write.csv(coverage_ym, 
          paste(save_path_head, 'data/csv/coverage_ym_grid_', grid_res,'km.csv', sep=""),
          row.names = F)