# PROJECT: Deforestation and Biodiversity
# PURPOSE: Prepare eBird for Analysis
# AUTHOR: Raahil Madhok
# DATE: Dec 26, 2018 [created]

### SET-UP
# Directories
read_path_head <- '/Volumes/Backup Plus/research/def_biodiv/ebird'
save_path_head <- '/Users/rmadhok/Documents/ubc/research/def_biodiv/'
dist_shpf <- '/Users/rmadhok/Dropbox (CID)/IndiaPowerPlant/data/'
setwd(read_path_head)

# Load Packages
require(data.table)
require(rgdal)
require(rgeos)
require(sp)
require(raster)
require(splitstackshape)
require(dplyr)
require(geosphere)
require(ggplot2)

# Read Custom Functions
source('/Users/rmadhok/Documents/ubc/research/def_biodiv/scripts/R/ebird_functions.R')

## 1. LOAD DATA -------------------------------

# Load 2011 District Map
india.districts.2011 <- readOGR(paste(dist_shpf, "maps/india-district", sep=""),
                                'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

# Write Attribute Data
dist.data <- india.districts.2011@data
write.csv(dist.data,
          paste(save_path_head,'data/csv/2011_india_dist.csv', sep=''),
          row.names = F)

# Load eBird (n=13,481,001)
ebird <- fread('ebd_IN_201401_201904_relApr-2019.txt')
colnames(ebird) <- make.names(colnames(ebird))


## 2. ATTACH DISTRICT CENSUS CODES -------------

# Keep 2015-2019
ebird$YEAR <- as.numeric(strftime(ebird$OBSERVATION.DATE, format = "%Y"))
ebird <- ebird[ebird$YEAR > 2014, ]
ebird$YEARMONTH <- strftime(ebird$OBSERVATION.DATE, format = "%Y-%m")

# Overlay District Code
proj <- proj4string(india.districts.2011)
ebird$c_code_2011 <- over(SpatialPoints(ebird[, 27:26], proj4string = CRS(proj)), india.districts.2011)$c_code_11
ebird <- ebird[!is.na(ebird$c_code_2011), ]

# Drop unnecessary columns
ebird <- ebird[,-c(1,2,10,11,14,16,18,19,20,21,22,24,29,34,41,42,43,44,45,46,47)]
# n = 12,776,073

## 3. FILTER DATA --------------------------------

# Duration
# ebird <- subset(ebird, DURATION.MINUTES >= 5 & DURATION.MINUTES <= 240)

# All Species Reported 
ebird <- ebird[ebird$ALL.SPECIES.REPORTED == 1, ]
# n=12,222,092

# Protocol -- keep stationary, travelling, historical, banding, random
ebird <- subset(ebird, PROTOCOL.CODE %in% c('P21', 'P22', 'P62', 'P33', 'P48'))
# n=12,192,547

# Drop Duplicates (probably a more efficient way)
ebird$GROUP.IDENTIFIER[ebird$GROUP.IDENTIFIER == ''] <- NA
ebird.gid.miss <- ebird[is.na(ebird$GROUP.IDENTIFIER), ]
ebird.gid.nonmiss <- ebird[!is.na(ebird$GROUP.IDENTIFIER), ] 
ebird.dd <- distinct(ebird.gid.nonmiss, GROUP.IDENTIFIER, TAXONOMIC.ORDER, .keep_all = T)
ebird <- rbind(ebird.gid.miss, ebird.dd)
rm(list=c('ebird.dd', 'ebird.gid.miss', 'ebird.gid.nonmiss'))
# n=8,433,613

# Keep Veterans
ebird <- ebird %>%
  group_by(YEAR, OBSERVER.ID) %>%
  mutate(n_months = n_distinct(YEARMONTH))

ebird.vet1 <-  ebird[ebird$YEAR < 2019 & ebird$n_months >= 5 , ]
ebird.vet2 <- ebird[ebird$YEAR == 2019 & ebird$n_months >= 2, ]
ebird <- rbind(ebird.vet1, ebird.vet2)
rm(list=c('ebird.vet1', 'ebird.vet2'))
#n=7,052,672 (3153 users)

# Stats
ebird <- ebird %>% 
  group_by(c_code_2011, YEARMONTH) %>% 
  mutate(n_birders = n_distinct(OBSERVER.ID))

## 4. DIVERSITY INDICES -----------------------------

# Species Richness
ebird$OBSERVATION.COUNT[ebird$OBSERVATION.COUNT == 'X'] <- NA
ebird <- ebird %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  mutate(species_richness = n())

# Shannon Index
ebird$OBSERVATION.COUNT <- as.numeric(ebird$OBSERVATION.COUNT)
ebird <- ebird %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  mutate(shannon_index = -sum((OBSERVATION.COUNT / sum(OBSERVATION.COUNT, na.rm = T))*
                                log(OBSERVATION.COUNT / sum(OBSERVATION.COUNT, na.rm = T)), 
                              na.rm = T))

# Simpson Index
ebird <- ebird %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>%
  mutate(simpson_index = 1 - ( (sum(OBSERVATION.COUNT*(OBSERVATION.COUNT - 1), na.rm = T)) /
                                 (sum(OBSERVATION.COUNT, na.rm = T)*(sum(OBSERVATION.COUNT, na.rm = T) - 1)) ))

# Plot
ggplot() +
  geom_polygon(data = india.districts.2011, aes(x=long,y=lat,group=group),fill= 'white', color = 'grey', size=.5) + 
  geom_point(data=ebird, aes(x=LONGITUDE,y=LATITUDE), color='red', alpha=.03) +
  coord_equal() + theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),  
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(),  
        axis.title.y = element_blank(),
        legend.position = 'bottom',
        legend.title=element_blank())
ggsave(paste(save_path_head, 'docs/tex_doc/fig/ebird_all.png', sep=''), width=8,height=6)

## 5. AGGREGATE TO DISTRICT-MONTH -----------------------------

# Sampling Event Level
ebird.full <- distinct(ebird, SAMPLING.EVENT.IDENTIFIER, .keep_all = T)

# Save - for making plots
write.csv(ebird.full,
          paste(save_path_head, 'data/csv/ebird_triplevel.csv', sep=""),
          row.names = F)
# n = 508,185 trips

# Aggregate
ebird.full <- ebird.full %>%
  group_by(c_code_2011, YEARMONTH) %>%
  summarize(species_richness_mean = mean(species_richness, na.rm = T),
            species_richness_md = median(species_richness, na.rm = T),
            shannon_index_mean = mean(shannon_index, na.rm = T),
            shannon_index_md = median(shannon_index, na.rm = T),
            simpson_index_mean = mean(simpson_index, na.rm = T),
            simpson_index_md = median(simpson_index, na.rm = T),
            duration_min_mean = mean(DURATION.MINUTES, na.rm = T),
            duration_min_md = median(DURATION.MINUTES, na.rm = T),
            effort_distance_km_mean = mean(EFFORT.DISTANCE.KM, na.rm = T),
            effort_distance_km_md = median(EFFORT.DISTANCE.KM, na.rm = T),
            n_birders = first(n_birders),
            n_trips = n())

# Write
write.csv(ebird.full,
          paste(save_path_head, 'data/csv/ebird_full.csv', sep=""),
          row.names = F)
rm(list='ebird.full')

## 6. SPATIAL COVERAGE ------------------------------------

# Initialize Grid
grid <- raster(extent(india.districts.2011))
res(grid) <- .08 # grid resolution in degrees lat-lon
proj4string(grid) <- proj4string(india.districts.2011) # sync coordinate systems 

# Cell Counts of Birds
bird.coords <- SpatialPoints(ebird[, 15:14], proj4string = CRS(proj4string(grid)))
count.df <- spatial_coverage(bird.coords, grid)

# Aggregate to District
coverage.all <- count.df %>% 
  group_by(c_code_2011) %>% 
  summarise(coverage_all = mean(bird.obs), 
            n_cells = n(), 
            n_birds = sum(layer, na.rm=T)) 

# Write
grid.res <- res(grid)[1]*100
write.csv(coverage.all, 
          paste(save_path_head, 'data/csv/coverage_dist_grid_', grid.res,'km.csv', sep=""),
          row.names = F)

# Compute Monthly Spatial Coverage
ebird$YEARMONTH <- as.factor(ebird$YEARMONTH)
coverage.ym <- data.frame()
for(ym in levels(ebird$YEARMONTH)){
  
  print(paste('Computing Spatial Coverage of Date:', ym))
  
  # Get Bird Coordinates in year-month
  bird.coords <- SpatialPoints(ebird[ebird$YEARMONTH == ym, 15:14], 
                               proj4string=CRS(proj4string(grid)))
  
  # Run My Coverage Function
  count.df <- spatial_coverage(bird.coords, grid)
  
  # Aggregate to District
  dist.ym.coverage <- count.df %>%
    group_by(c_code_2011) %>%
    summarise(coverage = mean(bird.obs),
              n_birds_ym = sum(layer, na.rm=T))
  dist.ym.coverage$yearmonth <- ym
  
  # Append to Master
  coverage.ym <- rbind(coverage.ym, dist.ym.coverage)
}

# Write
write.csv(coverage.ym, 
          paste(save_path_head, 'data/csv/coverage_ym_grid_', grid.res,'km.csv', sep=""),
          row.names = F)