#-------------------------------------------------------------
# PROJECT: Deforestation and Biodiversity
# PURPOSE: User Demographics
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
require(data.table)
require(tidyverse)
require(sf)
require(raster)
require(exactextractr)

#-------------------------------------------------------------
# Num Trips per cell
#-------------------------------------------------------------

# Load District Map
india_dist <- st_read(paste(SHP, "maps/district2011", sep=""), 
                      'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

# Load State Map
india_st <- st_read(paste(SHP, "maps/district2011", sep=""), 
                      'SDE_DATA_IN_F7DSTRBND_2011_STATES', stringsAsFactors = F)

## Impute Home coordinates (DEFUNCT)
# - This is based on centre of stationary/travelling trips
{
# Read trip level data
ebird_trip <- read_csv('./ebird/ebird_trip.csv')

# Imputed Home --------------------------------------
# 1. Estimate gravitational center of all trips
# 2. Compute distance from "home" to each trip
# 3. Remove outliers (e.g. faraway trips - by plane)
# 4. Recompute "home" from remaining trips

# Center of user's trips
home <- distinct(ebird_trip, OBSERVER.ID, LONGITUDE, LATITUDE) %>%
  group_by(OBSERVER.ID) %>%
  mutate(lon_home = mean(LONGITUDE, na.rm=T),
         lat_home = mean(LATITUDE, na.rm=T)) %>% ungroup()

# straight-line distance from center to each site
home$distance <- st_distance(st_as_sf(home, 
                                      coords = c('lon_home', 'lat_home'),
                                      crs=4326), 
                             st_as_sf(home, 
                                      coords = c('LONGITUDE', 'LATITUDE'),
                                      crs=4326), 
                             by_element = T)
home$distance <- as.numeric(home$distance)/1000

# Remove outlier trips
home <- home %>%
  group_by(OBSERVER.ID) %>%
  filter(distance <= quantile(distance, 0.75) + 1.5*IQR(distance))

# Recompute home (n=16,908)
home <- home %>%
  group_by(OBSERVER.ID) %>%
  summarize(lon_home=mean(LONGITUDE, na.rm=T),
            lat_home=mean(LATITUDE, na.rm=T))

# Overlay home districts
# -------------------------------------
# Note: assign off-coast homes centroid 
# of nearest district
#--------------------------------------
home$c_code_2011_home <- st_join(st_as_sf(home, 
                                          coords=c('lon_home', 'lat_home'), 
                                          crs=4326), 
                                 india_dist, 
                                 join = st_intersects)$c_code_11
# Handle Out-of-bounds homes --- FOR NOW, DROP OFF-COAST HOMES
home <- filter(home, !is.na(c_code_2011_home))

# Save home list
write_csv(home, paste(SAVE, 'data/csv/user_home_impute.csv', sep=''))
}

## Home Coordinates of sample

# Analysis sample users (n=16,908 users)
home <- merge(distinct(read_csv('./def_biodiv/ebird/ebird_trip.csv'), OBSERVER.ID),
              read_csv('./ebird_wtp/user_home_impute.csv'),
              by.x = 'OBSERVER.ID', by.y = 'observer_id')

# Number of homes per cell
grid <- raster(extent(india_dist))
res(grid) <- .2
crs(grid) <- crs(india_dist)
home_grid <- rasterize(home[, c('lon_home', 'lat_home')], grid, fun='count')
home_grid[is.na(home_grid)] <- 0 
home_grid <- mask(home_grid, india_dist)

# Plot
df <- as.data.frame(home_grid, xy = T)
df$cuts <- cut(df$layer, breaks=c(0, 1, 20, 40, 60, 80, 100, 450), 
               labels=c('0', '1-20', '20-40', '40-60', '60-80', '80-100', '100+'),
               right=F,
               include.lowest=T)

ebd_pop_plot <- ggplot() + 
  ggtitle("A") +
  geom_tile(data=df, mapping = aes(x = x, y = y, fill = cuts)) + 
  #geom_sf(data=india_st, alpha = 0.1, size = 0.05) +
  coord_sf(datum = NA) +
  scale_fill_viridis_d(name='Home Locations',
                       direction=-1,
                       guide=guide_legend(title.position='top')) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position='bottom',
        legend.key.width=unit(1.3,'cm'),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13)) 
ggsave(paste(SAVE, '/docs/jmp/tex_doc/v3/fig/home_density.png', sep=''))

#-------------------------------------------------------------
# India Population Density
#-------------------------------------------------------------

# Read worldpop global mosaic
r <- raster('./def_biodiv/worldpop/ppp_2015_1km_Aggregated.tiff')
r <- crop(r, extent(india_dist))
r <- mask(r, india_dist)
r_ag <- raster::aggregate(r, fact = 20, fun=sum) # population per 20km cell

# Plot
df <- as.data.frame(r_ag, xy = T)
df$cuts <- cut(df$ppp_2015_1km_Aggregated, breaks=c(0, 1000, 200000, 400000, 600000, 800000, 1000000, 8000000), 
               labels=c('0-1','1-200', '200-400', '400-600', '600-800', '800-1000', '1000+'),
               right=F,
               include.lowest=T)

wpop_plot <- ggplot() + 
  ggtitle("B") +
  #geom_sf(data=india_st, alpha = 0.1, size = 0.05) +
  geom_tile(data=df, mapping = aes(x = x, y = y, fill = cuts)) + 
  coord_sf(datum = NA) +
  scale_fill_viridis_d(name='2015 Population Density (Thousands)',
                       direction=-1,
                       guide=guide_legend(title.position='top')) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position='bottom',
        legend.key.width=unit(1.3,'cm'),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13)) 
ggsave(paste(SAVE, '/docs/jmp/tex_doc/v3/fig/pop_density_2015.png', sep=''))

#-------------------------------------------------------------
# Density Comparison
#-------------------------------------------------------------

# Convert home grid to polygon (20km^2 cells)
home_sf <- st_as_sf(rasterToPolygons(home_grid))

# Extract population over home grid 
pd <- cbind(st_drop_geometry(home_sf), exact_extract(r, home_sf, 'sum'))
names(pd) <- c('user_homes', 'pd_2015')

# Plot - population per cell vs. users per cell
upd <- ggplot(pd, aes(x=pd_2015/1000000, y=user_homes)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  ylab('User Density \n') +
  xlab('\nPopulation Density (millions)') +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), 
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        text = element_text(size=17))
ggsave(paste(SAVE, '/docs/jmp/tex_doc/v3/fig/user_pd_2015.png', sep=''))

#-------------------------------------------------------------
# % of Population in cities (>100,000)
#-------------------------------------------------------------

## ---- Pop. share in cells with pop > 100,000
pd_sum <- pd %>%
  mutate(city = ifelse(pd_2015 >= 100000, 1, 0)) %>%
  group_by(city) %>%
  summarize(city_users = sum(user_homes, na.rm=T),
            city_pop = sum(pd_2015, na.rm=T))
# (13453 users (80%) in cities > 100,000
# these cities host 86% of the population (1119070241 / 1298712620)

## ---- Pop. Share in cities --

# Read city shp
city <- st_read('./def_biodiv/grump', 'global_urban_extent_polygons_v1.01', 
                stringsAsFactors = F)
city <- lwgeom::st_make_valid(city) 
city <- st_crop(city, india_dist)

## 3km buffer (i.e. include metro areas)
# cities w boundaries < 3km apart are dissolved
city_buf <- st_as_sf(st_cast(st_union(st_buffer(city, 0.03)), 'POLYGON'))
city_buf$cityid <- 1:nrow(city_buf)

# City Population
city_pop <- cbind(city_buf, exact_extract(r, city_buf, 'sum'))
names(city_pop)[2] <- 'population'

# Get city of user home
user_city <- st_join(st_as_sf(home, 
                              coords=c('lon_home', 'lat_home'),
                              crs=4326),
                     city_pop, 
                     join = st_intersects) %>% st_drop_geometry()
#user_city <- filter(user_city, !is.na(ID))

# users in cities
user_stats <- user_city %>%
  mutate(city = ifelse(population > 1000000, 1, 0)) %>%
  group_by(city) %>%
  summarize(city_users = n())
# 7168/16908 = 42% of users live in cities > 1million

# city stats
city_stats <- city_pop %>%
  st_drop_geometry() %>%
  mutate(city = ifelse(population > 1000000, 1, 0)) %>%
  group_by(city) %>%
  summarize(population = sum(population, na.rm=T))
# 348145894/1298712620 (27%) of ppl live in cities > 1 mil pop
# sum of population raster cells = 1298712620
