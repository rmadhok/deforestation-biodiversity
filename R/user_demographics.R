#-------------------------------------------------------------
# PROJECT: Deforestation and Biodiversity
# PURPOSE: User Demographics
# AUTHOR: Raahil Madhok
#-------------------------------------------------------------

### SET-UP
# Directories
rm(list=ls())
READ <- '/Volumes/Backup Plus 1/research/data'
SAVE <- '/Users/rmadhok/Dropbox/def_biodiv/'
SHP <- '/Users/rmadhok/Dropbox/IndiaPowerPlant/data/'
setwd(READ)

# Load Packages
require(data.table)
require(tidyverse)
require(sf)
require(raster)
require(exactextractr)
require(showtext)
font_add('latex', 'latinmodern-math.otf')
showtext_auto()

#-------------------------------------------------------------
# Num homes per cell
#-------------------------------------------------------------

# Load District Map
india_dist <- st_read(paste(SHP, "maps/district2011", sep=""), 
                      'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

# Load State Map
india_st <- st_read(paste(SHP, "maps/district2011", sep=""), 
                      'SDE_DATA_IN_F7DSTRBND_2011_STATES', stringsAsFactors = F)

# Analysis sample users (n=17,634 users)
home <- merge(distinct(read_csv('./def_biodiv/ebird/ebird_trip.csv'), user_id),
              read_csv(paste(SAVE, 'data/csv/user_home_impute.csv', sep='')),
              by='user_id')

# Number of homes per cell
grid <- raster(extent(india_dist))
res(grid) <- .2
crs(grid) <- crs(india_dist)
home_grid <- rasterize(home[, c('lon_home', 'lat_home')], grid, fun='count')
home_grid[is.na(home_grid)] <- 0 
home_grid <- mask(home_grid, india_dist)
  
# Plot
df <- as.data.frame(home_grid, xy = T) %>%
  filter(!is.na(layer))
df$cuts <- cut(df$layer, breaks=c(0, 1, 20, 40, 60, 80, 100, 450), 
               labels=c('0', '1-20', '20-40', '40-60', '60-80', '80-100', '100+'),
               right=F,
               include.lowest=T)

ebd_pop_plot <- ggplot() + 
  ggtitle("B") +
  geom_tile(data=df, mapping = aes(x = x, y = y, fill = cuts)) + 
  coord_sf(datum = NA) +
  scale_fill_viridis_d(name='User Homes',
                       direction=-1,
                       guide=guide_legend(title.position='top')) +
  theme_minimal() +
  theme(text = element_text(family = 'latex'),
        plot.title = element_text(size = 20, face = "bold"),
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position='bottom',
        legend.key.width=unit(1.3,'cm'),
        legend.text=element_text(size=15),
        legend.title=element_text(size=20)) 
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
df <- as.data.frame(r_ag, xy = T) %>%
  filter(!is.na(ppp_2015_1km_Aggregated))
df$cuts <- cut(df$ppp_2015_1km_Aggregated, breaks=c(0, 1000, 200000, 400000, 600000, 800000, 1000000, 8000000), 
               labels=c('0-1','1-200', '200-400', '400-600', '600-800', '800-1000', '1000+'),
               right=F,
               include.lowest=T)

wpop_plot <- ggplot() + 
  ggtitle("A") +
  geom_tile(data=df, mapping = aes(x = x, y = y, fill = cuts)) + 
  coord_sf(datum = NA) +
  scale_fill_viridis_d(name='2015 Population Density (Thousands)',
                       direction=-1,
                       guide=guide_legend(title.position='top')) +
  theme_minimal() +
  theme(text = element_text(family = 'latex'),
        plot.title = element_text(size = 20, face = "bold"),
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = 'bottom',
        legend.key.width=unit(1.3,'cm'),
        legend.text=element_text(size=15),
        legend.title=element_text(size=20)) 
ggsave(paste(SAVE, '/docs/jmp/tex_doc/v3/fig/pop_density_2015.png', sep=''))

#-------------------------------------------------------------
# % of Population in cities
#-------------------------------------------------------------
# Construct city shapes (buffer + dissolve)
{
# Read city shp
city <- st_read('./def_biodiv/grump', 'global_urban_extent_polygons_v1.01', 
                stringsAsFactors = F)
city <- st_make_valid(city) # fast
city <- st_crop(city, india_dist)

## 3km buffer (i.e. include metro areas)
# cities w boundaries < 3km apart are dissolved
city_buf <- st_as_sf(st_cast(st_union(st_buffer(city, 0.03)), 'POLYGON'))
}

# Read buffered cities
city_buf <- st_read('./def_biodiv/grump/india_buffer/city_buffer.shp', 
                    stringsAsFactors = F)

# City Population
city_pop <- cbind(city_buf, exact_extract(r, city_buf, 'sum'))
names(city_pop)[2] <- 'population'

# Get city of user home
user_city <- st_join(st_as_sf(home, 
                              coords=c('lon_home', 'lat_home'),
                              crs=4326),
                     city_pop, 
                     join = st_intersects) %>% st_drop_geometry()

# users in cities
user_stats <- user_city %>%
  mutate(city = ifelse(population > 1000000, 1, 0)) %>%
  group_by(city) %>%
  summarize(city_users = n())
# 7566/17634 = 43% of users live in cities > 1million

# city stats
city_stats <- city_pop %>%
  st_drop_geometry() %>%
  mutate(city = ifelse(population > 1000000, 1, 0)) %>%
  group_by(city) %>%
  summarize(population = sum(population, na.rm=T))
# 348145894/1298712620 (27%) of ppl live in cities > 1 mil pop
# sum of population raster cells = 1298712620
