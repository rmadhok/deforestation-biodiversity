# PROJECT: Deforestation and Biodiversity
# PURPOSE: Paper Plots
# AUTHOR: Raahil Madho
########################################################################
# Directories
read_path <- '/Volumes/Backup Plus/research/def_biodiv/forest_cover/'
save_path <- '/Users/rmadhok/Dropbox/def_biodiv/'
dist_shp <- '/Users/rmadhok/Dropbox/IndiaPowerPlant/data/'

# Load Packages
require(tidyverse)
require(sf)
require(terra)
require(patchwork)
########################################################################

## FUNCTIONS ###

# Truncate
truncate <- function(x, num, top=FALSE) {
  if(isFALSE(top)){
    x = ifelse(x == Inf | x>quantile(x,1-num, na.rm=T) | 
                 x<quantile(x, num, na.rm=T), NA, x)
  }else{
    x = ifelse(x == Inf | x>quantile(x,1-num, na.rm=T), NA, x)
  }
}

# Winsorize
winsorize <- function(x, num, top=FALSE) {
  top <- quantile(x, 1-num, na.rm=T)
  bottom <- quantile(x, num, na.rm=T)
  if(isFALSE(top)){
    x = case_when(
      x > bottom & x < top ~x,
      x >=top ~ top,
      x <= bottom ~ bottom)
  } else{
    x = case_when(
      x >=top ~ top,
      x < top ~ x)
  }
}
#------------------------------
# 1. TREE COVER
#------------------------------

# Read raster
r <- rast(paste(read_path, 
                'mosaic/MOD44B.006_Percent_Tree_Cover_doy2014065_aid0001.tif', 
                sep=''))

# Aggregate
r_a <- terra::aggregate(r, fact = 10)

# Data frame
r_a_df <- as.data.frame(r_a, xy = T)

# Spatial join
map <- st_read(paste(dist_shp, 'maps/district2011', sep=''),'SDE_DATA_IN_F7DSTRBND_2011')
r_a_df$c_code_2011 <- st_join(st_as_sf(r_a_df, coords=c('x', 'y'), crs=4326), 
                              map,
                              join=st_intersects)$c_code_11
r_a_df <- filter(r_a_df, !is.na(c_code_2011))

# Clean
r_a_df <- r_a_df %>%
  rename(tree_cover = lyr.1) %>%
  mutate(tree_cover=replace(tree_cover,tree_cover>100,NA))

# Plot
ind <- st_read(paste(dist_shp, 'maps/district2011', sep=''),'SDE_DATA_IN_F7DSTRBND_2011_NATION') # Coast
tree <- ggplot() + 
  ggtitle("A") +
  geom_sf(data=ind, alpha = 0.5, size = 0.3) +
  geom_tile(data=r_a_df, mapping = aes(x = x, y = y, fill = tree_cover)) + 
  coord_sf(datum = NA) +
  scale_fill_distiller(palette='Greens', 
                       name='Tree Cover (%)',
                       trans='reverse',
                       guide=guide_colourbar(title.position='top',
                                             reverse = TRUE)) +
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
ggsave(paste(save_path, '/docs/jmp/tex_doc/v2/fig/fcover_plot.png', sep=''))

#------------------------------
# 2. PROJECTS
#------------------------------

# Read forest infrastructure
fi <- read_csv(paste(save_path, 'data/csv/fc_dist_ym_stage2.csv', sep=''))

# Approvals (2015-18)
proj <- fi %>%
  group_by(c_code_2011) %>%
  summarize(proj = sum(n_patches, na.rm=T)) %>%
  mutate(cut = cut(proj, breaks=c(0, 1, 20, 40, 60, 80, 100), 
                   labels=c('0', '1-20', '20-40', '40-60', '60-80', '80+'),
                   right=F,
                   include.lowest=T)) %>%
  rename(c_code_11=c_code_2011)

# Map Join
proj_map <- left_join(map[,'c_code_11'], proj, by='c_code_11')

# Plot
proj_plot <- ggplot() +
  ggtitle("B") +
  geom_sf(data=ind, alpha = 0.5, size = 0.3) +
  geom_sf(data=proj_map, aes(fill = cut), 
          alpha = 0.8, colour = 'white', size = 0.1) +
  scale_fill_viridis_d(name='Num. Projects', 
                       option='inferno', 
                       direction=-1,
                       guide=guide_legend(title.position='top')) +
  coord_sf(datum = NA) +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position='bottom',
        legend.key.width=unit(1.8,'cm'),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13)) 
ggsave(paste(save_path, '/docs/jmp/tex_doc/v2/fig/proj_plot.png', sep=''))

#------------------------------
# 3. Birdwatching Activity
#------------------------------

# Read user data
bird <- read_csv(paste(save_path, 'data/csv/ebird_user.csv', sep='')) %>%
  filter(between(year, 2015, 2018))

# Z-score of trips
trips <- bird %>%
  group_by(c_code_2011) %>%
  summarize(n_trips = sum(n_trips, na.rm=T)) %>%
  mutate(n_trips_w = winsorize(n_trips, num=0.05),
         z_trips = (n_trips_w - mean(n_trips_w))/sd(n_trips_w)) %>%
  rename(c_code_11 = c_code_2011)

# Map Join
bird_map <- left_join(map[,'c_code_11'], trips, by='c_code_11')

# Plot
trip_plot <- ggplot() +
  ggtitle("C") +
  geom_sf(data=ind, alpha = 0.5, size = 0.1, colour='black') +
  geom_sf(data=bird_map, aes(fill = z_trips), 
          alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_gradient2(low = 'blue', mid='grey94', high = 'red', 
                       guide=guide_colourbar(
                         title='Num. Trips (z-score)',
                         title.position='top',
                         limits=c(-1,3))) +
  coord_sf(datum = NA) +
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
ggsave(paste(save_path, '/docs/jmp/tex_doc/v2/fig/trip_plot.png', sep=''))
