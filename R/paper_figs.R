# PROJECT: Deforestation and Biodiversity
# PURPOSE: Paper Plots
# AUTHOR: Raahil Madhok

# Directories
rm(list=ls())
READ <- '/Volumes/Backup Plus 1/research/data/def_biodiv/'
SAVE <- '/Users/rmadhok/Dropbox/def_biodiv/'
SHP <- '/Users/rmadhok/Dropbox/IndiaPowerPlant/data/'

# Load Packages
require(tidyverse)
require(sf)
require(terra)
require(patchwork)
require(showtext)
require(scattermore)
require(ggridges)
require(hrbrthemes)
require(viridis)
require(units)
font_add('latex', 'latinmodern-math.otf')
showtext_auto()

# Load maps
setwd(SHP)
india_dist <- st_read('./maps/district2011/SDE_DATA_IN_F7DSTRBND_2011.shp') %>% dplyr::select('c_code_11')
india <- st_read('./maps/district2011/SDE_DATA_IN_F7DSTRBND_2011_NATION.shp') %>% dplyr::select('geometry')

#----------------------------------------------------------------
## FUNCTIONS
#----------------------------------------------------------------
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
# 0. GLOBAL BIODIVERSITY
#------------------------------
setwd(READ)
r <- rast('./biomes/BiodiversityMapping/Birds/Richness_10km_Birds_v7_EckertIV_no_seabirds.tif')
r_df <- as.data.frame(r, xy=T)
names(r_df)[3] <- 'biodiversity'

# plot
bd <- ggplot() + 
  geom_sf() +
  geom_tile(data=r_df, mapping = aes(x = x, y = y, fill = biodiversity)) + 
  coord_sf(datum = NA) +
  scale_fill_viridis_c(name='Bird Species (10km x 10km)', 
                       option='inferno', 
                       direction=-1,
                       guide=guide_colourbar(title.position='top')) +
  theme_minimal() +
  theme(text = element_text(family = 'latex'),
        plot.title = element_text(size = 20, face = "bold"),
        plot.margin=grid::unit(c(0,0,0,0), "mm"),
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position='bottom',
        legend.key.width=unit(1.3,'cm'),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13))
setwd(SAVE)
ggsave('./docs/jmp/tex_doc/v3/fig/global_biodiversity.png')

#------------------------------
# 1. TREE COVER
#------------------------------
# Read raster
setwd(READ)
r <- rast('./forest_cover/MOD44B.006_Percent_Tree_Cover_doy2015065_aid0001.tif')

# Aggregate
r[r > 100] <- NA
r_a <- aggregate(r, fact = 10) # aggregate
r_df <- as.data.frame(r_a, xy = T) # dataframe

# Merge
r_df$c_code_2011 <- st_join(st_as_sf(r_df, 
                                     coords=c('x', 'y'), 
                                     crs=4326), 
                            india_dist,
                            join=st_intersects)$c_code_11
r_df <- filter(r_df, !is.na(c_code_2011))
names(r_df)[3] <- 'tree_cover'

# Plot
tree <- ggplot() + 
  geom_sf(data=india, alpha = 0.5, size = 0.3) +
  geom_tile(data=r_df, 
            mapping = aes(x = x, y = y, fill = tree_cover)) + 
  coord_sf(datum = NA) +
  scale_fill_distiller(palette='Greens', 
                       name='2015 Forest Cover (%)',
                       trans='reverse',
                       guide=guide_colourbar(title.position='top',
                                             reverse = TRUE)) +
  theme_minimal() +
  theme(text = element_text(family = 'latex'),
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position='bottom',
        legend.key.width=unit(1.3,'cm'),
        legend.text=element_text(size=15),
        legend.title=element_text(size=20))
setwd(SAVE)
ggsave('./docs/jmp/tex_doc/v3/fig/forest_cover_2015.png')

#------------------------------
# 2. PROJECTS
#------------------------------

# Read csv
setwd(SAVE)
fi <- read_csv('./data/csv/fc_dym_s2_v02.csv') %>%
  filter(year > 2014)

# Bins
proj <- fi %>%
  group_by(c_code_2011) %>%
  summarize(n_proj = last(n_proj_cum)) %>%
  mutate(cut = cut(n_proj, breaks=c(0, 1, 20, 40, 60, 80, 100, 200), 
                   labels=c('0', '1-20', '20-40', '40-60', '60-80','80-100', '100+'),
                   right=F,
                   include.lowest=T)) %>%
  rename(c_code_11=c_code_2011)

# Merge map
proj_map <- left_join(india_dist, proj, by='c_code_11')

# Plot
proj_plot <- ggplot() +
  geom_sf(data=india, alpha = 0.5, size = 0.3) +
  geom_sf(data=proj_map, aes(fill = cut), 
          alpha = 0.8, colour = 'gray69', size = 0.1) +
  scale_fill_viridis_d(name='Number of Projects (2015-2020)', 
                       option='inferno', 
                       direction=-1,
                       guide=guide_legend(title.position='top')) +
  coord_sf(datum = NA) +
  theme(text = element_text(family='latex'),
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position='bottom',
        legend.key.width=unit(1.3,'cm'),
        legend.text=element_text(size=15),
        legend.title=element_text(size=20)) 
ggsave('./docs/jmp/tex_doc/v3/fig/project_map.png')

#------------------------------
# 3. Birdwatching Activity
#------------------------------

## Plot Trip Points
setwd(READ)
trips <- read_csv('./ebird/ebird_trip.csv') %>%
  dplyr::select(lat, lon)

trip_pts <- ggplot() +
  geom_sf(data=india, fill='black') +
  geom_scattermore(data=trips, 
                   aes(x=lon,y=lat, 
                       fill='eBird Trip Location'), 
                   color='red1') +
  coord_sf(datum = NA) +
  labs(fill=' ') +
  theme(text = element_text(family='latex'),
        panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.position='bottom',
        legend.key=element_blank(),
        legend.text=element_text(size=30))
setwd(SAVE)
ggsave('./docs/jmp/tex_doc/v3/fig/trip_map.png')

## Plot Trips per District

# Read user data
setwd(SAVE)
bird <- read_csv('./data/csv/ebird_udt.csv') %>%
  filter(between(year, 2015, 2020))

# Z-score of trips
n_trips <- bird %>%
  group_by(c_code_2011) %>%
  summarize(n_trips = sum(n_trips, na.rm=T)) %>%
  rename(c_code_11=c_code_2011)

# Map Join
bird_map <- left_join(india_dist, n_trips, by='c_code_11')
bird_map$n_trips[is.na(bird_map$n_trips) & !is.na(bird_map$c_code_11)] <- 0
bird_map <- bird_map %>%
  mutate(cut = cut(n_trips, breaks=c(0, 1, 100, 200, 500, 1000, 5000, 65000), 
                   labels=c('0', '1-100', '100-200', '200-500', '500-1000','1000-5000', '5000+'),
                   right=F,
                   include.lowest=T))

# Plot
trip_plot <- ggplot() +
  geom_sf(data=india, alpha = 0.5, size = 0.3) +
  geom_sf(data=bird_map, aes(fill = cut), 
          alpha = 0.8, colour = 'gray69', size = 0.1) +
  scale_fill_viridis_d(name='Number of eBird Trips (2015-2020)', 
                       option='inferno', 
                       direction=-1,
                       guide=guide_legend(title.position='top')) +
  coord_sf(datum = NA) +
  theme(text = element_text(family='latex'),
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position='bottom',
        legend.key.width=unit(1.3,'cm'),
        legend.text=element_text(size=15),
        legend.title=element_text(size=20)) 
ggsave('./docs/jmp/tex_doc/v3/fig/trip_district_map.png')

#-------------
# FINAL PLOT
#-------------
p <- tree + plot_spacer() + proj_plot + plot_spacer() + trip_pts + 
  plot_layout(widths=c(4,1,4,1,4))
setwd(SAVE)
ggsave('./docs/jmp/tex_doc/v3/fig/summary_maps.png')

#-------------------------------------------
# 4. Distance b/w real and imputed homes
#-------------------------------------------

setwd(SAVE)
real_home <- read_csv('./data/csv/user_home_real.csv')
imputed_home <- read_csv('./data/csv/user_home_impute.csv')

# Construct sample
homes <- merge(real_home, imputed_home, by='user_id')

# Offset
homes$offset <- st_distance(st_as_sf(homes, 
                                     coords = c('lon_home_real', 'lat_home_real'),
                                     crs=4326), 
                            st_as_sf(homes, 
                                     coords = c('lon_home', 'lat_home'),
                                     crs=4326), 
                            by_element = T) %>% set_units(km)
homes$offset <- as.numeric(homes$offset)

# Plot
ggplot(homes, aes(x=offset)) + 
  stat_bin(aes(y=..count..), color='black', fill='steelblue3') +
  ylab('Number of Users\n') +
  geom_vline(aes(xintercept = median(offset)), col='red') +
  annotate(x=200,y=90,label="Median = 16.97 km", 
           vjust=2, family='latex', geom="label") +
  scale_x_continuous(name='\nDistance Between Actual and Imputed Home (km)', 
                     breaks = scales::pretty_breaks(n = 15)) +
  theme_minimal() +
  theme(text = element_text(family='latex', size=15),
        axis.line = element_blank(), 
        axis.ticks = element_blank())
setwd(SAVE)
ggsave('./docs/jmp/tex_doc/v3/fig/distance_home_offset.png')

#-------------------------------------------
# 5. RIGELINE CHART - FRA COMPLIANCE
#-------------------------------------------
setwd(SAVE)

# Prep data
fc <- read_csv('./data/csv/fc_dym_s2_v02.csv') %>%
  group_by(c_code_2011) %>%
  summarize(n_proj = last(n_proj_cum))
fra <- haven::read_dta('./data/dta/iv_fra.dta') %>%
  dplyr::select(c_code_2011, st_ha)
st <- haven::read_dta('./data/dta/2011_india_dist.dta') %>%
  mutate(st_share = tot_st/tot_pop) %>%
  dplyr::select(c_code_2011, st_share)

df <- left_join(fc, fra, by='c_code_2011') %>%
  left_join(st, by='c_code_2011') %>%
  mutate(decile = as.factor(ntile(n_proj, 10)))
  
# Ridgeline
ggplot(df, aes(x = st_share, y = decile, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_distiller(palette='Blues', 
                       trans='reverse') +
  labs(title = 'B: Project Placement and ST Population\n',
       y = 'Projects in District (Decile)\n',
       x = '\nST Population Share') +
  scale_x_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  scale_y_discrete(breaks=c(2,4,6,8,10)) +
  theme_minimal() +
  theme(legend.position="none",
        text = element_text(family='latex', size=20),
        axis.line = element_blank(), 
        panel.background = element_blank(), 
        plot.title = element_text(hjust = 0.5))
ggsave('./docs/jmp/tex_doc/v3/fig/placement_st.png')

# histogram of informed consent
fra_proj <- haven::read_dta('./data/dta/fc_pdym_s2_v02.dta') %>%
  group_by(c_code_2011) %>%
  summarize(fra = sum(proj_fra_num, na.rm=T),
            n_proj=n()) %>%
  mutate(fra_share = fra/n_proj)

ggplot(fra_proj, aes(x=fra_share)) + 
  ggtitle('A: Informed Consent\n') +
  stat_bin(aes(y=..count../sum(..count..)), color='black', fill='steelblue3') +
  labs(x = '\nProject Share Obtaining Gram Sabha Consent',
       y = 'Share of Districts\n') +
  geom_vline(aes(xintercept = mean(fra_share)), col='red', linetype = 'dashed') +
  annotate(x=0.6,y=0.2,label="Mean = 0.48", 
           vjust=2, family='latex', geom="label") +
  scale_x_continuous(breaks = c(0, .2, .4, .6, .8, 1.0)) +
  scale_y_continuous(breaks = c(0, .1, .2, .3)) +
  theme_minimal() +
  theme(text = element_text(family='latex', size=20),
        axis.line = element_blank(), 
        panel.background = element_blank())
ggsave('./docs/jmp/tex_doc/v3/fig/hist_fra.png')


