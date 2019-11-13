# PROJECT: Deforestation and Biodiversity
# PURPOSE: Construct eBird Plots
# AUTHOR: Raahil Madhok
# DATE: Sep 3 2019 [created]

# Set Directories
ROOT <- '/Users/rmadhok/Dropbox (Personal)/def_biodiv/'
SHP <- '/Users/rmadhok/Dropbox (Personal)/IndiaPowerPlant/data/'

# Packages
require(tidyverse)
require(sf)
require(viridis)
#--------------------------------------------------------------------------

# Basemap
india_districts <- st_read(paste(SHP, "maps/india-district", sep=""), 
                           'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)
india_districts <- india_districts[india_districts$ID>1,]

# --- 1. Deforestation
defn <- st_read(paste(ROOT, 'data/csv/', sep=''), 
                         'fc_dist_ym', stringsAsFactors = F)

# Deforestation cross-section
defn_year <- defn %>% 
  mutate(district_forest_cum_km2 = as.numeric(district_forest_cum_km2)) %>%
  arrange(c_code_2011, year_month) %>%
  group_by(c_code_2011, year) %>%
  summarize(district_forest_cum_km2 = last(district_forest_cum_km2)) %>%
  filter(year == 2015 | year == 2018) %>%
  rename(c_code_11 = c_code_2011)
  
india_dist_def <- left_join(india_districts, defn_year, by="c_code_11")

# Plot Deforestation by year
ggplot(india_dist_def, aes(fill = district_forest_cum_km2)) +
  geom_sf(alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_viridis(name = "Cumulative Deforestation (km2)",
                     direction = -1,
                     guide = guide_colourbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5)) +
  coord_sf(datum = NA) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  facet_wrap(~year, drop=T)
ggsave(paste(ROOT, 'docs/tex_doc/fig/deforestation.png', sep=''))

# Plot Change in Deforestation between 2015 and 2018
defn_change <- spread(defn_year, year, district_forest_cum_km2)
defn_change$theta_district_forest_cum_km2 <- defn_change$`2018`-defn_change$`2015`
defn_change <- defn_change[,c(1,4)]
india_dist_def <- left_join(india_districts, defn_change, by="c_code_11")

ggplot(india_dist_def, aes(fill = theta_district_forest_cum_km2)) +
  geom_sf(alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_viridis(name = "Cumulative Deforestation (km2)",
                     direction = -1,
                     guide = guide_colourbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5)) +
  coord_sf(datum = NA) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))
ggsave(paste(ROOT, 'docs/tex_doc/fig/theta_deforestation.png', sep=''))
rm(list=c('defn', 'defn_year', 'defn_change'))

# 2. ------ Birding Activity

# Number of trips CDF (year-month)
ebird <- st_read(paste(ROOT, 'data/csv', sep=''),
                   'ebird_all', stringsAsFactors = F)

ebird_trunc <- ebird %>%
  mutate(n_trips = as.numeric(n_trips)) %>%
  filter(n_trips < quantile(n_trips, 0.95))

ggplot(ebird_trunc, aes(n_trips)) +
  stat_ecdf(geom='step') +
  ylab('Cumulative Probability') +
  xlab('Number of Trips') +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(panel.grid.minor = element_blank(), 
        axis.line = element_blank(), 
        axis.ticks = element_blank())
ggsave(paste(ROOT, 'docs/tex_doc/fig/cdf_n_trips.png', sep=''))

# Number of Trips by Month
ebird$month <- as.numeric(substr(ebird$YEARMONTH, 6,7))
ebird_trips_month <- ebird %>%
  group_by(month) %>%
  mutate(n_trips = as.numeric(n_trips)) %>%
  summarize(n_trips = sum(n_trips, na.rm=T))

months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
ggplot(ebird_trips_month, aes(x=month, y=n_trips)) +
  geom_bar(stat="identity", width=0.5) +
  ylab('Number of Trips') +
  scale_x_continuous(breaks=1:12, labels=months) +
  theme(panel.grid.minor = element_blank(), 
        axis.line = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks = element_blank())
ggsave(paste(ROOT, 'docs/tex_doc/fig/n_trips_month.png', sep=''))

# Map of number of trips in sample
trips <- ebird_trunc %>%
  group_by(c_code_2011) %>%
  mutate(n_trips = as.numeric(n_trips)) %>%
  summarize(n_trips = sum(n_trips, na.rm=T)) %>%
  rename(c_code_11 = c_code_2011)

india_dist_def <- left_join(india_districts, trips, by="c_code_11")

ggplot(india_dist_def, aes(fill = n_trips)) +
  geom_sf(alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_viridis(name = "Number of Trips (2015-19)",
                     direction = -1,
                     guide = guide_colourbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5)) +
  coord_sf(datum = NA) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))
ggsave(paste(ROOT, 'docs/tex_doc/fig/n_trips_district.png', sep=''))

# Number of species per year
n_species_ym <- st_read(paste(ROOT, 'data/csv/', sep=''), 
                    'n_species_ym', stringsAsFactors = F)

n_species_ym <- n_species_ym %>%
  mutate(Year=as.numeric(year), n_species=as.numeric(n_species)) %>%
  filter(Year<2019)
n_species_ym$month <- as.numeric(substr(n_species_ym$YEARMONTH, 6,7))

ggplot(n_species_ym, aes(x=month, y=n_species, group=Year)) +
  geom_line(aes(color=Year)) +
  geom_point(aes(color=Year)) +
  ylab('Species Richness') +
  scale_x_continuous(breaks=1:12, labels=months) +
  scale_color_viridis() + theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        axis.line = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks = element_blank())
ggsave(paste(ROOT, 'docs/tex_doc/fig/n_species_ym.png', sep=''))

# Spatial Coverage
coverage <- st_read(paste(ROOT, 'data/csv/', sep=''), 
                'coverage_dist_grid_8km', stringsAsFactors = F)
coverage <- coverage %>%
  mutate(coverage_all = as.numeric(coverage_all)) %>%
  rename(c_code_11 = c_code_2011)

india_dist_def <- left_join(india_districts, coverage, by="c_code_11")

ggplot(india_dist_def, aes(fill = coverage_all)) +
  geom_sf(alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_viridis(name = "% of district grid cells in which birds spotted",
                     direction = -1,
                     guide = guide_colourbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5)) +
  coord_sf(datum = NA) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))
ggsave(paste(ROOT, 'docs/tex_doc/fig/coverage.png', sep=''))
rm(ls='coverage')

# 3. ------- Trip Level Data
ebird_trip <- st_read(paste(ROOT, 'data/csv/', sep=''), 
                'ebird_triplevel', stringsAsFactors = F)

ebird_trip$state_code_2011 <- substr(ebird_trip$c_code_2011,1,3)

ebird_user <- ebird_trip %>%
  group_by(OBSERVER.ID) %>%
  summarize("No. of Trips" = as.numeric(n_distinct(SAMPLING.EVENT.IDENTIFIER)),
            "No. of States" = as.numeric(n_distinct(state_code_2011)),
            "No. of Districts" = as.numeric(n_distinct(c_code_2011)),
            "No. of Year-months" = as.numeric(n_distinct(YEARMONTH))) %>%
  filter(`No. of Trips` < quantile(`No. of Trips`, 0.95)) %>%
  gather(user_characteristic, number, 2:5) %>%
  arrange(OBSERVER.ID, user_characteristic)
 
# Plot Distribution of user characteristics
ggplot(ebird_user, aes(ebird_user$number)) +
  geom_histogram(aes(y=..density..), binwidth=1) +
  facet_wrap(~user_characteristic, scales='free') +
  ylab('Density') +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(),
        axis.line = element_blank(), 
        axis.ticks = element_blank())
ggsave(paste(ROOT, 'docs/tex_doc/fig/user_charac_hist.png', sep=''))


