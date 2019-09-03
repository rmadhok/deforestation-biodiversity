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


### DEFORESTATION
india_districts <- st_read(paste(SHP, "maps/india-district", sep=""), 
                           'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

deforestation <- st_read(paste(ROOT, 'data/csv/', sep=''), 
                         'fc_dist_ym', stringsAsFactors = F)

defn_year <- deforestation %>% 
  rename(c_code_11 = c_code_2011) %>%
  mutate(district_forest = as.numeric(district_forest)) %>%
  group_by(c_code_11, year) %>%
  summarize(district_forest = sum(district_forest, na.rm = T)) %>%
  filter(year == '2017')
           
india_dist_def <- left_join(india_districts, defn_year, by='c_code_11')

ggplot(india_dist_def, aes(fill = district_forest)) +
  geom_sf(alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_viridis(discrete = F,
                     name = "Deforestation (km2)",
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







## SPECIES DIVERSITY
kkkkkkkk


# Histogram - triplevel data
cols <- c('Mean'='red', 'Median' = 'blue')
ggplot(ebird.full, aes(ebird.full$species_richness)) + 
  geom_histogram(aes(y=..density..), binwidth = 3) +
  geom_vline(aes(xintercept = mean(species_richness), colour='Mean'), size=0.5) +
  geom_vline(aes(xintercept = median(species_richness), colour='Median'), size=0.5) +
  ggtitle('Histogram of Trip-level Species Richness') +
  xlab('Species Richness') + 
  ylab('Density') +
  scale_colour_manual(values=cols) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_blank(), 
        axis.ticks = element_blank())
ggsave(paste(save_path_head, 'docs/tex_doc/fig/density_triplevel.png', sep=''), width=8,height=6)



