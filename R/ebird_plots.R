# PROJECT: Deforestation and Biodiversity
# PURPOSE: Construct eBird Plots
# AUTHOR: Raahil Madhok
# DATE: Jan 12 2019 [created]

# Set Directories
ROOT <- '/Users/rmadhok/Documents/ubc/research/def_biodiv/'
EBIRD.GEO <- paste(ROOT, 'data/csv/', sep='')
DIST.GEO <- '/Users/rmadhok/Dropbox (CID)/IndiaPowerPlant/data/maps/india-district/'
STATE.GEO <- '/Users/rmadhok/Dropbox (CID)/IndiaPowerPlant/data/maps/datameet_maps/States/'
SAVE.PATH <- paste(ROOT, 'docs/tex_doc/fig/', sep='')

# Packages
require(rgeos)
require(rgdal)
require(raster)
require(ggplot2)
require(geosphere)
require(viridis)

## Read Data

# Read Maps
districts <- readOGR(DIST.GEO,'SDE_DATA_IN_F7DSTRBND_2011',stringsAsFactors = F)
states <- readOGR(STATE.GEO, 'Admin2', stringsAsFactors = F)


# Read eBird
proportion <- 5
ebird <- read.csv(paste(ROOT,'data/csv/ebird_sample_', proportion, '.csv', sep=''), stringsAsFactors=F)

# Read Deforestation-Biodiversity
def_biodiv <- read.csv(paste(ROOT, 'data/csv/fc_ebd_master_', proportion,'.csv', sep=''), stringsAsFactors=F)

## Make Plots

#   1. All eBird (2017)

# 2017 
ebird.2017 <- ebird[ebird$YEAR==2017, ]

# Plot
ggplot() +
  geom_polygon(data = districts, aes(x=long,y=lat,group=group),fill= 'white', color = 'grey', size=.5) + 
  geom_point(data=ebird.2017, aes(x=LONGITUDE,y=LATITUDE), color='red', alpha=.03) +
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
        legend.title=element_text(size=20) , legend.text=element_text(size=10))
ggsave(paste(SAVE.PATH, 'ebird_', proportion, '_all_2017.pdf', sep=''), width=8,height=6)

#   2. Biodiviersity (2017)

# Index
biodiversity.df <- def_biodiv[def_biodiv$year==2017, c('c_code_2011', 'year', 'month', 'shannon_index', 'district_forest_cum')]

# Aggregate to year
biodiversity.2017.df <- aggregate(.~c_code_2011, biodiversity.df, mean)
names(biodiversity.2017.df)[1] <- 'c_code_11'

# Merge with district attribute table
districts <- merge(districts, biodiversity.2017.df, by='c_code_11')

# PLot
p <- ggplot() +
  # municipality polygons
  geom_polygon(data = districts, aes(fill = districts@data$shannon_index, 
                                    x = long, 
                                    y = lat, 
                                    group = group)) +
  # municipality outline
  geom_path(data = districts, aes(x = long, 
                                 y = lat, 
                                 group = group), 
            color = "white", size = 0.1) +
  coord_equal()


map <- ggplot() +
  geom_polygon(data=districts, color='grey', size=.5, aes(x=long,y=lat,group=group, fill=districts@data$shannon_index)) +
  coord_equal() + theme_minimal() + 
  scale_fill_viridis(option = "magma", direction = -1) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),  
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(),  
        axis.title.y = element_blank(),
        legend.position = 'bottom',
        legend.title=element_text(size=20) , legend.text=element_text(size=10))
ggsave(paste(SAVE.PATH, 'biodiversity_2017.pdf', sep=''), width=8,height=6)

map <- ggplot(data=districts, aes(x=long,y=lat,group=group), 
       color='grey', size=.5) +
  geom_polygon(aes(fill=districts@data$index)) +
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
        legend.title=element_text(size=20) , legend.text=element_text(size=10))


