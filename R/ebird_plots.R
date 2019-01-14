# PROJECT: Deforestation and Biodiversity
# PURPOSE: Construct eBird Plots
# AUTHOR: Raahil Madhok
# DATE: Jan 12 2019 [created]

# Set Directories
ROOT <- '/Users/rmadhok/Documents/ubc/research/def_biodiv/'
EBIRD.GEO <- paste(ROOT, 'data/csv/', sep='')
DIST.GEO <- '/Users/rmadhok/Dropbox (CID)/IndiaPowerPlant/data/maps/india-district/'
SAVE.PATH <- paste(ROOT, 'docs/tex_doc/fig/', sep='')

# Packages
require(rgeos)
require(rgdal)
require(raster)
require(ggplot2)
require(geosphere)

## Read Data

# Read District Map
districts <- readOGR(DIST.GEO,'SDE_DATA_IN_F7DSTRBND_2011',stringsAsFactors = F)

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
ggsave(paste(SAVE.PATH, 'ebird_', proportion, 'all_2017.pdf', sep=''), width=8,height=6)

#   2. Biodiviersity (2017)

# Index
biodiversity.df <- def_biodiv[def_biodiv$year==2017, c('c_code_2011', 'year', 'month', 'shannon_index')]

# Aggregate to year
biodiversity.2017.df <- aggregate(biodiversity.df$shannon_index,
                                  by=list(c_code_11=biodiversity.df$c_code_2011), 
                                  mean)
names(biodiversity.2017.df)[2] <- 'shannon_index'

# Merge with district attribute table
districts <- merge(districts, biodiversity.2017.df, by='c_code_11')

# Plot
ggplot(data = districts, aes(x=long,y=lat, group=group)) +
  geom_sf(aes(fill='shannon_index')) + 
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

ggsave(paste(SAVE.PATH, 'biodiversity_2017.pdf', sep=''), width=8,height=6)


