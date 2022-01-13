# PROJECT: Deforestation and Biodiversity
# PURPOSE: Process Tree Cover
# AUTHOR: Raahil Madhok

# ----------- SET-UP -----------------------------------------------------
# Directories
rm(list=ls())
READ <- '/Volumes/Backup Plus/research/data/def_biodiv/forest_cover/'
SAVE <- '/Users/rmadhok/Dropbox/def_biodiv/data/csv/'
SHP <- '/Users/rmadhok/Dropbox/IndiaPowerPlant/data/'
setwd(READ)

# Load Packages
require(tidyverse)
require(sf)
require(raster)
require(exactextractr)
# ------------------------------------------------------------------------

#---------------------------------------------
# RECLASSIFY
#---------------------------------------------

# Load district map
india_dist <- st_read(paste(SHP, "maps/india-district", sep=""),
                      'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F) %>%
  dplyr::select(c_code_11) %>%
  rename(c_code_2011=c_code_11)

# Re-classify forest pixel % to four classes (LTF, OF, MDF, VDF)
{
# Stack Rasters
tifs <- list.files()
r <- raster(tifs[2])

# Crop
r <- crop(r, extent(india_dist))

# Remove non-tree cells
r[r > 100] <- NA

#---------------------------------------------
# FOREST TYPE CLASSIFICATION
#---------------------------------------------
# Very dense: >=70%
# Moderately Dense Forest : 40-70%
# Open forest: 10-40%
# Less than 10% canopy (Scrub): <10%

# Recode forest classes
r[r >= 0 & r < 10] <- 1 #LTF
r[r >= 10 & r < 40] <- 2 #OF
r[r >= 40 & r < 70] <- 3 #MDF 
r[r >= 70] <- 4 #VDF

# Write to tiff
writeRaster(r, "forest_class_2015", format = "GTiff")
  }

#---------------------------------------------
# VALUATION WEIGHTS
#---------------------------------------------
# % of distrcit pixels under each class

# Read classified raster
r <- raster('forest_class_2015.tif')

# Separate into dummy layers
rstack <- layerize(r)

# Fraction of district pixels of each class
# - These are the valuation weights
df <- cbind(st_drop_geometry(india_dist), exact_extract(rstack, india_dist, 'mean'))
names(df) <- c('c_code_2011', 'wt_ltf', 'wt_of', 'wt_mdf', 'wt_vdf')

# Write
write_csv(df, paste(SAVE, '/dist_forest_value_wt.csv', sep=''))

#---------------------------------------------
# VALUATION
#---------------------------------------------
df <- read_csv(paste(SAVE, '/dist_forest_value_wt.csv', sep=''))
# Multiply weights by matrix of 
# forest-class-specific values
# we use: carbon sequestration, 

# Value matrices
# index =[ltf,of,mdf,vdf ]
nwfp <- rev(c(10457,10058,5284,939))
fodder <- rev(c(10669,10669,10669,10669))
fuelwood <- rev(c(8379,8379,8379,8379))
cseq <- rev(c(5051,3409,1525,349))
csto <- rev(c(333227,231585,144019,144019))
seed <- rev(c(9087, 9087, 9087, 9087))
soil <- rev(c(19063,12334,5604,1125))
water <- rev(c(2918, 1825,896,172))
tev <- rev(c(149757,108419,60302,31528))
npv <- rev(c(3198571,2368571,1455000, 957692))
X <- cbind(nwfp,fodder,fuelwood,cseq,csto,seed,soil,water,tev,npv)
row.names(X) <- c('ltf', 'of', 'mdf', 'vdf')

# Weight Matrix
W <- as.matrix(dplyr::select(df, starts_with('wt')))
row.names(W) <- df$c_code_2011

# Values
WX <- W %*% X
WX <- as.data.frame(WX)
WX$c_code_2011 = row.names(WX)

# Write
write_csv(WX, paste(SAVE, '/dist_forest_value.csv', sep=''))
#---------------------------------------------
# PLOT
#---------------------------------------------

# Add geometry
df_plot <- left_join(india_dist, WX, by='c_code_2011')

# Plot
value_plot <- ggplot(df_plot, aes(fill = cseq_npv)) +
  geom_sf(alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_viridis_c(name='Carbon Sequestration (Rs./ha/year)',
                       direction = -1,
                       guide = guide_colourbar(title.position='top')) +
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


