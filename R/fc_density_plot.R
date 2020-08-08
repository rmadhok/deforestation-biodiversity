# PROJECT: Deforestation and Biodiversity
# PURPOSE: Overlapping density
# AUTHOR: Raahil Madhok
# DATE: Aug 1 2020 [created]

### SET-UP
# Directories
rm(list=ls())
read_path <- '/Users/rmadhok/Dropbox (Personal)/def_biodiv'
setwd(read_path)

# Load Packages
require(lubridate)
require(dplyr)
require(reshape2)
require(ggplot2)

# Assemble Data
data <- data.frame()
full <- read.csv('./data/csv/fc_s2.csv')
full <- full %>%
  dplyr::select(proj_area_forest2) %>%
  mutate(dist = 'C. Full Data')
data <- rbind(data, full)

drop3 <- full %>%
  filter(proj_area_forest2 <= 1000) %>%
  mutate(dist = 'B. Drop 3 Mega-Projects')
data <- rbind(data, drop3)

p95 <- full %>%
  filter(proj_area_forest2 <= quantile(proj_area_forest2, 0.95)) %>%
  mutate(dist = 'A. Truncated at 95th Pctile')
data <- rbind(data, p95)

# PLOT
data$dist <- factor(data$dist, levels=c('A. Truncated at 95th Pctile', 
                                        'B. Drop 3 Mega-Projects', 
                                        'C. Full Data'))
ggplot(data, aes(x=proj_area_forest2, y = ..count../sum(..count..))) + 
  geom_histogram() + 
  labs(x = '\nArea of Forest Cleared for Project (ha.)', 
       y = '% of Projects\n') + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size=15),
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_text(),
        strip.text = element_text(size=15)) +
  facet_wrap(~dist, scales='free')
ggsave('./docs/manuscript/fig/fc_histogram.png', height=3.5,width=10)
