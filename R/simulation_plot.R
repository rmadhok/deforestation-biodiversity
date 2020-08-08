# PROJECT: Deforestation and Biodiversity
# PURPOSE: SIMULATION PLOT
# AUTHOR: Raahil Madhok

### SET-UP
# Directories
rm(list=ls())
read_path <- '/Users/rmadhok/Dropbox (Personal)/def_biodiv'
setwd(read_path)

# Load Packages
require(dplyr)
require(ggplot2)
#------------------------------------------------------------

# REDUCTION POLICY
reduction <- read.csv('./data/csv/sim_reduction.csv')

# PLOT
constraint <- reduction %>%
  group_by(state) %>%
  summarise(sr_max = mean(sr_max))
ggplot(reduction, aes(x=decline, y=sr_red_)) +
  geom_line() + geom_point() +
  #geom_hline(data = constraint, aes(yintercept = sr_max)) +
  facet_wrap(~state, scales='free')
  
