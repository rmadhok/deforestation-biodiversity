# Load Packages
require(dplyr)

read_path_head <- '/Users/rmadhok/Documents/ubc/research/def_biodiv/data/'
setwd(read_path_head)

# Read eBird
ebird <- read.csv(paste(read_path_head, 'csv/ebird_sample_5.csv', sep=''))

## Filter Down -----

# Print task
print('----')
print('Cleaning raw data')

# Keep Veterans
ebird <- ebird %>%
  group_by(YEAR, OBSERVER.ID) %>%
  mutate(n_months = n_distinct(YEARMONTH))

ebird <- ebird[ebird$n_months >= 6, ]

# Drop Duplicates
ebird <- distinct(ebird, GROUP.IDENTIFIER, TAXONOMIC.ORDER, .keep_all = T)

# Duration
ebird <- subset(ebird, DURATION.MINUTES >= 5 & DURATION.MINUTES <= 240)

# Incomplete Checklist
ebird$OBSERVATION.COUNT[ebird$OBSERVATION.COUNT == 'X'] <- NA
ebird <- ebird %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  mutate(num_birds = sum(!is.na(OBSERVATION.COUNT)))
ebird <- ebird[ebird$num_birds > 0, ]

# Select Protocols
ebird <- ebird[ebird$PROTOCOL.CODE == 'P21' | ebird$PROTOCOL.CODE == 'P22', ]

# Stats
ebird <- ebird %>% 
  group_by(c_code_2011, YEARMONTH) %>% 
  mutate(n_birders = n_distinct(OBSERVER.ID),
         n_trips = n_distinct(SAMPLING.EVENT.IDENTIFIER))

# Diversity Indices

# Print task
print('----')
print('Computing Species Diversity Indices')

# Species Richness
ebird <- ebird %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  mutate(species_richness = n())

# Shannon Index
ebird$OBSERVATION.COUNT <- as.numeric(as.character(ebird$OBSERVATION.COUNT))
ebird <- ebird %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  mutate(shannon_index = -sum((OBSERVATION.COUNT / sum(OBSERVATION.COUNT, na.rm = T))*
                                  log(OBSERVATION.COUNT / sum(OBSERVATION.COUNT, na.rm = T)), 
                               na.rm = T))

# Simpson Index
ebird <- ebird %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>%
  mutate(simpson_index = 1 - ( (sum(OBSERVATION.COUNT*(OBSERVATION.COUNT - 1), na.rm = T)) /
           (sum(OBSERVATION.COUNT, na.rm = T)*(sum(OBSERVATION.COUNT, na.rm = T) - 1)) ))

# AGGREGATE

# Print task
print('----')
print('Aggregating to district-month')

# Sampling Event Level
ebird <- distinct(ebird, SAMPLING.EVENT.IDENTIFIER, .keep_all = T)

# Aggregate
ebird <- ebird %>%
  group_by(c_code_2011, YEARMONTH) %>%
  summarize(species_richness = mean(species_richness, na.rm = T),
            shannon_index = mean(shannon_index, na.rm = T),
            simpson_index = mean(simpson_index, na.rm = T),
            duration_min = mean(DURATION.MINUTES, na.rm = T),
            effort_distance_km = mean(EFFORT.DISTANCE.KM),
            n_birders = first(n_birders),
            n_trips = first(n_trips))
