#-------------------------------------------------------------
# PROJECT: Deforestation and Biodiversity
# PURPOSE: T-test betwen eBird user homes and DHS 
# AUTHOR: Raahil Madhok
#-------------------------------------------------------------

### SET-UP
# Directories
rm(list=ls())
READ <- '/Volumes/Backup Plus 1/research/data/'
SAVE <- '/Users/rmadhok/Dropbox/def_biodiv/'
setwd(READ)

# Load Packages
require(tidyverse)
require(sf)
require(weights)
require(data.table)
require(stargazer)
#-------------------------------------------------------------
# CONSTRUCT SAMPLE
#-------------------------------------------------------------

# Load eBird-DHS matched sample (10,832 paird users)
ebd_dhs <- read_csv(paste(SAVE, 'data/csv/ebd_dhs_match.csv', sep='')) %>%
  filter(keep == 1) %>% 
  dplyr::select(!c('similar', 'keep'))

# Load DHS household data (n=601,509 households)
dhs <- haven::read_stata('./IndiaPowerPlant/data/nfhs/household/IAHR74FL.DTA',
                          col_select = c('hhid', 'hv001', 'hv002', 'hv005', 'hv009', 
                                         'hv014', 'hv201', 'hv205', 'hv206','hv207', 
                                         'hv209', 'hv212', 'hv216', 'hv217', 'hv220', 
                                         'hv242', 'hv243a', 'hv270', 'sh37j', 'sh37n', 
                                         'sh37r', 'hv025'))

# Clean DHS
dhs <- dhs %>%
  rename(dhsclust = hv001,
         hh_id = hv002,
         wt = hv005,
         hh_size = hv009,
         children = hv014,
         ruralurban = hv025,
         water_source = hv201,
         toilet = hv205,
         elec = hv206,
         radio = hv207,
         fridge = hv209,
         car = hv212,
         rooms_sleep = hv216,
         relation = hv217,
         hh_head_age = hv220,
         kitchen_separate = hv242,
         cellphone = hv243a,
         wealth_idx = hv270,
         tv_colour = sh37j,
         internet = sh37n,
         washer = sh37r) %>%
  mutate(water_piped = ifelse(water_source %in% c('10', '11'), 1, 0),
         rooms_per_adult = rooms_sleep / (hh_size - children),
         momdad = ifelse(relation == 2, 1, 0),
         flush_toilet = ifelse(toilet >= 10 & toilet <= 15, 1, 0),
         wealth_idx_z = (wealth_idx - mean(wealth_idx))/sd(wealth_idx),
         wt = wt/1000000) # scaling factor to make bootstrap work.
#-----------------------------------------------
# Note: some clusters paired to multiple users 
# i.e. many duplicates in matched sample
# construct weighted subset of unique hh with
# weights = num users the cluster is matched to
match <- ebd_dhs %>%
  group_by(dhsclust) %>%
  summarise(n_users = n(),
            ru_dhs = first(ru_dhs)) %>%
  ungroup()

# hh subset
match <- merge(dhs, match, by='dhsclust')

#-------------------------------------------------------------
# T-TESTS
#-------------------------------------------------------------
varlist <- c('hh_size', 'cellphone', 'fridge', 'car', 
             'kitchen_separate', 'tv_colour', 'internet', 
             'washer', 'flush_toilet')

# ttest function
t_test <- function(i, sample = 'All') {
  
  # select variables
  if(sample == 'All') {
    x <- match %>% dplyr::select(varlist[i]) %>% as_vector()
    y <- dhs %>% dplyr::select(varlist[i]) %>% as_vector()
    xwt <- dplyr::select(match, n_users) %>% as_vector()
    ywt <- dplyr::select(dhs, wt) %>% as_vector()
  }
  
  if(sample == 'U') {
    x <- match %>% filter(ruralurban == 1) %>% dplyr::select(varlist[i]) %>% as_vector()
    y <- dhs %>% filter(ruralurban == 1) %>% dplyr::select(varlist[i]) %>% as_vector()
    xwt <- match %>% filter(ruralurban == 1) %>% dplyr::select(n_users) %>% as_vector()
    ywt <- dhs %>% filter(ruralurban == 1) %>% dplyr::select(wt) %>% as_vector()
  }
  
  if(sample == 'R') {
    x <- match %>% filter(ruralurban == 2) %>% dplyr::select(varlist[i]) %>% as_vector()
    y <- dhs %>% filter(ruralurban == 2) %>% dplyr::select(varlist[i]) %>% as_vector()
    xwt <- match %>% filter(ruralurban == 2) %>% dplyr::select(n_users) %>% as_vector()
    ywt <- dhs %>% filter(ruralurban == 2) %>% dplyr::select(wt) %>% as_vector()
  }
  
  # ttest
  ttest <- wtd.t.test(x, y, 
                      weight = xwt, 
                      weighty = ywt, 
                      samedata = F, 
                      mean1 = F, 
                      bootse = F, 
                      bootn = 100)
  
  # build output dataframe
  item <- data.frame(
    list(
      Variable = varlist[i],
      `Matched eBird` = ttest$additional[[2]],
      DHS = ttest$additional[[3]],
      Difference = ttest$additional[[1]],
      `p-value` = ttest$coefficients[[3]]
      )
  )
  
  return(item)
}
idx <- 1:length(varlist)

# difference in means for all samples
df_all <- as.data.frame(rbindlist(lapply(idx, t_test, sample = 'All')))
df_r <- as.data.frame(rbindlist(lapply(idx, t_test, sample = 'R')))
df_u <- as.data.frame(rbindlist(lapply(idx, t_test, sample = 'U')))

#-------------------------------------------------------------
# FINAL TABLE
#-------------------------------------------------------------

# Prepare Tables function
clean <- function(df) {
  
  # variable labels
  df$Variable <- c('HH Size', 'Cellphone (=1)', 'Fridge (=1)', 'Car (=1)', 
                   'Sep. Kitchen (=1)', 'Colour TV (=1)',  'Internet (=1)', 
                   'Washing Machine (=1)', 'Flush Toilet (=1)')

  # Columns
  names(df) <- c('Variable', 'Matched eBird', 'DHS', 'Difference', 'p-value')

  # trailing zeros
  df$Difference <- sprintf("%.3f", df$Difference)
  df$`p-value` <- sprintf("%.3f", df$`p-value`)

# Significance stars
  df <- df %>%
    mutate(Difference = replace(Difference, `p-value` <= 0.01, paste(Difference, '***', sep='')),
           Difference = replace(Difference, `p-value` > 0.01 & `p-value` <= 0.05 , paste(Difference, '**', sep='')),
           Difference = replace(Difference, `p-value` > 0.05 & `p-value` <= 0.1 , paste(Difference, '*', sep='')))
  
  return(df)

}

# Prep tables
dflist <- list(df_all = df_all, df_u = df_u, df_r = df_r)
df_clean <- lapply(dflist, clean)
df_clean <- lapply(df_clean, function(x) x %>% select(Variable, Difference))

# Merge tables
table <- df_clean %>% 
  reduce(full_join, by='Variable')
names(table) <- c('Variable', 'All', 'Urban', 'Rural')

# Write out
stargazer(table, 
          summary=F, 
          rownames=F,
          float=F,
          column.sep.width = '1.8cm',
          out=paste(SAVE,'docs/jmp/tex_doc/v3/tables/ebd_dhs_ttest.tex', sep=''))

