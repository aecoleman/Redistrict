# Load packages ----

library(tidyverse)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)

# Download US Shapes ----

conus_code <- 
  tigris::fips_codes %>% 
  select(state, state_code) %>% 
  unique() %>% 
  filter(!state %in% c('AK', 'HI', 'AS', 'GU', 'MP', 'PR', 'UM', 'VI')) %>% 
  .[['state_code']]

ply_county <- 
  tigris::counties(class = 'sf') %>% 
  select(GEOID, STATEFP, COUNTYFP, NAME, CSAFP, CBSAFP) %>% 
  filter(STATEFP %in% conus_code)

rm(conus_code)

# Function to Create Random Variable by Group ----

# This function is used to create random effects. It uses a uniform 
# distribution from 0 to 1, which can then be converted to the distribution of 
# choice by using the quantile function

rand_col <- function(x, group, colname){
  
  lambda_group <- 
    data.frame(
      group = x[[group]] %>% unique(), 
      colname = x[[group]] %>% unique() %>% length() %>% runif(),
      stringsAsFactors = FALSE)
  
  names(lambda_group) <- 
    names(lambda_group) %>% 
    str_replace('^colname$', colname) %>% 
    str_replace('^group$', group)
  
  x <- 
    x %>% 
    merge(lambda_group, by = group)
  
  x
  
}

metrics <- paste0('metric_', 1:4)

for ( metric in metrics ) {
  
  ply_county <- 
    ply_county %>% 
    rand_col(
      group   = 'STATEFP', 
      colname = 'state_mu') %>% 
    rand_col(
      group   = 'GEOID',
      colname = 'county_mu') %>% 
    mutate(!!metric := state_mu + qnorm(county_mu, sd = 0.3)) %>%
    select(-c(state_mu, county_mu))

}

rm(metric, metrics)

ply_county <- 
  ply_county %>% 
  st_transform(crs = 2163)

saveRDS(ply_county, 'ply_county.RDS')
