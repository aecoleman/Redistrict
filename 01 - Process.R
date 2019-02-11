

# Parameters ----

# Number of districts
num_districts <- 100

# Weights
w <- c(0.47, 0.4, 0.07, 0.06)

# Setup ----

library(tidyverse)
library(sf)
library(lwgeom)
library(nngeo)
library(future)
library(furrr)

if (!file.exists('ply_county.RDS')) 
  source('00 - Setup.R')

ply_county <- readRDS('ply_county.RDS')

pnt_county <- 
  ply_county %>% 
  st_centroid()

# Function to create decile rankings ----
decile <- function(x) {
  x %>% 
  rank() %>% 
  `%/%`(., (max(.) / 9)) + 1
}

# Get Deciles and compute county score ----

t_0 <- Sys.time()

# This probably should be changed so that the county_score is the 
# Mahalanobis Distance
pnt_county <- 
  pnt_county %>% 
  mutate( decile_1 = decile(metric_1),
          decile_2 = decile(metric_2),
          decile_3 = decile(metric_3),
          decile_4 = decile(metric_4)) %>% 
  mutate( county_score = (decile_1 + decile_2 + decile_3 + decile_4) / 4 )

# Initial Step ----

i <- 0L

district_seed <- 
  pnt_county %>% 
  sample_n(num_districts) %>% 
  mutate(DISTRICT = seq_len(num_districts), 
         ROUND    = i) %>% 
  st_set_geometry(NULL)

pnt_county <- 
  pnt_county %>% 
  merge(district_seed, 
        all = TRUE)

rm(district_seed)

# Test List Matching ----

tpb <- txtProgressBar(min = 0, max = nrow(pnt_county), style = 3)

future::plan(multiprocess)

while (pnt_county %>% filter(is.na(DISTRICT)) %>% nrow() > 0) {
  
  setTxtProgressBar(pb = tpb, value = nrow(pnt_county) - nrow(filter(pnt_county, is.na(DISTRICT))) )
  
  i <- i + 1L
  
  nn_county <- 
    st_nn(
      pnt_county %>% filter(ROUND == 0L),
      pnt_county %>% filter(is.na(DISTRICT)),
      progress = FALSE)
  
  nn_unique <- 
    nn_county %>% 
    unique()
  
  nn_dupe <- 
    nn_unique %>% 
    map(~which(.x == nn_county))
  
  
  new_counties <- 
    furrr::future_map2(
      nn_unique,
      nn_dupe,
      function(x, y) {
        
        contestant_district <- 
          pnt_county %>% 
          filter(ROUND == 0L) %>% 
          slice(y) %>% 
          .[['DISTRICT']]
        
        contested_county <- 
          pnt_county %>% 
          filter(is.na(DISTRICT)) %>% 
          slice(rep(x, length(y))) %>% 
          mutate(DISTRICT = contestant_district,
                 ROUND    = rep(i, length(y)))
        
        district_scores <- 
          pnt_county %>% 
          filter(DISTRICT %in% contestant_district) %>% 
          rbind(contested_county) %>% 
          group_by(DISTRICT) %>% 
          summarize(dist_score_1 = sum(county_score), 
                    dist_score_2 = n()) %>% 
          st_convex_hull() %>% 
          mutate(dist_score_3 = st_area(geometry) %>% as.numeric(),
                 dist_score_4 = st_perimeter(geometry) %>% as.numeric()) %>% 
          mutate(dist_score = w[1] * dist_score_1 + 
                   w[2] * dist_score_2 + 
                   w[3] * dist_score_3 + 
                   w[4] * dist_score_4)
        
        winning_district <- 
          district_scores[['dist_score']] %>% 
          which.min() %>% 
          district_scores[['DISTRICT']][.]
        
        contested_county %>% 
          filter(DISTRICT == winning_district)
        
      }) %>% 
    do.call(rbind, .)
  
  pnt_county <- 
    pnt_county %>% 
    filter(!GEOID %in% new_counties[['GEOID']]) %>% 
    rbind(new_counties)
  
  rm(new_counties, nn_county, nn_dupe, nn_unique)
  
}

close(tpb)
rm(tpb)

t_1 <- Sys.time()

district_scores <- 
  pnt_county %>%  
  group_by(DISTRICT) %>% 
  summarize(dist_score_1 = sum(county_score, na.rm = TRUE), 
            dist_score_2 = n()) %>% 
  st_convex_hull() %>% 
  mutate(dist_score_3 = st_area(geometry) %>% as.numeric(),
         dist_score_4 = st_perimeter(geometry) %>% as.numeric()) %>% 
  mutate(dist_score = w[1] * dist_score_1 + 
           w[2] * dist_score_2 + 
           w[3] * dist_score_3 + 
           w[4] * dist_score_4)

time_to_complete <- 
  difftime(t_1, t_0, units = 'secs') %>% 
  as.numeric() 

cat(
sprintf(fmt = 'Completed %d iterations in %02.0f:%02.0f:%04.1f\n', 
        i, 
        time_to_complete %/% (60 * 60), 
        (time_to_complete %% (60 * 60) ) %/% 60, 
        time_to_complete %% 60) )

