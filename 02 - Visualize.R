
source('01 - Process.R')

ply_county_district <- 
  pnt_county %>% 
  st_set_geometry(NULL) %>% 
  select(GEOID, DISTRICT, ROUND) %>% 
  merge(ply_county, .) %>% 
  group_by(DISTRICT) %>% 
  summarize()

ggplot() + 
  geom_sf(
    aes(
      fill = DISTRICT),
    lwd = 0,
    data = ply_county_district) + 
  geom_sf(
    data = pnt_county %>% filter(ROUND == 0L)) + 
  scale_fill_viridis_c()

