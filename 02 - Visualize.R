
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

sparse_connect <- 
  pnt_county %>% 
  filter(ROUND == 0L) %>% 
  .[['DISTRICT']] %>% 
  map( ~which( .x == (
    pnt_county %>% 
  filter(ROUND != 0L) %>% 
  .[['DISTRICT']] ) ) )

line_connect <-   
  st_connect(
    pnt_county %>% filter(ROUND == 0L), 
    pnt_county %>% filter(ROUND != 0L), 
    ids = sparse_connect )

ggplot() + 
  geom_sf(
    aes(
      fill = DISTRICT),
    lwd = 0,
    data = ply_county_district) + 
  geom_sf(
    data = line_connect) + 
  geom_sf(
    data = pnt_county %>% filter(ROUND == 0L)) + 
  scale_fill_viridis_c()
