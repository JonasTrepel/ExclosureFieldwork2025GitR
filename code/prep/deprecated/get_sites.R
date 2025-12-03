library(sf)
library(tidyverse)
library(mapview)


addo_nyathi <- st_read("data/spatial/aenp_nyathi_shape.gpkg") %>% 
  dplyr::select(geom) %>% 
  st_make_valid() %>% 
  mutate(name = "addo_nyathi", 
         area_km2 = as.numeric(st_area(.)/1000000))

addo_main <- st_read("data/spatial/aenp_main_shape.gpkg") %>% 
  rename(geom = geometry) %>% 
  dplyr::select(geom) %>% 
  st_make_valid() %>% 
  mutate(name = "addo_main", 
         area_km2 = as.numeric(st_area(.)/1000000))

pnr_main <- st_read("data/spatial/pnr_main_shape.gpkg") %>% 
  rename(geom = geometry) %>% 
  dplyr::select(geom) %>% 
  st_make_valid() %>% 
  mutate(name = "pnr_main", 
         area_km2 = as.numeric(st_area(.)/1000000))

knp_main <- st_read("data/spatial/knp_shape.gpkg") %>% 
  dplyr::select(geom) %>% 
  st_make_valid() %>% 
  mutate(name = "knp", 
         area_km2 = as.numeric(st_area(.)/1000000))


all_shapes <- rbind(knp_main, pnr_main, addo_main, addo_nyathi)
st_write(all_shapes, "data/spatial/all_shapes.gpkg", append = FALSE)
mapview(all_shapes)
