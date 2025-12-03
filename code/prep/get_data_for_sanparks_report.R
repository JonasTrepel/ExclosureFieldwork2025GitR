#get data for sanparks ready 
library(data.table)
library(tidyverse)
library(sf)

coords <- st_read("data/spatial/clean_plot_locations_south_africa_exclosures_2025.gpkg") %>% 
  st_transform(crs = 4326) %>% 
  mutate(lon = st_coordinates(.)[,1], 
         lat = st_coordinates(.)[,2]) %>% 
  as.data.frame() %>% 
  mutate(geom = NULL)
  
dt_traits <- fread("data/processed/fragments/species_trait_data_exclosures_2025.csv")

dt_sp <- fread("data/raw/plot_species_2025.csv") %>% 
  left_join(coords[, c("plot_id", "lon", "lat")])

fwrite(x = dt_sp, "data/processed/fragments/addo_knp_exclosure_plots_2025_species_lists.csv")
glimpse(dt_sp)
#next: traits....

dt_traits <- fread("data/processed/fragments/species_trait_data_exclosures_2025.csv") %>% 
  dplyr::select(species, spines, leaf_type, growth_form, 
                leaf_length, leaf_width, plant_height_max, biomass_density)
fwrite(x = dt_traits, "data/processed/fragments/addo_knp_exclosure_plots_2025_pant_traits.csv")


glimpse(dt_traits)
