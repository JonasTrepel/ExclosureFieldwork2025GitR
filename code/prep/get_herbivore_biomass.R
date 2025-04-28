library(data.table)
library(tidyverse)
library(sf)
library(tidylog)


pc <- fread("data/raw/population_counts.csv") %>% 
  mutate(species = case_when(
    .default = species, 
    species == "Taurotragus oryx" ~ "Tragelaphus oryx",
    species == "Tragelaphus sylvaticus" ~ "Tragelaphus imberbis",
    species  == "Giraffa giraffa" ~ "Giraffa camelopardalis"))

ht <- fread("data/raw/HerbiTraits_1.2.csv") %>% 
  dplyr::select(species = Binomial, 
                class = Class, order = Order, 
                mass_g = Mass.g, 
                feeding_guild = Guild.w.Omnivory, 
                fermentation_type = Fermentation.Type) %>% 
  mutate(mass_kg = mass_g/1000)

area <- st_read("data/spatial/all_shapes.gpkg") %>% 
  as.data.table() %>% 
  mutate(geom = NULL) %>% 
  mutate(site = case_when(
    name == "knp" ~ "knp", 
    name == "addo_main" ~ "addo_main", 
    name == "addo_nyathi" ~ "addo_nyathi",
    name == "pnr_main" ~ "pnr"
  ))

dt <- pc %>%
  left_join(ht) %>%
  left_join(area) %>% 
  filter(!is.na(mass_kg) & !is.na(count) & !count == 0) %>% #remove predators
  mutate(species_total_biomass = mass_kg*count, 
         elephant_biomass = ifelse(species == "Loxodonta africana", species_total_biomass, 0)) %>% 
  group_by(site) %>% 
  mutate(total_biomass = sum(species_total_biomass), 
         herbivore_biomass_kg_km2 = total_biomass/area_km2, 
         elephant_biomass_kg_km2 = sum(elephant_biomass)/area_km2, 
         ratio_elephant_biomass  = elephant_biomass_kg_km2/herbivore_biomass_kg_km2)

dt %>% dplyr::select(site, area_km2, herbivore_biomass_kg_km2, elephant_biomass_kg_km2, ratio_elephant_biomass) %>% unique()

