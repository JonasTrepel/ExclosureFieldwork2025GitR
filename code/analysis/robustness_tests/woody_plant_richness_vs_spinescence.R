library(data.table)
library(tidyverse)
library(tidylog)

dt_sp <- fread("data/processed/fragments/plot_species_and_data.csv")
dt_traits <- fread("data/processed/fragments/species_trait_data_exclosures_2025.csv")



trait_data <- dt_traits %>%
  mutate(leaf_area = ifelse(leaf_type == "a", 0, leaf_area)) %>% # absent leaves have an area of 0... 
  mutate(
    growth_form = as.factor(growth_form_simple),
    spines = as.factor(spines), 
    biomass_density_ordinal = factor(biomass_density_ordinal,
                                     levels = sort(unique(biomass_density_ordinal)),
                                     ordered = TRUE),
    leaf_type = as.factor(leaf_type), 
    plant_height_max = as.numeric(plant_height_max), 
    leaf_area = as.numeric(leaf_area)) %>%
  dplyr::select(species,
                plant_height_max, leaf_area,
                growth_form, spines, biomass_density_ordinal, leaf_type) %>% 
  unique() %>%
  filter(complete.cases(.))



dt_tsp <- dt_sp %>%
  left_join(trait_data) %>%
  mutate(spinescence = ifelse(spines == "n", "no", "yes")) %>% 
  group_by(plot_id) %>% 
  mutate(ratio_spinescence = mean(spinescence == "yes", na.rm = T)) %>% 
  ungroup()

summary(dt_tsp)
dt_tsp$spinescence
dt_tsp$spines
dt_tsp$ratio_spinescence


dt_tsp2 <- dt_tsp %>%
  dplyr::select(ratio_spinescence, woody_richness_plot, plot_id, in_or_out) %>% 
  unique()

cor.test(dt_tsp2$ratio_spinescence, dt_tsp2$woody_richness_plot)

mean(dt_tsp2[dt_tsp2$in_or_out == "outside", ]$ratio_spinescence)
sd(dt_tsp2[dt_tsp2$in_or_out == "outside", ]$ratio_spinescence)

mean(dt_tsp2[dt_tsp2$in_or_out == "inside", ]$ratio_spinescence)
sd(dt_tsp2[dt_tsp2$in_or_out == "inside", ]$ratio_spinescence)

#woodies
dt_tsp_w <- dt_sp %>%
  left_join(trait_data) %>%
  filter(life_form %in% c("Shrub", "Tree")) %>% 
  mutate(spinescence = ifelse(spines == "n", "no", "yes")) %>% 
  group_by(plot_id) %>% 
  mutate(ratio_spinescence = mean(spinescence == "yes", na.rm = T)) %>% 
  ungroup()

summary(dt_tsp_w)
dt_tsp_w$spinescence
dt_tsp_w$spines
dt_tsp_w$ratio_spinescence


dt_tsp_w2 <- dt_tsp_w %>%
  dplyr::select(ratio_spinescence, woody_richness_plot, plot_id, in_or_out) %>% 
  unique()

cor.test(dt_tsp_w2$ratio_spinescence, dt_tsp_w2$woody_richness_plot)

mean(dt_tsp_w2[dt_tsp_w2$in_or_out == "outside", ]$ratio_spinescence)
sd(dt_tsp_w2[dt_tsp_w2$in_or_out == "outside", ]$ratio_spinescence)

mean(dt_tsp_w2[dt_tsp_w2$in_or_out == "inside", ]$ratio_spinescence)
sd(dt_tsp_w2[dt_tsp_w2$in_or_out == "inside", ]$ratio_spinescence)

##### get general species richness numbers -----

dt_sp %>% 
  mutate(graminoid_richness_plot = ifelse(is.na(graminoid_richness_plot), 0, graminoid_richness_plot), 
         forb_richness_plot = ifelse(is.na(forb_richness_plot), 0, forb_richness_plot), 
         woody_richness_plot = ifelse(is.na(woody_richness_plot), 0, woody_richness_plot)) %>% 
  group_by(in_or_out) %>% 
  summarize(
    mean_plant_richness_plot = mean(plant_richness_plot), 
    sd_plant_richness_plot = sd(plant_richness_plot),
    
    mean_graminoid_richness_plot = mean(graminoid_richness_plot), 
    sd_graminoid_richness_plot = sd(graminoid_richness_plot), 
    
    mean_forb_richness_plot = mean(forb_richness_plot), 
    sd_forb_richness_plot = sd(forb_richness_plot), 
    
    mean_woody_richness_plot = mean(woody_richness_plot), 
    sd_woody_richness_plot = sd(woody_richness_plot), 
  ) %>% 
  as.data.table()


n_distinct(dt_sp[life_form %in% c("Tree", "Shrub")]$species)
n_distinct(dt_sp[!life_form %in% c("Tree", "Shrub")]$species)
