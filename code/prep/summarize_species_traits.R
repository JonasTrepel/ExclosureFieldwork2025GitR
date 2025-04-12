#clean species traits 

get_mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

library(data.table)
library(tidyverse)

dt <- fread("data/raw/species_traits_2025.csv") 
glossary <- fread("data/raw/glossary_data_2025_field_work.csv") 


dt[dt == ""] <- NA
names(dt)

### 1. Life Form, Spines & Leaf Type --------
table(dt$life_form)
table(dt$spines)
table(dt$leaf_type)

dt_lt_names <- glossary %>% 
  rename(leaf_type = abbreviation, 
         leaf_type_clean = meaning)

dt_lf_names <- glossary %>% 
  rename(life_form = abbreviation, 
         life_form_clean = meaning)

dt_lf_sp_lt <- dt %>% 
  dplyr::select(species, life_form, spines, leaf_type) %>% 
  group_by(species) %>% 
  summarize(life_form = get_mode(life_form, na.rm = TRUE), 
            spines = get_mode(spines, na.rm = TRUE), 
            leaf_type = get_mode(leaf_type, na.rm = TRUE)) %>% 
  left_join(dt_lt_names) %>% 
  left_join(dt_lf_names) %>% 
  dplyr::select(species, spines, leaf_type = leaf_type_clean, life_form = life_form_clean)
  
  summary(dt_lf_sp_lt)


### 2. Growth Form ---------------------------------
  
dt_gf_names <- glossary %>% 
    rename(growth_form = abbreviation, 
           growth_form_clean = meaning)
  
dt_gf <- dt %>% 
  dplyr::select(species, growth_form_1, growth_form_2, growth_form_3, growth_form_4, growth_form_5) %>% 
  pivot_longer(cols = c(growth_form_1, growth_form_2, growth_form_3, growth_form_4, growth_form_5), 
               values_to = "growth_form") %>% 
  filter(!is.na(growth_form)) %>% 
  group_by(species) %>% 
  summarize(growth_form = get_mode(growth_form, na.rm = TRUE), 
            n_samples_growth_form = n()) %>% 
  left_join(dt_gf_names) %>% 
  dplyr::select(species, growth_form = growth_form_clean, n_samples_growth_form) %>% 
  mutate(growth_form_simple = case_when(
    growth_form %in% c("tree_single_stemmed", "shrub_single_stemmed", "succulent_shrub_single_stemmed") ~ "single_stemmed_woody", 
    growth_form %in% c("tree_multi_stemmed", "shrub_multi_stemmed", "succulent_shrub_multi_stemmed") ~ "multi_stemmed_woody",
    growth_form %in% c("creeping_forb", "creeping_graminoid") ~ "creeping_herb",
    growth_form %in% c("straight_forb", "sparse_upward_graminoid", "large_graminoid", "succulent_straight_forb") ~ "straight_herb",
    growth_form %in% c("round_forb", "relaxed_tussock_graminoid", "angry_tussock_graminoid", "succulent_round_forb") ~ "round_herb",
    growth_form %in% c("messy_forb", "succulent_messy_forb") ~ "messy_herb",
    growth_form %in% c("cushion_forb") ~ "cushion_herb",
    growth_form %in% c("succulent_climbing_forb", "climbing_forb") ~ "climbing_herb",
    growth_form %in% c("parasite") ~ "parasitic_herb",
  ))

table(dt_gf$growth_form_simple)
unique(dt_gf$growth_form)
summary(dt_gf)

### 3. Biomass Density ---------------------------------
dt_bd_names <- glossary %>% 
  rename(biomass_density = abbreviation, 
         biomass_density_clean = meaning)

dt_bd <- dt %>% 
  dplyr::select(species, biomass_density_1, biomass_density_2, biomass_density_3, biomass_density_4, biomass_density_5) %>% 
  pivot_longer(cols = c(biomass_density_1, biomass_density_2, biomass_density_3, biomass_density_4, biomass_density_5), 
               values_to = "biomass_density") %>% 
  filter(!is.na(biomass_density)) %>% 
  group_by(species) %>% 
  summarize(biomass_density = get_mode(biomass_density, na.rm = TRUE), 
            n_samples_biomass_density = n()) %>% 
  left_join(dt_bd_names) %>% 
  dplyr::select(species, biomass_density = biomass_density_clean, n_samples_biomass_density)
summary(dt_bd)

### 4. Leaf Length ---------------------------------
dt_ll <- dt %>% 
  dplyr::select(species, leaf_length_1, leaf_length_2, leaf_length_3, leaf_length_4, leaf_length_5) %>% 
  pivot_longer(cols = c(leaf_length_1, leaf_length_2, leaf_length_3, leaf_length_4, leaf_length_5), 
               values_to = "leaf_length") %>% 
  filter(!is.na(leaf_length)) %>% 
  group_by(species) %>% 
  summarize(leaf_length = mean(leaf_length, na.rm = TRUE), 
            n_samples_leaf_length = n())

summary(dt_ll)

### 5. Leaf Width ---------------------------------
dt_lw <- dt %>% 
  dplyr::select(species, leaf_width_1, leaf_width_2, leaf_width_3, leaf_width_4, leaf_width_5) %>% 
  pivot_longer(cols = c(leaf_width_1, leaf_width_2, leaf_width_3, leaf_width_4, leaf_width_5), 
               values_to = "leaf_width") %>% 
  filter(!is.na(leaf_width)) %>% 
  group_by(species) %>% 
  summarize(leaf_width = mean(leaf_width, na.rm = TRUE), 
            n_samples_leaf_width = n())

summary(dt_lw)

### 6. Plant Height ---------------------------------
dt_ph <- dt %>% 
  dplyr::select(species, plant_height_1, plant_height_2, plant_height_3, plant_height_4, plant_height_5) %>% 
  pivot_longer(cols = c(plant_height_1, plant_height_2, plant_height_3, plant_height_4, plant_height_5), 
               values_to = "plant_height") %>% 
  filter(!is.na(plant_height)) %>% 
  group_by(species) %>% 
  summarize(plant_height_max = max(plant_height, na.rm = TRUE), 
            plant_height_mean = mean(plant_height, na.rm = TRUE),
            n_samples_plant_height = n())
summary(dt_ph)
plot(dt_ph$plant_height_mean, dt_ph$plant_height_max)
cor.test(dt_ph$plant_height_mean, dt_ph$plant_height_max)

### 7. Leaf Size ---------------------------------

## size = length + width??

dt_lw_ls <- dt %>% 
  dplyr::select(species, leaf_width_1, leaf_width_2, leaf_width_3, leaf_width_4, leaf_width_5) %>% 
  pivot_longer(cols = c(leaf_width_1, leaf_width_2, leaf_width_3, leaf_width_4, leaf_width_5), 
               values_to = "leaf_width") %>% 
  dplyr::select(leaf_width)

dt_ll_ls <- dt %>% 
  dplyr::select(species, leaf_length_1, leaf_length_2, leaf_length_3, leaf_length_4, leaf_length_5) %>% 
  pivot_longer(cols = c(leaf_length_1, leaf_length_2, leaf_length_3, leaf_length_4, leaf_length_5), 
               values_to = "leaf_length")

dt_ls <- cbind(dt_ll_ls, dt_lw_ls) %>% 
  filter(!is.na(leaf_length)) %>% 
  filter(!is.na(leaf_width)) %>% 
  mutate(leaf_size = leaf_length + leaf_width,
         leaf_area = leaf_length*leaf_width) %>% 
  group_by(species) %>% 
  summarize(leaf_size= mean(leaf_size, na.rm = TRUE), 
            leaf_area= mean(leaf_area, na.rm = TRUE), 
            n_samples_leaf_size = n())
  

#hist(log(dt_ls$leaf_size))
#hist(log(dt_ls$leaf_area))
plot(log(dt_ls$leaf_size), log(dt_ls$leaf_area))

### 7. Combine ----------

dt_traits <- dt_lf_sp_lt %>% 
  left_join(dt_gf) %>% 
  left_join(dt_bd) %>% 
  left_join(dt_ll) %>% 
  left_join(dt_lw) %>% 
  left_join(dt_ph) %>% 
  left_join(dt_ls) %>% 
  as.data.table() %>% 
  mutate(leaf_type = ifelse(grepl("succulent", growth_form), "succulent", leaf_type), 
         biomass_density_ordinal = case_when(
           biomass_density == "low" ~ 1, 
           biomass_density == "medium" ~ 2, 
           biomass_density == "high" ~ 3, 
           biomass_density == "extremely_thick" ~ 4 
         ))
dt_traits[is.na(leaf_width)]

dt_traits[grepl("succulent", growth_form), leaf_type]

summary(dt_traits)

fwrite(dt_traits, "data/processed/fragments/species_trait_data_exclosures_2025.csv")
plot(dt_traits$leaf_length, dt_traits$leaf_area)
