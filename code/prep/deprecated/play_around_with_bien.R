library(BIEN)
library(ape) #Package for working with phylogenies in R
library(maps) #Useful for making quick maps of occurrences
library(sf)


dt_traits <- fread("data/processed/fragments/species_trait_data_exclosures_2025.csv") %>%
  mutate(leaf_area = ifelse(leaf_type == "a", 0, leaf_area)) %>% # absent leaves have an area of 0... 
  mutate(
    growth_form = as.factor(growth_form_simple),
    growth_form = factor(growth_form, levels = c(
      "round_herb",  "messy_herb", "creeping_herb",         
      "climbing_herb", "cushion_herb",        
      "parasitic_herb", "straight_herb", 
      "single_stemmed_woody", "multi_stemmed_woody")),
    spines = as.factor(spines),
    spines = factor(spines, levels = c("n", "<2", ">2")),
    biomass_density_ordinal = factor(biomass_density_ordinal,
                                     levels = sort(unique(biomass_density_ordinal)),
                                     ordered = TRUE),
    leaf_type = as.factor(leaf_type), 
    leaf_type = factor(leaf_type, levels = c(
      "absent", "linear", "simple", "lobed",
      "palmately_compound", "pinnately_compound", "succulent")),
    plant_height_max = as.numeric(plant_height_max), 
    leaf_area = as.numeric(leaf_area)) %>%
  dplyr::select(species,
                plant_height_max, leaf_area,
                growth_form, spines, biomass_density_ordinal, leaf_type) %>% 
  unique() %>%
  filter(complete.cases(.))


dt_traits$species

BIEN_trait_list()

dt_bien_traits <- BIEN_trait_species(species = c(dt_traits$species)) %>% 
  rename(species = scrubbed_species_binomial) %>% 
  filter(trait_name %in% c(
    "leaf area per leaf dry mass", 
    "leaf carbon content per leaf dry mass",
    "leaf dry mass per leaf fresh mass",
    "leaf carbon content per leaf nitrogen content",
    "leaf nitrogen content per leaf dry mass",
    "leaf area",
    "seed mass",
   " whole plant height"
  )) %>% 
  mutate(trait_name = gsub(" ", "_", trait_name), 
         trait_value = as.numeric(trait_value)) %>% 
  group_by(species, trait_name) %>% 
  summarize(trait_value = mean(trait_value))


dt_bien_wide = dt_bien_traits %>% 
  pivot_wider(names_from = "trait_name", 
              id_cols = "species", 
              values_from = trait_value) %>% 
  ungroup() %>% 
  filter(complete.cases(.))

table(dt_bien_traits$species)
