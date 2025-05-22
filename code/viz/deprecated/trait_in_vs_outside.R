library(data.table)
library(tidyverse)
library(tidylog)
library(MetBrewer)

dt_plot <- fread("data/processed/fragments/plot_species_and_data.csv")
dt_traits <- fread("data/processed/fragments/species_trait_data_exclosures_2025.csv")
names(dt_traits)

dt <- dt_plot %>%
  dplyr::select(-life_form) %>% 
  left_join(dt_traits) %>% 
  mutate(biome = ifelse(setup_id %in% c("addo_nyathi_full", "addo_jack"), "thicket", "savanna"))


dt_long_cat <- dt %>% 
  pivot_longer(cols = c(leaf_type, life_form, 
                        growth_form_simple, biomass_density), 
               names_to = "trait_name", values_to = "trait_value") %>%
  mutate(group = paste(biome, in_or_out, sep = "_")) %>% 
  filter(!is.na(trait_value))

dt_long_num <- dt %>% 
  pivot_longer(cols = c(biomass_density_ordinal, 
                        leaf_length, leaf_width,
                        plant_height_max, plant_height_mean, 
                        leaf_area), 
               names_to = "trait_name", values_to = "trait_value") %>%
  mutate(group = paste(biome, in_or_out, sep = "_")) %>% 
  filter(!is.na(trait_value))


trait_freq_cat <- dt_long_cat %>%
  group_by(trait_name, trait_value, group) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(trait_name, group) %>%
  mutate(prop = count / sum(count)) 

# Plot
p_cat <- ggplot(trait_freq_cat, aes(x = trait_value, y = prop, fill = group)) +
  geom_col(position = "dodge") +
  facet_wrap(~ trait_name, scales = "free_x") +
  labs(
    x = "Trait Value",
    y = "Proportion",
    title = "Categorical trait"
  ) +
  theme_minimal() +
  scale_fill_met_d(name = "Egypt") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.background = element_rect(fill = "snow3", color = "snow3"))
p_cat


p_num <- ggplot(dt_long_num, aes(x = group, y = trait_value, fill = group)) +
  geom_boxplot(outlier.alpha = 0.2, alpha =0.99) +
  facet_wrap(~ trait_name, scales = "free_y") +
  labs(
    x = "Group",
    y = "Trait Value",
  ) +
  theme_minimal(base_size = 12) +
  scale_fill_met_d(name = "Egypt") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none", 
        strip.background = element_rect(fill = "snow3", color = "snow3"))
p_num

ggsave(plot = p_cat, "builds/plots/exploratory/comparison_categorical_traits.png", dpi = 600)
ggsave(plot = p_num, "builds/plots/exploratory/comparison_numeric_traits.png", dpi = 600)
