library(sf)
library(mapview)
library(tidyverse)
library(data.table)
library(scico)
library(gridExtra)
library(ggcorrplot)
library(ggridges)

## 1. Boxplots -------------------------------------------

dt_box <- fread("data/processed/clean/all_vars.csv") %>% 
  pivot_longer(
    cols = c(
             plant_richness_plot, plant_richness_site,
             woody_richness_plot, plant_richness_site,
             forb_richness_plot, plant_richness_site,
             graminoid_richness_plot,  graminoid_richness_site,
             
             berger_parker_plot, berger_parker_site, 

             functional_dispersion_plot, functional_dispersion_site, 
             functional_specialization_plot, functional_specialization_site, 
             functional_diversity_plot, functional_diversity_site, 
             functional_nearerst_neighbour_distance_plot, functional_nearerst_neighbour_distance_site, 
             
             plant_evenness_plot, plant_evenness_site,
             shannon_diversity_plot, shannon_diversity_site, 
             
             point_return_fraction_plot, point_return_fraction_site, 
             mean_point_height_plot, mean_point_height_site), 
    names_to = "response_name", values_to = "response_value") %>% 
  mutate(
    clean_response = gsub("_plot", "", response_name),
    clean_response = gsub("_cluster", "", clean_response),
    clean_response = gsub("_site", "", clean_response),
    clean_response = case_when(
      clean_response == "plant_richness" ~ "Plant Richness", 
      clean_response == "woody_richness" ~ "Woody Richness", 
      clean_response == "forb_richness" ~ "Forb Richness", 
      clean_response == "graminoid_richness" ~ "Graminoid Richness", 
      clean_response == "shannon_diversity" ~ "Shannon Diversity", 
      clean_response == "berger_parker" ~ "Plant Dominance", 
      clean_response == "plant_evenness" ~ "Plant Evenness", 
      clean_response == "point_return_fraction" ~ "Vegetation Density", 
      clean_response == "mean_point_height" ~ "Vegetation Height", 
      clean_response == "functional_nearerst_neighbour_distance" ~ "Plant Functional Distance", 
      clean_response == "functional_diversity" ~ "Plant Functional Diversity", 
      clean_response == "functional_specialization" ~ "Plant Functional Specialization", 
      clean_response == "functional_dispersion" ~ "Plant Functional Dispersion", 
    ), 
    clean_response = factor(clean_response, levels = c(
      "Plant Richness", "Shannon Diversity", "Graminoid Richness", "Forb Richness", "Woody Richness",
      "Plant Dominance", "Plant Evenness",
      "Vegetation Density", "Vegetation Height",
      "Plant Functional Diversity", "Plant Functional Distance",
      "Plant Functional Specialization", "Plant Functional Dispersion"
    ))) 

p_p_box <- dt_box %>% 
  dplyr::select(response_value, clean_response, response_name, in_or_out) %>% 
  filter(grepl("plot", response_name)) %>% 
  distinct() %>% 
  ggplot(aes(x = in_or_out, y = response_value, color = in_or_out)) +
  geom_boxplot(outlier.shape = NA, width = 0.6, alpha = 0.9, color = "grey25", fill = "white") +
  geom_jitter(width = 0.2, alpha = 0.6, size = 1.5) +
  geom_boxplot(outlier.shape = NA, width = 0.6, alpha = 0.5, color = "grey25", fill = "white") +
  scale_color_scico_d(palette = "bam") +
  facet_wrap(~ clean_response, scales = "free_y", ncol = 5) +
  labs(x = NULL, y = "Response Value", title = "a)", subtitle = "Plot-Scale") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(face = "bold", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )
p_p_box

p_s_box <- dt_box %>% 
  dplyr::select(response_value, clean_response, response_name, in_or_out) %>% 
  filter(grepl("site", response_name)) %>% 
  distinct() %>% 
  ggplot(aes(x = in_or_out, y = response_value, color = in_or_out)) +
  geom_boxplot(outlier.shape = NA, width = 0.6, alpha = 0.9, color = "grey25", fill = "white") +
  geom_jitter(width = 0.2, alpha = 0.6, size = 1.5) +
  geom_boxplot(outlier.shape = NA, width = 0.6, alpha = 0.5, color = "grey25", fill = "white") +
  scale_color_scico_d(palette = "bam") +
  facet_wrap(~ clean_response, scales = "free_y", ncol = 5) +
  labs(x = NULL, y = "Response Value", title = "b)", subtitle = "Site-Scale") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(face = "bold", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )
p_s_box

p_box <- gridExtra::grid.arrange(p_p_box, p_s_box, ncol = 1)
ggsave(plot = p_box, "builds/plots/supplement/response_boxplots.png", dpi = 600, height = 12, width = 12)

# 2. Correlations ----------------------------------------------------

#Raw Data 
dt_co_p <- fread("data/processed/clean/all_vars.csv") %>% 
  dplyr::select(plant_richness_plot,
                woody_richness_plot, 
                forb_richness_plot, 
                graminoid_richness_plot,
                
                berger_parker_plot, 
                
                functional_dispersion_plot,
                functional_specialization_plot,
                functional_diversity_plot, 
                functional_nearerst_neighbour_distance_plot, 
                
                plant_evenness_plot, 
                shannon_diversity_plot,
                
                point_return_fraction_plot, 
                mean_point_height_plot) %>% 
  rename(
    "Plant Richness" = plant_richness_plot,
    "Woody Richness" = woody_richness_plot,
    "Forb Richness" = forb_richness_plot,
    "Graminoid Richness" = graminoid_richness_plot,
    "Shannon Diversity" = shannon_diversity_plot,
    "Plant Dominance" = berger_parker_plot,
    "Plant Evenness" = plant_evenness_plot,
    "Vegetation Density" = point_return_fraction_plot,
    "Vegetation Height" = mean_point_height_plot,
    "Plant Functional Distance" = functional_nearerst_neighbour_distance_plot,
    "Plant Functional Diversity" = functional_diversity_plot,
    "Plant Functional Specialization" = functional_specialization_plot,
    "Plant Functional Dispersion" = functional_dispersion_plot
  ) %>% filter(complete.cases(.))

p_corr <- round(cor(dt_co_p), 1)
p_plot_corr <- ggcorrplot(p_corr, hc.order = TRUE, type = "lower",
           lab = TRUE, 
           colors = c("#6D9EC1", "white", "#E46726")) +
  labs(title = "a)", subtitle = "Raw Data Correlations") +
  theme(plot.subtitle = element_text(size = 12, hjust = 0.5))
p_plot_corr

#Effect Sizes 
dt_co_p_es <- fread("data/processed/clean/long_data_with_lnrr.csv") %>% 
  dplyr::select(c(response_name, ln_rr, pair_id, cluster_id, exclosure_id, setup_id)) %>% 
  pivot_wider(names_from = "response_name", values_from = "ln_rr", id_cols = c(pair_id, cluster_id, exclosure_id, setup_id)) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .))) %>% 
  as.data.table() %>%
  dplyr::select(plant_richness_plot,
                woody_richness_plot, 
                forb_richness_plot, 
                graminoid_richness_plot,
                
                berger_parker_plot, 
                
                functional_dispersion_plot,
                functional_specialization_plot,
                functional_diversity_plot, 
                functional_nearerst_neighbour_distance_plot, 
                
                plant_evenness_plot, 
                shannon_diversity_plot,
                
                point_return_fraction_plot, 
                mean_point_height_plot) %>% 
  rename(
    "Plant Richness" = plant_richness_plot,
    "Woody Richness" = woody_richness_plot,
    "Forb Richness" = forb_richness_plot,
    "Graminoid Richness" = graminoid_richness_plot,
    "Shannon Diversity" = shannon_diversity_plot,
    "Plant Dominance" = berger_parker_plot,
    "Plant Evenness" = plant_evenness_plot,
    "Vegetation Density" = point_return_fraction_plot,
    "Vegetation Height" = mean_point_height_plot,
    "Plant Functional Distance" = functional_nearerst_neighbour_distance_plot,
    "Plant Functional Diversity" = functional_diversity_plot,
    "Plant Functional Specialization" = functional_specialization_plot,
    "Plant Functional Dispersion" = functional_dispersion_plot
  ) %>% filter(complete.cases(.))

p_corr_es <- round(cor(dt_co_p_es), 1)
p_plot_corr_es <- ggcorrplot(p_corr_es, hc.order = TRUE, type = "lower",
                          lab = TRUE, 
                          colors = c("#6D9EC1", "white", "#E46726")) +
  labs(title = "b)", subtitle = "Effect-Size Correlations") +
  theme(plot.subtitle = element_text(size = 12, hjust = 0.5))
p_plot_corr_es

corr_plot <- grid.arrange(p_plot_corr, p_plot_corr_es, ncol = 1)
ggsave(plot = corr_plot, "builds/plots/supplement/corr_plots.png", dpi = 600, height = 12, width = 10)


# 3. Cover differences between biomes  

dt_cd <- fread("data/processed/clean/all_vars.csv")%>%
  mutate(biome = ifelse(setup_id %in% c("addo_nyathi_full", "addo_jack"), "Thicket", "Savanna"), 
         percent_woody = (woody_richness_plot / plant_richness_plot)*100)

p1 <- dt_cd %>%
  ggplot() +
  geom_boxplot(aes(x = biome, y = woody_richness_plot), outlier.shape = NA) +
  geom_jitter(aes(x = biome, y = woody_richness_plot), width = 0.2, alpha = 0.3) +
  labs(x = "", y = "Woody Richness") +
  theme_classic()
p1

p2 <- dt_cd %>%
  ggplot() +
  geom_boxplot(aes(x = biome, y = graminoid_richness_plot), outlier.shape = NA) +
  geom_jitter(aes(x = biome, y = graminoid_richness_plot), width = 0.2, alpha = 0.3) +
  labs(x = "", y = "Graminoid Richness") +
  theme_classic()
p2

p3 <- dt_cd %>%
  ggplot() +
  geom_boxplot(aes(x = biome, y = forb_richness_plot), outlier.shape = NA) +
  geom_jitter(aes(x = biome, y = forb_richness_plot), width = 0.2, alpha = 0.3) +
  labs(x = "", y = "Forb Richness") +
  theme_classic()
p3

p4 <- dt_cd %>%
  ggplot() +
  geom_boxplot(aes(x = biome, y = percent_woody), outlier.shape = NA) +
  geom_jitter(aes(x = biome, y = percent_woody), width = 0.2, alpha = 0.3) +
  labs(x = "", y = "% Woody Species") +
  theme_classic()
p4

p5 <- dt_cd %>%
  ggplot() +
  geom_boxplot(aes(x = biome, y = woody_cover_percent), outlier.shape = NA) +
  geom_jitter(aes(x = biome, y = woody_cover_percent), width = 0.2, alpha = 0.3) +
  labs(x = "", y = "Woody Species Cover (%)") +
  theme_classic()
p5

p6 <- dt_cd %>%
  ggplot() +
  geom_boxplot(aes(x = biome, y = graminoid_cover_percent), outlier.shape = NA) +
  geom_jitter(aes(x = biome, y = graminoid_cover_percent), width = 0.2, alpha = 0.3) +
  labs(x = "", y = "Graminoid Cover (%)") +
  theme_classic()
p6

p7 <- dt_cd %>%
  ggplot() +
  geom_boxplot(aes(x = biome, y = forb_cover_percent), outlier.shape = NA) +
  geom_jitter(aes(x = biome, y = forb_cover_percent), width = 0.2, alpha = 0.3) +
  labs(x = "", y = "Forb Cover (%)") +
  theme_classic()
p7

library(gridExtra)
p_biome_diff <- grid.arrange(p1, p2, p3, p4, p5, p6, p7, ncol = 4)
ggsave(plot = p_biome_diff, "builds/plots/supplement/biome_differences_woody_plants.png", dpi = 600, height = 5, width = 10)

# 4 Life-Form Specific Dominance ------------------

dt_dom <- fread("data/processed/fragments/plot_species_and_data.csv")%>%
  mutate(biome = ifelse(setup_id %in% c("addo_nyathi_full", "addo_jack"), "Thicket", "Savanna"), 
         life_form = ifelse(life_form %in% c("Tree", "Shrub"), "Woody", life_form), 
         percent_woody = (woody_richness_plot / plant_richness_plot)*100)

mean(dt_dom[life_form == "Woody", ]$cover)
sd(dt_dom[life_form == "Woody", ]$cover)

mean(dt_dom[life_form == "Graminoid", ]$cover)
sd(dt_dom[life_form == "Graminoid", ]$cover)

mean(dt_dom[life_form == "Forb", ]$cover)
sd(dt_dom[life_form == "Forb", ]$cover)


p_dom <- dt_dom %>% 
  ggplot() +
  geom_boxplot(aes(x = life_form, y = cover), outlier.shape = NA) +
  geom_jitter(aes(x = life_form, y = cover), width = 0.2, alpha = 0.3) +
  labs(x = "", y = "Percentage Cover") +
  theme_classic()

p_dom

ggsave(plot = p_dom, "builds/plots/supplement/life_form_cover.png", dpi = 600, height = 3, width = 3)

# 5. LNRR Ridges -------------
dt_rr <- fread("data/processed/clean/long_data_with_lnrr.csv") %>% 
  filter(response_name %in% c(
    "plant_richness_plot",
    "woody_richness_plot", 
    "forb_richness_plot", 
    "graminoid_richness_plot",
    
    "berger_parker_plot", 
    
    "functional_dispersion_plot",
    "functional_specialization_plot",
    "functional_diversity_plot", 
    "functional_nearerst_neighbour_distance_plot", 
    
    "plant_evenness_plot", 
    "shannon_diversity_plot",
    
    "point_return_fraction_plot", 
    "mean_point_height_plot", 
    "plant_richness_site",
    "woody_richness_site", 
    "forb_richness_site", 
    "graminoid_richness_site",
    
    "berger_parker_site", 
    
    "functional_dispersion_site",
    "functional_specialization_site",
    "functional_diversity_site", 
    "functional_nearerst_neighbour_distance_site", 
    
    "plant_evenness_site", 
    "shannon_diversity_site",
    
    "point_return_fraction_site", 
    "mean_point_height_site"
  )) %>% 
  mutate(
    clean_response = gsub("_plot", "", response_name),
    clean_response = gsub("_cluster", "", clean_response),
    clean_response = gsub("_site", "", clean_response),
    clean_response = case_when(
      clean_response == "plant_richness" ~ "Plant Richness", 
      clean_response == "woody_richness" ~ "Woody Richness", 
      clean_response == "forb_richness" ~ "Forb Richness", 
      clean_response == "graminoid_richness" ~ "Graminoid Richness", 
      clean_response == "shannon_diversity" ~ "Shannon Diversity", 
      clean_response == "berger_parker" ~ "Plant Dominance", 
      clean_response == "plant_evenness" ~ "Plant Evenness", 
      clean_response == "point_return_fraction" ~ "Vegetation Density", 
      clean_response == "mean_point_height" ~ "Vegetation Height", 
      clean_response == "functional_nearerst_neighbour_distance" ~ "Plant Functional Distance", 
      clean_response == "functional_diversity" ~ "Plant Functional Diversity", 
      clean_response == "functional_specialization" ~ "Plant Functional Specialization", 
      clean_response == "functional_dispersion" ~ "Plant Functional Dispersion", 
    ), 
    clean_response = factor(clean_response, levels = c(
      "Plant Richness", "Shannon Diversity", "Graminoid Richness", "Forb Richness", "Woody Richness",
      "Plant Dominance", "Plant Evenness",
      "Vegetation Density", "Vegetation Height",
      "Plant Functional Diversity", "Plant Functional Distance",
      "Plant Functional Specialization", "Plant Functional Dispersion"
    ))) 

p_ridges <- dt_rr %>%
  filter(!is.infinite(ln_rr)) %>%
  mutate(clean_response = fct_rev(clean_response)) %>%
  ggplot(aes(x = ln_rr, y = clean_response, fill = after_stat(x))) +
  geom_density_ridges_gradient(rel_min_height = 0.01, color = "grey", size = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_scico(palette = "bam", midpoint = 0, name = "Log Response Ratio") +
  facet_wrap(~scale, scales = "free_x") +
  labs(x = "Log Response Ratio",y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggsave(plot = p_ridges, "builds/plots/supplement/ln_rr_ridges.png", dpi = 600, height = 5, width = 8)

