## Combine all and calculate effect sizes 

library(data.table)
library(tidyverse)


#1. Load --------------------

## taxonomic diversity 
dt_sp <- fread("data/processed/fragments/plot_species_and_data.csv")
names(dt_sp)

dt_tax <- dt_sp %>% 
  dplyr::select(-c(species, cover, life_form)) %>% 
  unique()

## functional diversity 

dt_fun <- fread("data/processed/fragments/functional_diversity_metrics.csv")

## lidar 

dt_lid <- fread("data/processed/fragments/lidar_metrics_750cm_radius.csv") %>% 
  dplyr::select(mean_3d, adjusted_mean_3d, sd_3d, point_fraction, sd_height, mean_height, plot_id) %>% 
  left_join(dt_sp %>% dplyr::select(plot_id, site_id, cluster_id)) %>% 
  rename(mean_point_distance_plot = mean_3d, 
         sd_point_distance_plot = sd_3d, 
         adjusted_mean_point_distance_plot = adjusted_mean_3d, 
         point_return_fraction_plot = point_fraction, 
         mean_point_height_plot = mean_height, 
         sd_point_height_plot = sd_height) %>% 
  group_by(cluster_id) %>% 
  mutate(mean_point_distance_cluster = mean(mean_point_distance_plot, na.rm = T), 
         sd_point_distance_cluster = mean(sd_point_distance_plot, na.rm = T), 
         adjusted_mean_point_distance_cluster = mean(adjusted_mean_point_distance_plot, na.rm = T), 
         point_return_fraction_cluster = mean(point_return_fraction_plot, na.rm = T), 
         mean_point_height_cluster = mean(mean_point_height_plot, na.rm = T), 
         sd_point_height_cluster = mean(sd_point_height_plot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(site_id) %>% 
  mutate(mean_point_distance_site = mean(mean_point_distance_cluster, na.rm = T), 
         sd_point_distance_site = mean(sd_point_distance_cluster, na.rm = T), 
         adjusted_mean_point_distance_site = mean(adjusted_mean_point_distance_cluster, na.rm = T), 
         point_return_fraction_site = mean(point_return_fraction_cluster, na.rm = T), 
         mean_point_height_site = mean(mean_point_height_cluster, na.rm = T), 
         sd_point_height_site = mean(sd_point_height_cluster, na.rm = T)) %>% 
  ungroup()

# environmental
dt_env <- fread("data/processed/fragments/plot_environmental.csv")

## 2. Combine --------------------

dt_comb <- dt_tax %>% 
  left_join(dt_fun) %>% 
  left_join(dt_lid) %>% 
  left_join(dt_env) %>% 
  group_by(setup_id) %>% 
  mutate(npp = mean(npp_plot, na.rm = T),
         ndvi = mean(ndvi_plot, na.rm = T),
         evi = mean(evi_plot, na.rm = T),
         mat = mean(mat_plot, na.rm = T),
         map = mean(map_plot, na.rm = T)) %>% 
  ungroup()
glimpse(dt_comb)

fwrite(dt_comb, "data/processed/clean/all_vars.csv")
## 3. Calculate effect sizes --------------------

dt_out <- dt_comb %>%
  filter(grepl("out", plot_id)) %>%
  dplyr::select(-c(plot_id, site_id, cluster_id, in_or_out,
                   n_forb_flowers, n_grass_flowers, 
                   time, date,
                   rock_cover, notes, herbivore_signs, lidar_scanner_height)) %>%
  unique() %>% 
  pivot_longer(
    cols = c(bare_ground_cover, woody_height, herb_height,
             
             plant_richness_plot, plant_richness_cluster, plant_richness_site,
             woody_richness_plot, woody_richness_cluster, woody_richness_site,
             forb_richness_plot, forb_richness_cluster, forb_richness_site,
             graminoid_richness_plot, graminoid_richness_cluster, graminoid_richness_site,
             
             community_dominance_plot, community_dominance_cluster, community_dominance_site,
             berger_parker_plot, berger_parker_cluster, berger_parker_site,
             simpson_dominance_plot, simpson_dominance_cluster, simpson_dominance_site,
             
             functional_diversity_plot, functional_diversity_cluster, functional_diversity_site,
             functional_dispersion_plot, functional_dispersion_cluster, functional_dispersion_site,
             functional_specialization_plot, functional_specialization_cluster, functional_specialization_site,
             functional_pairwise_distance_plot, functional_pairwise_distance_cluster, functional_pairwise_distance_site,
             functional_nearerst_neighbour_distance_plot, functional_nearerst_neighbour_distance_cluster, functional_nearerst_neighbour_distance_site,
             
             functional_beta_diversity_cluster, functional_beta_diversity_site, 
             sorenson_dissimilarity_cluster, sorenson_dissimilarity_site,
             
             plant_evenness_plot, plant_evenness_cluster, plant_evenness_site,
             shannon_diversity_plot, shannon_diversity_cluster, shannon_diversity_site,
             
             mean_point_distance_plot, mean_point_distance_cluster, mean_point_distance_site,
             adjusted_mean_point_distance_plot, adjusted_mean_point_distance_cluster, adjusted_mean_point_distance_site,
             sd_point_distance_plot, sd_point_distance_cluster, sd_point_distance_site,
             point_return_fraction_plot, point_return_fraction_cluster, point_return_fraction_site,
             mean_point_height_plot, mean_point_height_cluster, mean_point_height_site,
             sd_point_height_plot, sd_point_height_cluster, sd_point_height_site
             
),
    names_to = "response_name", values_to = "response_value_out") %>% 
  unique() %>%
  dplyr::select(response_name, response_value_out, pair_id, cluster_number) %>% unique()

names(dt_out)

dt_in <- dt_comb %>%
  filter(grepl("in", plot_id)) %>%
  dplyr::select(-c(plot_id, site_id, cluster_id, in_or_out,
                   n_forb_flowers, n_grass_flowers, 
                   time, date,
                   rock_cover, notes, herbivore_signs, lidar_scanner_height)) %>%
  unique() %>% 
  pivot_longer(
    cols = c(bare_ground_cover, woody_height, herb_height,
             
             plant_richness_plot, plant_richness_cluster, plant_richness_site,
             woody_richness_plot, woody_richness_cluster, woody_richness_site,
             forb_richness_plot, forb_richness_cluster, forb_richness_site,
             graminoid_richness_plot, graminoid_richness_cluster, graminoid_richness_site,
             
             community_dominance_plot, community_dominance_cluster, community_dominance_site,
             berger_parker_plot, berger_parker_cluster, berger_parker_site,
             simpson_dominance_plot, simpson_dominance_cluster, simpson_dominance_site,
             
             functional_dispersion_plot, functional_dispersion_cluster, functional_dispersion_site,
             functional_specialization_plot, functional_specialization_cluster, functional_specialization_site,
             functional_diversity_plot, functional_diversity_cluster, functional_diversity_site,
             functional_pairwise_distance_plot, functional_pairwise_distance_cluster, functional_pairwise_distance_site,
             functional_nearerst_neighbour_distance_plot, functional_nearerst_neighbour_distance_cluster, functional_nearerst_neighbour_distance_site,
             
             functional_beta_diversity_cluster, functional_beta_diversity_site, 
             sorenson_dissimilarity_cluster, sorenson_dissimilarity_site,
             
             plant_evenness_plot, plant_evenness_cluster, plant_evenness_site,
             shannon_diversity_plot, shannon_diversity_cluster, shannon_diversity_site,
             
             mean_point_distance_plot, mean_point_distance_cluster, mean_point_distance_site,
             adjusted_mean_point_distance_plot, adjusted_mean_point_distance_cluster, adjusted_mean_point_distance_site,
             sd_point_distance_plot, sd_point_distance_cluster, sd_point_distance_site,
             point_return_fraction_plot, point_return_fraction_cluster, point_return_fraction_site,
             mean_point_height_plot, mean_point_height_cluster, mean_point_height_site,
             sd_point_height_plot, sd_point_height_cluster, sd_point_height_site
             
    ),
    names_to = "response_name", values_to = "response_value_in") %>% 
  unique() %>%
  dplyr::select(response_name, response_value_in, pair_id, cluster_number, setup_id, exclosure_id, 
                mat, map, npp, ndvi, evi) %>% unique()


dt_rr <- dt_in %>% left_join(dt_out) %>% 
  mutate(ln_rr = log(response_value_out/response_value_in), # that way, it's larger values represent positive effect of megafauna
         rr = response_value_out/response_value_in) %>% 
  mutate(scale = case_when(
    grepl("plot", response_name) ~ "plot", 
    grepl("cluster", response_name) ~ "cluster", 
    grepl("site", response_name) ~ "site", 
  ), 
  cluster_id = paste0(setup_id, "_cluster_", cluster_number))

dt_out$response_value_out
plot(dt_rr$ln_rr, dt_rr$rr)

fwrite(dt_rr, "data/processed/clean/long_data_with_lnrr.csv")

dt_rr %>% 
  filter(scale == "plot") %>% 
  ggplot() +
  geom_vline(xintercept = 0) +
  geom_jitter(aes(x = ln_rr, y = response_name, color = map), alpha = 0.5, height = 0.1) +
  theme_minimal() +
  theme(legend.position = "none")

dt_rr %>% 
  filter(scale == "cluster") %>% 
  dplyr::select(ln_rr, response_name, setup_id) %>% 
  unique() %>% 
  ggplot() +
  geom_vline(xintercept = 0) +
  geom_jitter(aes(x = ln_rr, y = response_name, color = setup_id), alpha = 0.5, height = 0.1) +
  theme_minimal() +
  theme(legend.position = "none")

dt_rr %>% 
  filter(scale == "site") %>% 
  dplyr::select(ln_rr, response_name, setup_id) %>% 
  unique() %>% 
  ggplot() +
  geom_vline(xintercept = 0) +
  geom_jitter(aes(x = ln_rr, y = response_name, color = setup_id), alpha = 0.5, height = 0.1) +
  theme_minimal() +
  theme(legend.position = "none")
