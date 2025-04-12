# clean plot data 

library(data.table)
library(tidyverse)
library(gridExtra)

plot_meta <-  fread("data/raw/plot_level_data_2025.csv")

plot_species <- fread("data/raw/plot_species_2025.csv") 
summary(plot_species)

dt_plot_raw <- plot_species %>% 
  left_join(plot_meta) %>% 
  mutate(plot_number = gsub("addo_n1_full_in_", "", plot_id), 
         plot_number = gsub("addo_n1_full_out_", "", plot_number),
         plot_number = gsub("addo_n2_full_in_", "", plot_number),
         plot_number = gsub("addo_n2_full_out_", "", plot_number),
         plot_number = gsub("addo_n3_full_in_", "", plot_number),
         plot_number = gsub("addo_n3_full_out_", "", plot_number),
         plot_number = gsub("addo_jack_in_", "", plot_number),
         plot_number = gsub("addo_jack_out_", "", plot_number),
         plot_number = gsub("pnr_in_", "", plot_number),
         plot_number = gsub("pnr_out_", "", plot_number),
         plot_number = gsub("knp_nkuhlu_full_in_", "", plot_number),
         plot_number = gsub("knp_nkuhlu_full_out_", "", plot_number),
         plot_number = gsub("knp_satara_in_", "", plot_number),
         plot_number = gsub("knp_satara_out_", "", plot_number),
         plot_number = gsub("knp_roan_in_", "", plot_number),
         plot_number = gsub("knp_roan_out_", "", plot_number), 
         plot_number = as.numeric(plot_number), 
         exclosure_id = gsub("_in", "",  site_id),
         exclosure_id = gsub("_out", "",  exclosure_id),
         exclosure_id = case_when(
           grepl("addo_n1", plot_id) ~ "addo_n1", 
           grepl("addo_n2", plot_id) ~ "addo_n2", 
           grepl("addo_n3", plot_id) ~ "addo_n3", 
           !grepl("addo_n", plot_id) ~ exclosure_id,
         ), 
         setup_id = gsub("_in", "",  site_id),
         setup_id = gsub("_out", "",  setup_id),
         pair_id = paste0(exclosure_id, "_", plot_number), 
         plot_number = case_when(
           plot_id %in% c("addo_n1_full_in_0", "addo_n1_full_out_0") ~ 1, 
           plot_id %in% c("addo_n1_full_in_1", "addo_n1_full_out_1") ~ 2,
           plot_id %in% c("addo_n1_full_in_2", "addo_n1_full_out_2") ~ 3, 
           plot_id %in% c("addo_n1_full_in_3", "addo_n1_full_out_3") ~ 4, 
           plot_id %in% c("addo_n1_full_in_4", "addo_n1_full_out_4") ~ 5, 
           plot_id %in% c("addo_n1_full_in_5", "addo_n1_full_out_5") ~ 6,
           plot_id %in% c("addo_n2_full_in_0", "addo_n2_full_out_0") ~ 7, 
           plot_id %in% c("addo_n2_full_in_1", "addo_n2_full_out_1") ~ 8,
           plot_id %in% c("addo_n2_full_in_2", "addo_n2_full_out_2") ~ 9, 
           plot_id %in% c("addo_n2_full_in_3", "addo_n2_full_out_3") ~ 10, 
           plot_id %in% c("addo_n2_full_in_4", "addo_n2_full_out_4") ~ 11, 
           plot_id %in% c("addo_n2_full_in_5", "addo_n2_full_out_5") ~ 12,
           plot_id %in% c("addo_n3_full_in_0", "addo_n3_full_out_0") ~ 13, 
           plot_id %in% c("addo_n3_full_in_1", "addo_n3_full_out_1") ~ 14,
           plot_id %in% c("addo_n3_full_in_2", "addo_n3_full_out_2") ~ 15, 
           plot_id %in% c("addo_n3_full_in_3", "addo_n3_full_out_3") ~ 16, 
           plot_id %in% c("addo_n3_full_in_4", "addo_n3_full_out_4") ~ 17, 
           plot_id %in% c("addo_n3_full_in_5", "addo_n3_full_out_5") ~ 18, 
           !grepl("addo_n", plot_id) ~ plot_number 
         ),
         cluster_number = case_when(
           plot_number %in% c(1:3) ~ 1,
           plot_number %in% c(4:6) ~ 2,
           plot_number %in% c(7:9) ~ 3,
           plot_number %in% c(10:12) ~ 4,
           plot_number %in% c(13:15) ~ 5,
           plot_number %in% c(16:18) ~ 6), 
         cluster_id = paste0(site_id, "_", cluster_number), 
         in_or_out = ifelse(grepl("out", site_id), "outside", "inside")
  ) %>% 
  group_by(plot_id) %>% 
  mutate(plant_richness_plot = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(cluster_id) %>% 
  mutate(plant_richness_cluster = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(site_id) %>% 
  mutate(plant_richness_site = n_distinct(species)) %>% 
  ungroup() 


dt_woody <- dt_plot_raw %>% 
  filter(life_form %in% c("Shrub", "Tree")) %>% 
  group_by(plot_id) %>% 
  mutate(woody_richness_plot = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(cluster_id) %>% 
  mutate(woody_richness_cluster = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(site_id) %>% 
  mutate(woody_richness_site = n_distinct(species)) %>% 
  ungroup() %>% 
  dplyr::select(plot_id, cluster_id, woody_richness_plot, woody_richness_cluster, woody_richness_site)  %>% unique()

dt_forb <- dt_plot_raw %>% 
  filter(life_form %in% c("Forb")) %>% 
  group_by(plot_id) %>% 
  mutate(forb_richness_plot = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(cluster_id) %>% 
  mutate(forb_richness_cluster = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(site_id) %>% 
  mutate(forb_richness_site = n_distinct(species)) %>% 
  ungroup() %>% 
  dplyr::select(plot_id, cluster_id, forb_richness_plot, forb_richness_cluster, forb_richness_site)  %>% unique()

dt_graminoid <- dt_plot_raw %>% 
  filter(life_form %in% c("Graminoid")) %>% 
  group_by(plot_id) %>% 
  mutate(graminoid_richness_plot = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(cluster_id) %>% 
  mutate(graminoid_richness_cluster = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(site_id) %>% 
  mutate(graminoid_richness_site = n_distinct(species)) %>% 
  ungroup() %>% 
  dplyr::select(plot_id, cluster_id, graminoid_richness_plot, graminoid_richness_cluster, graminoid_richness_site) %>% unique()


dt_plot <- dt_plot_raw %>% 
  left_join(dt_woody) %>% 
  left_join(dt_forb) %>% 
  left_join(dt_graminoid)

summary(dt_plot)

fwrite(dt_plot, "data/processed/fragments/plot_species_and_data.csv")

mean(dt_plot[dt_plot$in_or_out == "outside", ]$woody_height, na.rm = T)
mean(dt_plot[dt_plot$in_or_out == "inside", ]$woody_height, na.rm = T)
sd(dt_plot[dt_plot$in_or_out == "outside", ]$woody_height, na.rm = T)
sd(dt_plot[dt_plot$in_or_out == "inside", ]$woody_height, na.rm = T)

mean(dt_plot[dt_plot$in_or_out == "outside", ]$herb_height, na.rm = T)
mean(dt_plot[dt_plot$in_or_out == "inside", ]$herb_height, na.rm = T)
