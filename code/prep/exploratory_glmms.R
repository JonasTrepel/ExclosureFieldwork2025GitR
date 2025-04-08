
library(data.table)
library(tidyverse)
library(gridExtra)

plot_meta <-  fread("data/raw/plot_level_data_2025.csv")

plot_species <- fread("data/raw/plot_species_2025.csv") 


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
  mutate(n_species_plot = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(cluster_id) %>% 
  mutate(n_species_cluster = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(site_id) %>% 
  mutate(n_species_site = n_distinct(species)) %>% 
  ungroup() 


dt_woody <- dt_plot_raw %>% 
  filter(life_form %in% c("Shrub", "Tree")) %>% 
  group_by(plot_id) %>% 
  mutate(n_woodies_plot = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(cluster_id) %>% 
  mutate(n_woodies_cluster = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(site_id) %>% 
  mutate(n_woodies_site = n_distinct(species)) %>% 
  ungroup() %>% 
  dplyr::select(plot_id, cluster_id, n_woodies_plot, n_woodies_cluster, n_woodies_site)  %>% unique()

dt_forb <- dt_plot_raw %>% 
  filter(life_form %in% c("Forb")) %>% 
  group_by(plot_id) %>% 
  mutate(n_forbs_plot = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(cluster_id) %>% 
  mutate(n_forbs_cluster = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(site_id) %>% 
  mutate(n_forbs_site = n_distinct(species)) %>% 
  ungroup() %>% 
  dplyr::select(plot_id, cluster_id, n_forbs_plot, n_forbs_cluster, n_forbs_site)  %>% unique()

dt_graminoid <- dt_plot_raw %>% 
  filter(life_form %in% c("Graminoid")) %>% 
  group_by(plot_id) %>% 
  mutate(n_graminoids_plot = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(cluster_id) %>% 
  mutate(n_graminoids_cluster = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(site_id) %>% 
  mutate(n_graminoids_site = n_distinct(species)) %>% 
  ungroup() %>% 
  dplyr::select(plot_id, cluster_id, n_graminoids_plot, n_graminoids_cluster, n_graminoids_site) %>% unique()


dt_plot <- dt_plot_raw %>% 
  left_join(dt_woody) %>% 
  left_join(dt_forb) %>% 
  left_join(dt_graminoid)



dt_out <- dt_plot %>%
  filter(grepl("out", plot_id)) %>%
  dplyr::select(-c(species, cover, life_form, in_or_out,
                   plot_id, time, date, rock_cover, notes, herbivore_signs, lidar_scanner_height)) %>%
  unique() %>% 
  pivot_longer(
    cols = c("n_forb_flowers", "n_grass_flowers",
             "bare_ground_cover", 
             "woody_height", "herb_height",
             "n_species_plot", "n_species_cluster", "n_species_site",
             "n_woodies_plot", "n_woodies_cluster", "n_woodies_site",
             "n_forbs_plot", "n_forbs_cluster", "n_forbs_site", 
             "n_graminoids_plot", "n_graminoids_cluster", "n_graminoids_site"),
    names_to = "response_name", values_to = "response_value_out") %>% 
  unique() %>%
  dplyr::select(response_name, response_value_out, pair_id)

names(dt_out)

dt_in <- dt_plot %>%
  filter(grepl("in", plot_id)) %>%
  dplyr::select(-c(species, cover, life_form, in_or_out,
                   plot_id, time, date, rock_cover, notes, herbivore_signs, lidar_scanner_height)) %>%
  unique() %>% 
  pivot_longer(
    cols = c("n_forb_flowers", "n_grass_flowers",
             "bare_ground_cover",
             "woody_height", "herb_height",
             "n_species_plot", "n_species_cluster", "n_species_site",
             "n_woodies_plot", "n_woodies_cluster", "n_woodies_site",
             "n_forbs_plot", "n_forbs_cluster", "n_forbs_site", 
             "n_graminoids_plot", "n_graminoids_cluster", "n_graminoids_site"),
    names_to = "response_name", values_to = "response_value_in") %>% 
  unique()


dt_comb <- dt_in %>% left_join(dt_out) %>% 
  mutate(ln_rr = log(response_value_out/response_value_in),
         rr = response_value_out/response_value_in)

dt_out$response_value_out
dt_comb$response_value_out
plot(dt_comb$ln_rr, dt_comb$rr)


dt_comb %>% ggplot() +
  geom_vline(xintercept = 0) +
  geom_jitter(aes(x = ln_rr, y = response_name, color = exclosure_id), alpha = 0.5, height = 0.1) +
  theme_minimal()

library(glmmTMB)
guide <- dt_comb %>% 
  mutate(formula = case_when(
    #plot level
    response_name %in% c("n_forb_flowers", "n_grass_flowers", 
                         "bare_ground_cover",
                    "woody_height", "herb_height", 
                    "n_species_plot", "n_woodies_plot", 
                    "n_forbs_plot", "n_graminoids_plot") ~ "1 + (1 | exclosure_id/cluster_id)",
    #cluster level
    response_name %in% c("n_species_cluster", "n_woodies_cluster",
                         "n_graminoids_cluster", "n_forbs_cluster") ~ "1 + (1 | exclosure_id)", 
    #site_level
    response_name %in% c("n_species_site", "n_woodies_site",
                         "n_graminoids_site", "n_forbs_site") ~ "1"),
    scale = case_when(
      #plot level
      response_name %in% c("n_forb_flowers", "n_grass_flowers", 
                           "bare_ground_cover",
                           "woody_height", "herb_height", 
                           "n_species_plot", "n_woodies_plot", 
                           "n_forbs_plot", "n_graminoids_plot") ~ "plot",
      #cluster level
      response_name %in% c("n_species_cluster", "n_woodies_cluster",
                           "n_graminoids_cluster", "n_forbs_cluster") ~ "cluster", 
      #site_level
      response_name %in% c("n_species_site", "n_woodies_site",
                           "n_graminoids_site", "n_forbs_site") ~ "site")
  ) %>% 
  dplyr::select(formula, response_name, scale) %>% 
  unique()


library(brms)
library(tidybayes)
library(broom)
estimates <- data.table()
dt_points <- data.table()

for(response in unique(guide$response_name)){
  
  sca <- unique(guide[guide$response_name == response, ]$scale)
  form <- unique(guide[guide$response_name == response, ]$formula)
  response
  
  if(sca == "plot"){
    
    dt_mod <- dt_comb %>% 
      filter(response_name %in% c(response)) %>% 
      dplyr::select(site_id, exclosure_id, cluster_id, ln_rr) %>% 
      unique() %>% 
      filter(!is.na(ln_rr)) %>% 
      filter(!is.infinite(ln_rr))
    
    n <- nrow(dt_mod)
    
    m <- brm(as.formula(paste0("ln_rr ~ ", form)), data = dt_mod)
  }else if(sca == "cluster"){
    
    dt_mod <- dt_comb %>% 
      filter(response_name %in% c(response)) %>% 
      dplyr::select(site_id, exclosure_id, ln_rr) %>% 
      unique() %>% 
      filter(!is.na(ln_rr)) %>% 
      filter(!is.infinite(ln_rr))
    
    n <- nrow(dt_mod)
    
    m <- brm(as.formula(paste0("ln_rr ~ ", form)), data = dt_mod)
  }else if(sca == "site"){
    
    dt_mod <- dt_comb %>% 
      filter(response_name %in% c(response)) %>% 
      dplyr::select(site_id, ln_rr) %>% 
      unique() %>% 
      filter(!is.na(ln_rr)) %>% 
      filter(!is.infinite(ln_rr))
    
    n <- nrow(dt_mod)
    
    m <- brm(as.formula(paste0("ln_rr ~ ", form)), data = dt_mod)
  }
  
  tidy_m <- broom.mixed::tidy(m)
  
  tmp_est <- tidy_m %>% 
    rename(ci_ub = conf.high, 
           ci_lb = conf.low) %>% 
    mutate(scale = sca, 
           formula = form, 
           response_name = response, 
           n = n,
           sig = ifelse(ci_lb > 0 | ci_ub < 0, "significant", "non significant"), 
           term = ifelse(term == "(Intercept)", "Intercept", term)) %>% 
    filter(!effect == "ran_pars")
  
  estimates <- rbind(estimates, tmp_est)
  
  dt_points <- rbind(dt_points, 
                     dt_mod %>% 
                       dplyr::select(site_id, ln_rr) %>% 
                       mutate(response_name = response))
  
  print(paste0(response, " done"))
  
}

estimates %>% ggplot() +
  geom_vline(xintercept = 0) +
  geom_point(data = dt_points, aes(x = ln_rr, y = response_name), alpha = 0.7) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = response_name, color = sig)) +
  theme_minimal()




