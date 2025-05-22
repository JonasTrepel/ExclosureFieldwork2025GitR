# Exploratory plots 

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


unique(dt_plot[grepl("addo_n", dt_plot$site_id), ]$plot_id)
table(dt_plot$n_species_site)


#### ALL SPECIES PLOTS ####
p1a <- dt_plot %>%
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_species_site), size = 1.05) +
  geom_point(aes(x = in_or_out, y = n_species_site, color = setup_id), size = 3, alpha = 0.8) + 
  labs(x = "", y = "Site-Level Species Richness") +
  theme_bw()  +
  theme(legend.position = "none")
p1a

p1b <- dt_plot %>%
  ggplot() +
  geom_point(aes(x = in_or_out, y = n_species_site, color = setup_id), size = 3, alpha = 0.8) + 
  facet_wrap(~setup_id) +
  labs(x = "", y = "Site-Level Species Richness") +
  theme_bw()  +
  theme(legend.position = "none")
p1b

p1c <- dt_plot %>%
  group_by(setup_id, in_or_out, cluster_id) %>% 
  summarize(n_species_cluster = mean(n_species_cluster, na.rm = T)) %>% 
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_species_cluster), size = 1.05) +
  geom_jitter(aes(x = in_or_out, y = n_species_cluster, color = setup_id), size = 2.5, alpha = 0.8) + 
  labs(x = "", y = "Cluster-Level Species Richness") +
  theme_bw() +
  theme(legend.position = "none")
p1c

p1d <- dt_plot %>%
  group_by(setup_id, in_or_out, cluster_id) %>% 
  summarize(n_species_cluster = mean(n_species_cluster, na.rm = T)) %>% 
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_species_cluster), size = 1.05) +
  geom_jitter(aes(x = in_or_out, y = n_species_cluster, color = setup_id), size = 2, alpha = 0.75) + 
  labs(x = "", y = "Cluster-Level Species Richness") +
  facet_wrap(~setup_id) +
  theme_bw() +
  theme(legend.position = "none")
p1d

p1e <- dt_plot %>%
  group_by(setup_id, in_or_out, plot_id) %>% 
  summarize(n_species_plot = mean(n_species_plot, na.rm = T)) %>% 
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_species_plot), size = 1.05) +
  geom_jitter(aes(x = in_or_out, y = n_species_plot, color = setup_id), size = 2, alpha = 0.75) + 
  labs(x = "", y = "Plot-Level Species Richness") +
  theme_bw() +
  theme(legend.position = "none")
p1e

p1f <- dt_plot %>%
  group_by(setup_id, in_or_out, plot_id) %>% 
  summarize(n_species_plot = mean(n_species_plot, na.rm = T)) %>% 
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_species_plot), size = 1.05) +
  geom_jitter(aes(x = in_or_out, y = n_species_plot, color = setup_id), size = 2, alpha = 0.75) + 
  facet_wrap(~setup_id) +
  labs(x = "", y = "Plot-Level Species Richness") +
  theme_bw() +
  theme(legend.position = "none")
p1f

p_all <- grid.arrange(p1a, p1b, p1c, p1d, p1e, p1f, ncol = 2, widths = c(0.5, 1))
ggsave(plot = p_all, "builds/plots/exploratory/all_sp_richness.png", dpi = 600, height = 10, width = 8)

#### Woodies PLOTS ####
pwa <- dt_plot %>%
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_woodies_site), size = 1.05) +
  geom_point(aes(x = in_or_out, y = n_woodies_site, color = setup_id), size = 3, alpha = 0.8) + 
  labs(x = "", y = "Site-Level Woody Richness") +
  theme_bw()  +
  theme(legend.position = "none")
pwa

pwb <- dt_plot %>%
  ggplot() +
  geom_point(aes(x = in_or_out, y = n_woodies_site, color = setup_id), size = 3, alpha = 0.8) + 
  facet_wrap(~setup_id) +
  labs(x = "", y = "Site-Level Woody Richness") +
  theme_bw()  +
  theme(legend.position = "none")
pwb

pwc <- dt_plot %>%
  group_by(setup_id, in_or_out, cluster_id) %>% 
  summarize(n_woodies_cluster = mean(n_woodies_cluster, na.rm = T)) %>% 
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_woodies_cluster), size = 1.05) +
  geom_jitter(aes(x = in_or_out, y = n_woodies_cluster, color = setup_id), size = 2.5, alpha = 0.8) + 
  labs(x = "", y = "Cluster-Level Woody Richness") +
  theme_bw() +
  theme(legend.position = "none")
pwc

pwd <- dt_plot %>%
  group_by(setup_id, in_or_out, cluster_id) %>% 
  summarize(n_woodies_cluster = mean(n_woodies_cluster, na.rm = T)) %>% 
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_woodies_cluster), size = 1.05) +
  geom_jitter(aes(x = in_or_out, y = n_woodies_cluster, color = setup_id), size = 2, alpha = 0.75) + 
  labs(x = "", y = "Cluster-Level Woody Richness") +
  facet_wrap(~setup_id) +
  theme_bw() +
  theme(legend.position = "none")
pwd

pwe <- dt_plot %>%
  group_by(setup_id, in_or_out, plot_id) %>% 
  summarize(n_woodies_plot = mean(n_woodies_plot, na.rm = T)) %>% 
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_woodies_plot), size = 1.05) +
  geom_jitter(aes(x = in_or_out, y = n_woodies_plot, color = setup_id), size = 2, alpha = 0.75) + 
  labs(x = "", y = "Plot-Level Woody Richness") +
  theme_bw() +
  theme(legend.position = "none")
pwe

pwf <- dt_plot %>%
  group_by(setup_id, in_or_out, plot_id) %>% 
  summarize(n_woodies_plot = mean(n_woodies_plot, na.rm = T)) %>% 
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_woodies_plot), size = 1.05) +
  geom_jitter(aes(x = in_or_out, y = n_woodies_plot, color = setup_id), size = 2, alpha = 0.75) + 
  facet_wrap(~setup_id) +
  labs(x = "", y = "Plot-Level Woody Richness") +
  theme_bw() +
  theme(legend.position = "none")
pwf

p_woody <- grid.arrange(pwa, pwb, pwc, pwd, pwe, pwf, ncol = 2, widths = c(0.5, 1))
ggsave(plot = p_woody, "builds/plots/exploratory/woody_richness.png", dpi = 600, height = 10, width = 8)


#### Forbs PLOTS ####
pfa <- dt_plot %>%
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_forbs_site), size = 1.05) +
  geom_point(aes(x = in_or_out, y = n_forbs_site, color = setup_id), size = 3, alpha = 0.8) + 
  labs(x = "", y = "Site-Level Forb Richness") +
  theme_bw()  +
  theme(legend.position = "none")
pfa

pfb <- dt_plot %>%
  ggplot() +
  geom_point(aes(x = in_or_out, y = n_forbs_site, color = setup_id), size = 3, alpha = 0.8) + 
  facet_wrap(~setup_id) +
  labs(x = "", y = "Site-Level Forb Richness") +
  theme_bw()  +
  theme(legend.position = "none")
pfb

pfc <- dt_plot %>%
  group_by(setup_id, in_or_out, cluster_id) %>% 
  summarize(n_forbs_cluster = mean(n_forbs_cluster, na.rm = T)) %>% 
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_forbs_cluster), size = 1.05) +
  geom_jitter(aes(x = in_or_out, y = n_forbs_cluster, color = setup_id), size = 2.5, alpha = 0.8) + 
  labs(x = "", y = "Cluster-Level Forb Richness") +
  theme_bw() +
  theme(legend.position = "none")
pfc

pfd <- dt_plot %>%
  group_by(setup_id, in_or_out, cluster_id) %>% 
  summarize(n_forbs_cluster = mean(n_forbs_cluster, na.rm = T)) %>% 
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_forbs_cluster), size = 1.05) +
  geom_jitter(aes(x = in_or_out, y = n_forbs_cluster, color = setup_id), size = 2, alpha = 0.75) + 
  labs(x = "", y = "Cluster-Level Forb Richness") +
  facet_wrap(~setup_id) +
  theme_bw() +
  theme(legend.position = "none")
pfd

pfe <- dt_plot %>%
  group_by(setup_id, in_or_out, plot_id) %>% 
  summarize(n_forbs_plot = mean(n_forbs_plot, na.rm = T)) %>% 
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_forbs_plot), size = 1.05) +
  geom_jitter(aes(x = in_or_out, y = n_forbs_plot, color = setup_id), size = 2, alpha = 0.75) + 
  labs(x = "", y = "Plot-Level Forb Richness") +
  theme_bw() +
  theme(legend.position = "none")
pfe

pff <- dt_plot %>%
  group_by(setup_id, in_or_out, plot_id) %>% 
  summarize(n_forbs_plot = mean(n_forbs_plot, na.rm = T)) %>% 
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_forbs_plot), size = 1.05) +
  geom_jitter(aes(x = in_or_out, y = n_forbs_plot, color = setup_id), size = 2, alpha = 0.75) + 
  facet_wrap(~setup_id) +
  labs(x = "", y = "Plot-Level Forb Richness") +
  theme_bw() +
  theme(legend.position = "none")
pff

p_forb <- grid.arrange(pfa, pfb, pfc, pfd, pfe, pff, ncol = 2, widths = c(0.5, 1))
ggsave(plot = p_forb, "builds/plots/exploratory/forb_richness.png", dpi = 600, height = 10, width = 8)

#### Graminoids PLOTS ####
pga <- dt_plot %>%
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_graminoids_site), size = 1.05) +
  geom_point(aes(x = in_or_out, y = n_graminoids_site, color = setup_id), size = 3, alpha = 0.8) + 
  labs(x = "", y = "Site-Level Graminoid Richness") +
  theme_bw()  +
  theme(legend.position = "none")
pga

pgb <- dt_plot %>%
  ggplot() +
  geom_point(aes(x = in_or_out, y = n_graminoids_site, color = setup_id), size = 3, alpha = 0.8) + 
  facet_wrap(~setup_id) +
  labs(x = "", y = "Site-Level Graminoid Richness") +
  theme_bw()  +
  theme(legend.position = "none")
pgb

pgc <- dt_plot %>%
  group_by(setup_id, in_or_out, cluster_id) %>% 
  summarize(n_graminoids_cluster = mean(n_graminoids_cluster, na.rm = T)) %>% 
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_graminoids_cluster), size = 1.05) +
  geom_jitter(aes(x = in_or_out, y = n_graminoids_cluster, color = setup_id), size = 2.5, alpha = 0.8) + 
  labs(x = "", y = "Cluster-Level Graminoid Richness") +
  theme_bw() +
  theme(legend.position = "none")
pgc

pgd <- dt_plot %>%
  group_by(setup_id, in_or_out, cluster_id) %>% 
  summarize(n_graminoids_cluster = mean(n_graminoids_cluster, na.rm = T)) %>% 
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_graminoids_cluster), size = 1.05) +
  geom_jitter(aes(x = in_or_out, y = n_graminoids_cluster, color = setup_id), size = 2, alpha = 0.75) + 
  labs(x = "", y = "Cluster-Level Graminoid Richness") +
  facet_wrap(~setup_id) +
  theme_bw() +
  theme(legend.position = "none")
pgd

pge <- dt_plot %>%
  group_by(setup_id, in_or_out, plot_id) %>% 
  summarize(n_graminoids_plot = mean(n_graminoids_plot, na.rm = T)) %>% 
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_graminoids_plot), size = 1.05) +
  geom_jitter(aes(x = in_or_out, y = n_graminoids_plot, color = setup_id), size = 2, alpha = 0.75) + 
  labs(x = "", y = "Plot-Level Graminoid Richness") +
  theme_bw() +
  theme(legend.position = "none")
pge

pgf <- dt_plot %>%
  group_by(setup_id, in_or_out, plot_id) %>% 
  summarize(n_graminoids_plot = mean(n_graminoids_plot, na.rm = T)) %>% 
  ggplot() +
  geom_boxplot(aes(x = in_or_out, y = n_graminoids_plot), size = 1.05) +
  geom_jitter(aes(x = in_or_out, y = n_graminoids_plot, color = setup_id), size = 2, alpha = 0.75) + 
  facet_wrap(~setup_id) +
  labs(x = "", y = "Plot-Level Graminoid Richness") +
  theme_bw() +
  theme(legend.position = "none")
pgf

p_graminoid <- grid.arrange(pga, pgb, pgc, pgd, pge, pgf, ncol = 2, widths = c(0.5, 1))
ggsave(plot = p_graminoid, "builds/plots/exploratory/graminoid_richness.png", dpi = 600, height = 10, width = 8)



######### Sampling completeness ###########

dt_sc <- data.table()
for(site in unique(dt_plot$site_id)){
  
  dt_site <- dt_plot %>% filter(site_id == site)
  
  on <- 0
  dt_sc_site <- data.table()
  dt_sc_raw <- data.table()
  
  for(i in 1:18){
    
    dt_sc_raw <- dt_site %>% 
      filter(plot_number %in% c(1:i)) %>% 
      mutate(sampled_plots = i, 
             n_species = n_distinct(species)) %>% 
      dplyr::select(sampled_plots, n_species, site_id) %>%
      unique()
    
    dt_sc_site <- rbind(dt_sc_site, dt_sc_raw)
    
  }
  
  dt_sc <- rbind(dt_sc_site, dt_sc)
  
}



p_sc <- dt_sc %>% 
  ggplot() +
  geom_point(aes(x = sampled_plots, y = n_species)) +
  facet_wrap(~site_id, scale = "free_y") +
  labs(y = "N Species", x = "Sampled Plots") +
  theme_bw()
p_sc

ggsave(plot = p_sc, "builds/plots/exploratory/sampling_completeness.png", dpi = 600, height = 5, width = 8)
