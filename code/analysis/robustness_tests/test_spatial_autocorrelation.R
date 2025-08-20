#Test for spatial autocorrelation 

library(data.table)
library(tidyverse)
library(tidylog)
library(patchwork)
library(glmmTMB)
library(brms)
library(tidybayes)
library(broom)
library("ggh4x")
library(scico)
library(sf)
library(spdep)

dt_pair_id <- fread("data/processed/fragments/plot_species_and_data.csv") %>% 
  dplyr::select(pair_id, plot_id) %>% 
  unique()

#get coordinates 
dt_coords <- st_read("data/spatial/clean_plot_locations_south_africa_exclosures_2025.gpkg") %>% 
  filter(grepl("out", plot_id)) %>% 
  left_join(dt_pair_id) %>% 
  mutate(lon = st_coordinates(.)[,1], 
         lat = st_coordinates(.)[,2]) %>% 
  as.data.table() %>% 
  mutate(geom = NULL) %>% 
  dplyr::select(lon, lat, pair_id)

dt <- fread("data/processed/clean/long_data_with_lnrr.csv") %>% 
  left_join(dt_coords)
unique(dt$response_name)

### build model guide

guide <- dt %>% 
  mutate(formula = case_when(
    #plot level
    response_name %in% c("plant_richness_plot", "woody_richness_plot", 
                         "forb_richness_plot", "graminoid_richness_plot", 
                         
                         "berger_parker_plot", "plant_evenness_plot", 
                         
                         "shannon_diversity_plot",
                         
                         "functional_dispersion_plot", "functional_diversity_plot", 
                         "functional_richness_plot", 
                         "functional_nearerst_neighbour_distance_plot",
                         
                         "point_return_fraction_plot", "mean_point_height_plot"
    ) ~ "1 + (1 | exclosure_id/cluster_id)",
    #cluster level
    response_name %in% c("plant_richness_cluster", "woody_richness_cluster", 
                         "forb_richness_cluster", "graminoid_richness_cluster", 
                         
                         "berger_parker_cluster", "plant_evenness_cluster", 
                         
                         "shannon_diversity_cluster",
                         
                         "functional_dispersion_cluster", "functional_diversity_cluster", 
                         "functional_richness_cluster", 
                         "functional_nearerst_neighbour_distance_cluster",
                         
                         "point_return_fraction_cluster", "mean_point_height_cluster"
    ) ~ "1 + (1 | exclosure_id)", 
    #site_level
    response_name %in% c("plant_richness_site", "woody_richness_site", 
                         "forb_richness_site", "graminoid_richness_site", 
                         
                         "berger_parker_site", "plant_evenness_site", 
                         
                         "shannon_diversity_site",
                         
                         "functional_dispersion_site", "functional_diversity_site", 
                         "functional_richness_site",
                         "functional_nearerst_neighbour_distance_site",
                         
                         "point_return_fraction_site", "mean_point_height_site"
    ) ~ "1")) %>% 
  dplyr::select(formula, response_name, scale) %>% 
  filter(!is.na(formula)) %>% 
  unique() %>% 
  mutate(response_tier = case_when(
    grepl("plant_richness", response_name) ~ "taxonomic_diversity", 
    grepl("woody_richness", response_name) ~ "taxonomic_diversity",
    grepl("forb_richness", response_name) ~ "taxonomic_diversity",
    grepl("graminoid_richness", response_name) ~ "taxonomic_diversity",
    grepl("shannon_diversity", response_name) ~ "taxonomic_diversity",
    grepl("berger_parker", response_name) ~ "dominance",
    grepl("plant_evenness", response_name) ~ "dominance",
    grepl("functional_dispersion", response_name) ~ "functional_diversity",
    grepl("functional_diversity", response_name) ~ "functional_diversity",
    grepl("functional_richness", response_name) ~ "functional_diversity",
    grepl("functional_nearerst_neighbour_distance", response_name) ~ "functional_diversity",
    grepl("point_return_fraction", response_name) ~ "structure",
    grepl("mean_point_height", response_name) ~ "structure"
  )) %>% filter(grepl("plot", response_name))

table(guide$response_tier)

dt_moran <- data.table()

for(response in unique(guide$response_name)){
  
  sca <- unique(guide[guide$response_name == response, ]$scale)
  form <- unique(guide[guide$response_name == response, ]$formula)
  tier <- unique(guide[guide$response_name == response, ]$response_tier)
  response
  
  if(sca == "plot"){
    
    dt_mod <- dt %>% 
      filter(response_name %in% c(response)) %>% 
      dplyr::select(setup_id, exclosure_id, cluster_id, ln_rr, lon, lat) %>% 
      unique() %>% 
      filter(!is.na(ln_rr)) %>% 
      filter(!is.infinite(ln_rr))
    
    n <- nrow(dt_mod)
    
    m <- glmmTMB(as.formula(paste0("ln_rr ~ ", form)), data = dt_mod)
  }else if(sca == "cluster"){
    
    dt_mod <- dt %>% 
      filter(response_name %in% c(response)) %>% 
      dplyr::select(setup_id, exclosure_id, ln_rr, lon, lat) %>% 
      unique() %>% 
      filter(!is.na(ln_rr)) %>% 
      filter(!is.infinite(ln_rr))
    
    n <- nrow(dt_mod)
    
    m <- glmmTMB(as.formula(paste0("ln_rr ~ ", form)), data = dt_mod)
  }else if(sca == "site"){
    
    dt_mod <- dt %>% 
      filter(response_name %in% c(response)) %>% 
      dplyr::select(setup_id, ln_rr, lon, lat) %>% 
      unique() %>% 
      filter(!is.na(ln_rr)) %>% 
      filter(!is.infinite(ln_rr))
    
    n <- nrow(dt_mod)
    
    m <- glmmTMB(as.formula(paste0("ln_rr ~ ", form)), data = dt_mod)
  }
  
  
  
  resids <- residuals(m, type = "pearson")
  coords <- dt_mod %>% dplyr::select(lon, lat)
  
  #test different ks 
  dt_moran_tmp <- data.frame()
  ks <- c(3, 18, 36)
  for(k in unique(ks)){
    
    nb <- knn2nb(knearneigh(coords, k = k))
    list_w <- nb2listw(nb, style = "W")
    
    moran <- moran.test(resids, list_w)
    
    moran$estimate[1]
    
    tmp <- data.frame(
      p_val = moran$p.value, 
      morans_i = as.numeric(moran$estimate[1]), 
      k = k, 
      response_name = response
    )
    
    dt_moran_tmp <- rbind(tmp, dt_moran_tmp)
  }
  
  
  dt_moran <- rbind(dt_moran_tmp, 
                     dt_moran)
  
  print(paste0(response, " done"))
  
}

dt_moran_plot <- dt_moran %>% 
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
      clean_response == "functional_richness" ~ "Plant Functional Richness", 
      clean_response == "functional_dispersion" ~ "Plant Functional Dispersion", 
    ), 
    clean_response = factor(clean_response, levels = c(
      "Plant Richness", "Shannon Diversity", "Graminoid Richness", "Forb Richness", "Woody Richness",
      "Plant Dominance", "Plant Evenness",
      "Vegetation Density", "Vegetation Height",
      "Plant Functional Diversity", "Plant Functional Distance", "Plant Functional Richness", "Plant Functional Dispersion"
    )))



sp_plot <- dt_moran_plot %>% 
  ggplot() +
  geom_hline(yintercept = 0.05, color = "red") +
  geom_point(aes(x = k, y = p_val), size = 2) + 
  facet_wrap(~clean_response, ncol = 5) +
  labs(x = "k (number of nearest neighbours)", y = "Moran's I test p value") +
  theme_bw()
sp_plot

ggsave(sp_plot, filename = "builds/plots/supplement/moran_p_val.png", dpi = 600, width = 10, height = 7)
