
library(data.table)
library(tidyverse)
library(gridExtra)
library(glmmTMB)
library(brms)
library(tidybayes)
library(broom)

dt <- fread("data/processed/clean/long_data_with_lnrr.csv")
unique(dt$response_name)


guide <- dt %>% 
  mutate(formula = case_when(
    #plot level
    response_name %in% c("plant_richness_plot", "woody_richness_plot", 
                         "forb_richness_plot", "graminoid_richness_plot", 
                         
                         "berger_parker_plot", "plant_evenness_plot", 
                         
                         "shannon_diversity_plot",
                         
                         "functional_dispersion_plot", "functional_diversity_plot", 
                         "functional_specialization_plot", "functional_nearerst_neighbour_distance_plot",
                         
                         "point_return_fraction_plot", "mean_point_height_plot"
    ) ~ "setup_id + 0 + (1 | cluster_id)",
    #cluster level
    response_name %in% c("plant_richness_cluster", "woody_richness_cluster", 
                         "forb_richness_cluster", "graminoid_richness_cluster", 
                         
                         "berger_parker_cluster", "plant_evenness_cluster", 
                         
                         "shannon_diversity_cluster",
                         
                         "functional_dispersion_cluster", "functional_diversity_cluster", 
                         "functional_specialization_cluster", "functional_nearerst_neighbour_distance_cluster",
                         
                         "point_return_fraction_cluster", "mean_point_height_cluster"
    ) ~ "setup_id + 0")) %>% 
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
    grepl("functional_specialization", response_name) ~ "functional_diversity",
    grepl("functional_nearerst_neighbour_distance", response_name) ~ "functional_diversity",
    grepl("point_return_fraction", response_name) ~ "structure",
    grepl("mean_point_height", response_name) ~ "structure"
  ))

table(guide$response_tier)

estimates <- data.table()
dt_points <- data.table()

for(response in unique(guide$response_name)){
  
  sca <- unique(guide[guide$response_name == response, ]$scale)
  form <- unique(guide[guide$response_name == response, ]$formula)
  tier <- unique(guide[guide$response_name == response, ]$response_tier)
  response
  
  if(sca == "plot"){
    
    dt_mod <- dt %>% 
      filter(response_name %in% c(response)) %>% 
      dplyr::select(setup_id, exclosure_id, cluster_id, ln_rr) %>% 
      unique() %>% 
      filter(!is.na(ln_rr)) %>% 
      filter(!is.infinite(ln_rr))
    
    n <- nrow(dt_mod)
    
    m <- glmmTMB(as.formula(paste0("ln_rr ~ ", form)), data = dt_mod)
  }else if(sca == "cluster"){
    
    dt_mod <- dt %>% 
      filter(response_name %in% c(response)) %>% 
      dplyr::select(setup_id, exclosure_id, ln_rr) %>% 
      unique() %>% 
      filter(!is.na(ln_rr)) %>% 
      filter(!is.infinite(ln_rr))
    
    n <- nrow(dt_mod)
    
    m <- glmmTMB(as.formula(paste0("ln_rr ~ ", form)), data = dt_mod)
  }else if(sca == "site"){
    
    dt_mod <- dt %>% 
      filter(response_name %in% c(response)) %>% 
      dplyr::select(setup_id, ln_rr) %>% 
      unique() %>% 
      filter(!is.na(ln_rr)) %>% 
      filter(!is.infinite(ln_rr))
    
    n <- nrow(dt_mod)
    
    m <- glmmTMB(as.formula(paste0("ln_rr ~ ", form)), data = dt_mod)
  }
  
  tidy_m <- broom.mixed::tidy(m)
  
  tmp_est <- tidy_m %>% 
    # rename(ci_ub = conf.high, 
    #        ci_lb = conf.low) %>% 
    mutate(scale = sca, 
           formula = form, 
           response_name = response, 
           response_tier = tier, 
           n = n,
           ci_ub = estimate + 1.96*std.error, 
           ci_lb = estimate - 1.96*std.error,
           sig = ifelse(ci_lb > 0 | ci_ub < 0, "significant", "non-significant"), 
           term = gsub("setup_id", "", term)) %>% 
    filter(!effect == "ran_pars") %>% 
    mutate(group = NULL)
  
  estimates <- rbind(estimates, tmp_est) %>% as.data.table()
  
  print(paste0(response, " done"))
  
}

fwrite(estimates, "builds/model_outputs/glmm_estimates_sites_separately.csv")

dt_env <- fread("data/processed/fragments/plot_environmental.csv")

dt %>% dplyr::select(npp, setup_id) %>% arrange(npp) %>% unique() 

rts <- estimates %>% dplyr::select(response_name, response_tier) %>% 
  mutate(response_tier = factor(response_tier, levels = c("taxonomic_diversity", 
                                                          "dominance", 
                                                          "structure",
                                                          "functional_diversity"))) %>% unique()

p_plot <- estimates %>%
  filter(scale == "plot") %>% 
  mutate(response_tier = factor(response_tier, levels = c("taxonomic_diversity", 
                                                          "dominance", 
                                                          "structure",
                                                          "functional_diversity")),
         term = factor(term, levels = c("knp_roan", 
                                        "knp_satara",
                                        "knp_nkuhlu_full", 
                                        "pnr", 
                                        "addo_jack", 
                                        "addo_nyathi_full"))) %>%
  arrange(desc(response_tier)) %>% 
  mutate(response_name = factor(response_name, levels = unique(response_name))) %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_jitter(data = dt %>%
                filter(response_name %in% unique(estimates[scale == "plot", ]$response_name)) %>%
                left_join(rts) %>% 
                mutate(term = factor(setup_id, levels = c("knp_roan", 
                                                      "knp_satara",
                                                      "knp_nkuhlu_full", 
                                                      "pnr", 
                                                      "addo_jack", 
                                                      "addo_nyathi_full"))) %>%
                unique(), aes(x = ln_rr, y = response_name, fill = npp), alpha = 0.5, height = 0.1, shape = 21) +
  facet_wrap(~term, ncol = 6) +
  scale_color_manual(values = c("lightblue", "orange2")) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = response_name, color = sig), linewidth = 1.3, alpha = 0.9) +
  labs(title = "Plot-Scale", y = "Response", x = "Log Response-Ratio") +
  theme_bw() +
  theme(legend.position = "bottom")
p_plot

ggsave(plot = p_plot, "builds/plots/preliminary/glmm_estimates_setups_seperate.png", dpi = 600, height = 6, width = 12)
