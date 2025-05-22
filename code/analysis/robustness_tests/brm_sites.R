
library(data.table)
library(tidyverse)
library(gridExtra)
library(glmmTMB)
library(brms)
library(tidybayes)
library(broom)
library("ggh4x")
library(scico)

dt <- fread("data/processed/clean/long_data_with_lnrr.csv") %>% 
  mutate(biome = ifelse(setup_id %in% c("addo_nyathi_full", "addo_jack"), "Thicket", "Savanna"))
unique(dt$response_name)


guide <- dt %>% 
  mutate(formula = case_when(
    #plot level
    response_name %in% c("plant_richness_plot", "woody_richness_plot", 
                         "forb_richness_plot", "graminoid_richness_plot", 
                         
                         "berger_parker_plot", "plant_evenness_plot", 
                         
                         "shannon_diversity_plot",
                         
                         "functional_dispersion_plot", "functional_diversity_plot", 
                         "functional_richness_plot", "functional_nearerst_neighbour_distance_plot",
                         
                         "point_return_fraction_plot", "mean_point_height_plot"
    ) ~ "setup_id + 0 + (1 | cluster_id)",
    #cluster level
    response_name %in% c("plant_richness_cluster", "woody_richness_cluster", 
                         "forb_richness_cluster", "graminoid_richness_cluster", 
                         
                         "berger_parker_cluster", "plant_evenness_cluster", 
                         
                         "shannon_diversity_cluster",
                         
                         "functional_dispersion_cluster", "functional_diversity_cluster", 
                         "functional_richness_cluster", "functional_nearerst_neighbour_distance_cluster",
                         
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
    grepl("functional_richness", response_name) ~ "functional_diversity",
    grepl("functional_nearerst_neighbour_distance", response_name) ~ "functional_diversity",
    grepl("point_return_fraction", response_name) ~ "structure",
    grepl("mean_point_height", response_name) ~ "structure"
  )) %>% 
  filter(!grepl("cluster", response_name)) %>%
  filter(!grepl("functional_richness", response_name))

table(guide$response_tier)
unique(dt$setup_id)


estimates <- data.table()
dt_points <- data.table()

prior <- c(
  set_prior("normal(0, 10)", class = "b", coef = "setup_idknp_nkuhlu_full"),  
  set_prior("normal(0, 10)", class = "b", coef = "setup_idknp_roan"),  
  set_prior("normal(0, 10)", class = "b", coef = "setup_idknp_satara"), 
  set_prior("normal(0, 10)", class = "b", coef = "setup_idpnr"),     
  set_prior("normal(0, 10)", class = "b", coef = "setup_idaddo_nyathi_full"), 
  set_prior("normal(0, 10)", class = "b", coef = "setup_idaddo_jack")  
)

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
    set.seed(161)
    m <- brm(as.formula(paste0("ln_rr ~ ", form)), prior = prior, data = dt_mod)
  }else if(sca == "cluster"){
    
    dt_mod <- dt %>% 
      filter(response_name %in% c(response)) %>% 
      dplyr::select(setup_id, exclosure_id, ln_rr) %>% 
      unique() %>% 
      filter(!is.na(ln_rr)) %>% 
      filter(!is.infinite(ln_rr))
    
    n <- nrow(dt_mod)
    set.seed(161)
    m <- brm(as.formula(paste0("ln_rr ~ ", form)), prior = prior, data = dt_mod)
  }
  
  tidy_m <- broom.mixed::tidy(m)
  
  tmp_est <- tidy_m %>% 
    rename(ci_ub = conf.high,
           ci_lb = conf.low) %>%
    mutate(scale = sca, 
           formula = form, 
           response_name = response, 
           response_tier = tier, 
           n = n,
           # ci_ub = estimate + 1.96*std.error, 
           # ci_lb = estimate - 1.96*std.error,
           sig = ifelse(ci_lb > 0 | ci_ub < 0, "significant", "non-significant"), 
           term = gsub("setup_id", "", term)) %>% 
    filter(!effect == "ran_pars") %>% 
    mutate(group = NULL)
  
  estimates <- rbind(estimates, tmp_est) %>% as.data.table()
  
  print(paste0(response, " done"))
  
}

estimates <- estimates %>% 
  mutate(
    percent_change = (exp(estimate) - 1)*100, 
    ci_ub_percent_change = (exp(ci_ub) - 1)*100, 
    ci_lb_percent_change = (exp(ci_lb) - 1)*100, 
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
    )),
    clean_response_tier = case_when(response_tier == "taxonomic_diversity" ~ "Taxonomic\nDiversity", 
                                    response_tier ==  "dominance" ~ "Dominance", 
                                    response_tier ==  "structure"~ "Vegetation\nStructure",
                                    response_tier ==  "functional_diversity"~ "Functional\nDiversity"),
    clean_response_tier = factor(clean_response_tier, levels = c("Taxonomic\nDiversity", 
                                                                 "Dominance", 
                                                                 "Vegetation\nStructure",
                                                                 "Functional\nDiversity")), 
    clean_setup = case_when(
      term == "knp_roan" ~ "Nwaswitshumbe", 
      term == "knp_satara" ~ "Satara",
      term == "knp_nkuhlu_full" ~ "Nkuhlu", 
      term == "pnr" ~ "PNR", 
      term == "addo_jack" ~ "Jack's", 
      term == "addo_nyathi_full" ~ "Nyathi"
    ), 
    clean_setup = factor(clean_setup, levels = c("Nwaswitshumbe", "Satara", "Nkuhlu", 
                                                 "PNR", "Jack's", "Nyathi")))


rts <- estimates %>%
  dplyr::select(response_name, response_tier, clean_response_tier, clean_response, clean_setup, setup_id = term)


fwrite(estimates %>%
         mutate(estimate_ci = paste0(round(estimate, 2), " (", round(ci_lb, 2), "; ", round(ci_ub, 2), ")"), 
                percent_change_ci = paste0(round(percent_change, 2),
                                           " (", round(ci_lb_percent_change, 2), "; ", round(ci_ub_percent_change, 2), ")")) %>% 
         arrange(response_name, desc(response_tier)) %>% 
         dplyr::select(Site = clean_setup, clean_response, estimate_ci, percent_change_ci), "builds/model_outputs/table_brm_estimates_sites_separately.csv")

fwrite(estimates, "builds/model_outputs/raw_brm_estimates_sites.csv")


p_plot <- estimates %>%
  filter(scale == "plot") %>% 
  arrange(desc(response_tier)) %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25") +
  geom_jitter(data = dt %>%
                filter(response_name %in% unique(estimates[scale == "plot", ]$response_name)) %>%
                left_join(rts) %>% 
                unique(), aes(x = ln_rr, y = clean_response, color = biome), alpha = 0.75, height = 0.1) +
  facet_grid2(cols = vars(clean_setup), rows = vars(clean_response_tier), scales = "free_y", space = "free_y") +
 #scale_color_manual(values = c("grey50", "orange2")) +
  scale_fill_scico(palette = "bam", midpoint = 0) +
  scale_color_scico_d(palette = "bamako", direction = -1, begin = 0.5, end = 0.9) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_response,
                      fill = estimate), linewidth = 1.1, alpha = 0.9, shape = 23, size = 1.1, color = "black") +
  labs(title = "Site-Specific Estimates", y = "Response", x = "Log-Response Ratio", 
       color = "Biome", #expression(NPP~(kg~C~ha^{-1}~yr^{-1})),
       fill = "Estimate (Log-\nResponse Ratio)") +
  theme_minimal() +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "bottom", 
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = .5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(linetype = "dashed", color = "seashell3"), 
        panel.grid.major.y = element_line(linetype = "dashed", color = "seashell3"),
        panel.background = element_rect(fill = "grey98", color = "grey98"), 
        strip.text.y = element_text(size = 9, face = "italic"), 
        strip.text.x = element_text(size = 10, face = "bold"), 
        strip.background.x = element_rect(fill = "grey90", color = "grey90" )
        
  )
p_plot

ggsave(plot = p_plot, "builds/plots/supplement/brm_estimates_sites.png", dpi = 600, height = 6, width = 12.5)
