
library(data.table)
library(tidyverse)
library(gridExtra)
library(glmmTMB)
library(brms)
library(tidybayes)
library(broom)
library("ggh4x")
library(scico)

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
                         ) ~ "1 + (1 | exclosure_id/cluster_id)",
    #cluster level
    response_name %in% c("plant_richness_cluster", "woody_richness_cluster", 
                         "forb_richness_cluster", "graminoid_richness_cluster", 
                         
                         "berger_parker_cluster", "plant_evenness_cluster", 
                         
                         "shannon_diversity_cluster",
                         
                         "functional_dispersion_cluster", "functional_diversity_cluster", 
                         "functional_specialization_cluster", "functional_nearerst_neighbour_distance_cluster",
                         
                         "point_return_fraction_cluster", "mean_point_height_cluster"
                         ) ~ "1 + (1 | exclosure_id)", 
    #site_level
    response_name %in% c("plant_richness_site", "woody_richness_site", 
                         "forb_richness_site", "graminoid_richness_site", 
                         
                         "berger_parker_site", "plant_evenness_site", 
                         
                         "shannon_diversity_site",
                         
                         "functional_dispersion_site", "functional_diversity_site", 
                         "functional_specialization_site", "functional_nearerst_neighbour_distance_site",
                         
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
           term = ifelse(term == "(Intercept)", "Intercept", term)) %>% 
    filter(!effect == "ran_pars") %>% 
    mutate(group = NA)
  
  estimates <- rbind(estimates, tmp_est) %>% as.data.table()
  
  dt_points <- rbind(dt_points, 
                     dt_mod %>% 
                       dplyr::select(setup_id, ln_rr) %>% 
                       mutate(response_name = response))
  
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
    clean_response == "functional_specialization" ~ "Plant Functional Specialization", 
    clean_response == "functional_dispersion" ~ "Plant Functional Dispersion", 
  ), 
  clean_response = factor(clean_response, levels = c(
    "Plant Richness", "Shannon Diversity", "Graminoid Richness", "Forb Richness", "Woody Richness",
    "Plant Dominance", "Plant Evenness",
    "Vegetation Density", "Vegetation Height",
    "Plant Functional Diversity", "Plant Functional Distance", "Plant Functional Specialization", "Plant Functional Dispersion"
  )),
  clean_response_tier = case_when(response_tier == "taxonomic_diversity" ~ "Taxonomic\nDiversity", 
                                  response_tier ==  "dominance" ~ "Dominance", 
                                  response_tier ==  "structure"~ "Vegetation\nStructure",
                                  response_tier ==  "functional_diversity"~ "Functional\nDiversity"),
  clean_response_tier = factor(clean_response_tier, levels = c("Taxonomic\nDiversity", 
                                                      "Dominance", 
                                                      "Vegetation\nStructure",
                                                      "Functional\nDiversity")))
  

rts <- estimates %>% dplyr::select(response_name, response_tier, clean_response_tier, clean_response)

fwrite(estimates, "builds/model_outputs/glmm_estimates_full_dataset.csv")

estimates$estimate
exp(estimates$estimate)


ann_text <- tibble::tibble(
  ln_rr = c(-1.5, 1.5),
  label = c("Decrease in herbivore presence", "Increase in herbivore presence"),
  clean_response_name = NA, 
  clean_response_tier = factor("Taxonomic\nDiversity", levels = c("Taxonomic\nDiversity", 
                                                                 "Dominance", 
                                                                 "Vegetation\nStructure",
                                                                 "Functional\nDiversity")))


### Visualize -------------------
library("ggh4x")
library(scico)
p_plot <- estimates %>%
  filter(scale == "plot") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25") +
  geom_text(data = ann_text,
            aes(x = ln_rr, y = Inf, label = label),
            fontface = "italic", size = 2.5, vjust = 1.5, color = "grey50") +
  geom_jitter(data = dt %>%
               filter(response_name %in% unique(estimates[scale == "plot", ]$response_name)) %>%
               left_join(rts) %>% 
               mutate(setup_id = factor(setup_id, levels = c("knp_roan", 
                                                              "knp_satara",
                                                              "knp_nkuhlu_full", 
                                                              "pnr", 
                                                              "addo_jack", 
                                                              "addo_nyathi_full"))) %>%
                unique(), aes(x = ln_rr, y = clean_response, fill = ln_rr),
              alpha = 0.5, shape = 21, height = 0.1, color = "grey75") +
  facet_grid2(rows = vars(clean_response_tier), scales = "free_y", space = "free_y") +
  scale_color_manual(values = c("grey50", "orange2")) +
  scale_fill_scico(palette = "bam", midpoint = 0) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_response, color = sig), linewidth = 1.3, alpha = 0.9) +
  labs(title = "Plot-Scale", y = "Response", x = "Log-Response Ratio") +
  theme_minimal() +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = .5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(linetype = "dashed", color = "seashell3"), 
        panel.grid.major.y = element_line(linetype = "dashed", color = "seashell3"),
        panel.background = element_rect(fill = "seashell1", color = "seashell1"), 
        strip.text = element_blank() #element_text(size = 10, face = "italic")
        )
p_plot

p_site <- estimates %>%
  filter(scale == "site") %>% 
  mutate(response_tier = factor(response_tier, levels = c("taxonomic_diversity", 
                                                          "dominance", 
                                                          "structure",
                                                          "functional_diversity"))) %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25") +
  geom_jitter(data = dt %>%
                filter(response_name %in% unique(estimates[scale == "site", ]$response_name)) %>%
                dplyr::select(response_name, ln_rr, setup_id) %>% 
                left_join(rts) %>% 
                mutate(setup_id = factor(setup_id, levels = c("knp_roan", 
                                                              "knp_satara",
                                                              "knp_nkuhlu_full", 
                                                              "pnr", 
                                                              "addo_jack", 
                                                              "addo_nyathi_full"))) %>%
                unique(), aes(x = ln_rr, y = clean_response, fill = ln_rr),
              alpha = 0.5, shape = 21, height = 0.1, color = "grey75") +
  facet_grid2(rows = vars(clean_response_tier), scales = "free_y", space = "free_y") +
  scale_color_manual(values = c("grey50", "orange2")) +
  scale_fill_scico(palette = "bam", midpoint = 0) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_response, color = sig), linewidth = 1.3, alpha = 0.9) +
  labs(title = "Site-Scale", y = "", x = "Log-Response Ratio", fill = "Log-\nResponse\nRatio", color = "") +
  theme_minimal() +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = .5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(linetype = "dashed", color = "seashell3"), 
        panel.grid.major.y = element_line(linetype = "dashed", color = "seashell3"),
        panel.background = element_rect(fill = "seashell1", color = "seashell1"), 
        axis.text.y = element_blank(),
        strip.text = element_text(size = 10, face = "italic"))
p_site


p_comb <- grid.arrange(p_plot, p_site, ncol = 2, widths = c(1.2, 1))

ggsave(plot = p_comb, "builds/plots/preliminary/glmm_estimates.png", dpi = 600, height = 6, width = 11)

