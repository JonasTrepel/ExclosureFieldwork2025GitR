library(tidyverse)
library(data.table)
library(scico)
library(ggh4x)
library(gridExtra)

### 1. Sort data frames --------------------


dt_long <- fread("data/processed/clean/long_data_with_lnrr.csv") %>%
  mutate(biome = ifelse(setup_id %in% c("addo_nyathi_full", "addo_jack"), "Thicket", "Savanna")) %>%
  filter(scale == "plot")

dt_mod <- dt_long %>%
  dplyr::select(c(response_name, ln_rr, pair_id, cluster_id, exclosure_id, setup_id, biome)) %>%
  pivot_wider(names_from = "response_name", values_from = "ln_rr", id_cols = c(pair_id, cluster_id, exclosure_id, setup_id, biome)) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .))) %>%
  as.data.table()

dt_points <- dt_long %>%
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
    ))
  )
dt_res <- fread("builds/model_outputs/diversity_brms_model_results.csv") %>%
  mutate(
    clean_term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "berger_parker_plot" ~ "Plant Dominance",
      term == "plant_evenness_plot" ~ "Plant Evenness",
      term == "point_return_fraction_plot" ~ "Vegetation Density",
      term == "mean_point_height_plot" ~ "Vegetation Height", 
      term == "plant_richness_plot" ~ "Plant Richness"
    ),
    clean_term = factor(clean_term, levels = c(
      "Intercept", "Plant Dominance", "Plant Evenness",
      "Vegetation Density", "Vegetation Height", "Plant Richness"
    )),
    clean_response = case_when(
      response == "plant_richness_plot" ~ "Plant Richness",
      response == "woody_richness_plot" ~ "Woody Richness",
      response == "forb_richness_plot" ~ "Forb Richness",
      response == "graminoid_richness_plot" ~ "Graminoid Richness",
      response == "shannon_diversity_plot" ~ "Shannon Diversity",
      response == "functional_nearerst_neighbour_distance_plot" ~ "Plant Functional Distance", 
      response == "functional_diversity_plot" ~ "Plant Functional Diversity", 
      response == "functional_richness_plot" ~ "Plant Functional Richness", 
      response == "functional_dispersion_plot" ~ "Plant Functional Dispersion", 
    ),
    clean_response = factor(clean_response, levels = c(
      "Plant Richness", "Shannon Diversity", "Graminoid Richness", "Forb Richness", "Woody Richness",
      "Plant Functional Diversity", "Plant Functional Distance", "Plant Functional Richness", "Plant Functional Dispersion"
    )),
    sig = ifelse(ci_ub < 0 | ci_lb > 0, "significant", "non-significant")
  ) %>% 
  mutate(label = paste0(clean_response, "\n(R²m = ", round(rsq_m, 2), "; R²c = ", round(rsq_c, 2), ")")
  ) 

dt_sig <- dt_res %>% dplyr::select(response, tier, term, sig, clean_term, clean_response)


dt_pred <- fread("builds/model_outputs/diversity_brms_model_predictions.csv")  %>%
  left_join(dt_sig) %>% 
  mutate(clean_response = gsub("Functional ", "Functional\n", clean_response), 
         clean_response = gsub(" Richness", "\nRichness", clean_response), 
         clean_response = factor(clean_response, levels = c(
           "Plant\nRichness", "Shannon Diversity", "Graminoid\nRichness", "Forb\nRichness", "Woody\nRichness", 
           "Plant Functional\nDiversity", "Plant Functional\nDistance", "Plant Functional\nRichness", "Plant Functional\nDispersion"
         )))

dt_points <- dt_mod %>%
  mutate(plant_richness_plot_term = plant_richness_plot) %>% 
  pivot_longer(
    cols = c(
      plant_richness_plot, woody_richness_plot, forb_richness_plot,
      graminoid_richness_plot, shannon_diversity_plot,
      functional_nearerst_neighbour_distance_plot, functional_diversity_plot,
      functional_richness_plot, functional_dispersion_plot
    ),
    names_to = "response_name",
    values_to = "response_value"
  ) %>%
  pivot_longer(
    cols = c(
      berger_parker_plot, plant_evenness_plot, plant_richness_plot_term,
      point_return_fraction_plot, mean_point_height_plot
    ),
    names_to = "term_name",
    values_to = "term_value"
  ) %>%
  mutate(
    term_name = ifelse(term_name == "plant_richness_plot_term", "plant_richness_plot", term_name),
    clean_term = case_when(
      .default = NA,
      term_name == "berger_parker_plot" ~ "Plant Dominance",
      term_name == "plant_evenness_plot" ~ "Plant Evenness",
      term_name == "point_return_fraction_plot" ~ "Vegetation Density",
      term_name == "mean_point_height_plot" ~ "Vegetation Height",
      term_name == "plant_richness_plot" ~ "Plant Richness"
      
    ),
    clean_term = factor(clean_term, levels = c(
      "Plant Dominance", "Plant Evenness",
      "Vegetation Density", "Vegetation Height", "Plant Richness"
    )),
    clean_response = case_when(
      .default = NA,
      response_name == "plant_richness_plot" ~ "Plant\nRichness",
      response_name == "woody_richness_plot" ~ "Woody\nRichness",
      response_name == "forb_richness_plot" ~ "Forb\nRichness",
      response_name == "graminoid_richness_plot" ~ "Graminoid\nRichness",
      response_name == "shannon_diversity_plot" ~ "Shannon\nDiversity",
      response_name == "functional_nearerst_neighbour_distance_plot" ~ "Plant Functional\nDistance", 
      response_name == "functional_diversity_plot" ~ "Plant Functional\nDiversity", 
      response_name == "functional_richness_plot" ~ "Plant Functional\nRichness", 
      response_name == "functional_dispersion_plot" ~ "Plant Functional\nDispersion"
    ),
    clean_response = factor(clean_response, levels = c(
      "Plant\nRichness", "Shannon\nDiversity", "Graminoid\nRichness", "Forb\nRichness", "Woody\nRichness", 
      "Plant Functional\nDiversity", "Plant Functional\nDistance", "Plant Functional\nRichness", "Plant Functional\nDispersion"
    ))
  )

# 2. Estimate plots --------------------------------------------------------

## 2.1 Taxonomic ----------------------------
p_est_dom_tax <- dt_res %>%
  filter(response %in% c("plant_richness_plot", 
                         "functional_richness_plot",
                         "functional_nearerst_neighbour_distance_plot", 
                         "functional_diversity_plot",
                         "functional_dispersion_plot"
  ) & 
    grepl("dominance", tier)) %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25") +
  #facet_grid2(rows = vars(clean_response), scales = "free_y", space = "free_y") +
  facet_wrap(~ clean_response, ncol = 5) +
  scale_color_scico(palette = "bam", midpoint = 0) +
  scale_fill_scico(palette = "bam", midpoint = 0) +
  
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term, fill = estimate),
                  shape = 23, color = "black",
                  linewidth = 1.1, alpha = 0.9) +
  labs(y = "", x = "Estimate") +
  theme_minimal() +
  scale_y_discrete(limits = rev) +
  theme(
    legend.position = "right",
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = .5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(linetype = "dashed", color = "seashell3"),
    panel.grid.major.y = element_line(linetype = "dashed", color = "seashell3"),
    panel.background = element_rect(fill = "grey98", color = "grey98"), 
    strip.text = element_text(size = 10, face = "italic"),
    strip.background.x = element_rect(fill = "grey90", color = "grey90" )
  )
p_est_dom_tax


p_est_eve_tax <- dt_res %>%
  filter(response %in% c("plant_richness_plot", 
                         "functional_richness_plot", 
                         "functional_nearerst_neighbour_distance_plot", 
                         "functional_diversity_plot",
                         "functional_dispersion_plot"
  ) & 
    grepl("evenness", tier)) %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25") +
  #facet_grid2(rows = vars(clean_response), scales = "free_y", space = "free_y") +
  facet_wrap(~ clean_response, ncol = 5) +
  scale_color_scico(palette = "bam", midpoint = 0) +
  scale_fill_scico(palette = "bam", midpoint = 0) +
  
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term, fill = estimate),
                  shape = 23, color = "black",
                  linewidth = 1.1, alpha = 0.9) +
  labs(y = "", x = "Estimate") +
  theme_minimal() +
  scale_y_discrete(limits = rev) +
  theme(
    legend.position = "right",
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = .5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(linetype = "dashed", color = "seashell3"),
    panel.grid.major.y = element_line(linetype = "dashed", color = "seashell3"),
    panel.background = element_rect(fill = "grey98", color = "grey98"), 
    strip.text = element_text(size = 10, face = "italic"),
    strip.background.x = element_rect(fill = "grey90", color = "grey90" )
  )
p_est_eve_tax




# 4. Combine plots -------------------

p_comb <- grid.arrange(p_est_dom_tax, p_est_eve_tax, ncol = 1)
ggsave(plot = p_comb, "builds/plots/supplement/brm_diversity_mechanism_estimatess.png", dpi = 600, height = 5, width = 10)
