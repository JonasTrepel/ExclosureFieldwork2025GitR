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

dt_res <- fread("builds/model_outputs/diversity_glmms_model_results.csv") %>%
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



dt_pred <- fread("builds/model_outputs/diversity_glmms_model_predictions.csv") %>%
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

label_df_dom <- dt_res %>%
  filter(response %in% c("plant_richness_plot", 
                         "functional_nearerst_neighbour_distance_plot", 
                         "functional_diversity_plot",
                         "functional_dispersion_plot", 
                         "functional_richness_plot"
  ) & 
    grepl("dominance", tier)) %>% 
  distinct(clean_response, .keep_all = TRUE) %>%
  mutate(
    label = paste0(clean_response, "\n(R²m = ", round(rsq_m, 2), "; R²c = ", round(rsq_c, 2)),
    label = factor(label, levels = label[order(clean_response)])
  ) %>%
  select(clean_response, label)

p_est_dom_tax <- dt_res %>%
  filter(response %in% c("plant_richness_plot", 
                         "functional_nearerst_neighbour_distance_plot", 
                         "functional_diversity_plot",
                         "functional_dispersion_plot", 
                         "functional_richness_plot"
                         ) & 
           grepl("dominance", tier)) %>%
  dplyr::select(-label) %>% 
  left_join(label_df_dom) %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25") +
  #facet_grid2(rows = vars(clean_response), scales = "free_y", space = "free_y") +
  facet_wrap(~ label, ncol = 1) +
  scale_color_manual(values = c("grey50", "orange2")) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term, color = sig), linewidth = 1.3, alpha = 0.9) +
  labs(y = "", x = "Estimate") +
  theme_minimal() +
  scale_y_discrete(limits = rev) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = .5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(linetype = "dashed", color = "seashell3"),
    panel.grid.major.y = element_line(linetype = "dashed", color = "seashell3"),
    panel.background = element_rect(fill = "seashell1", color = "seashell1"),
    strip.text = element_text(size = 10, face = "italic"),
    strip.background = element_rect(fill = "seashell2", color = "seashell2")
  )
p_est_dom_tax

p_est_dom_tax_sr <- dt_res %>%
  filter(response %in% c("plant_richness_plot"
  ) & 
    grepl("dominance", tier)) %>%
  dplyr::select(-label) %>% 
  left_join(label_df_dom) %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25") +
  #facet_grid2(rows = vars(clean_response), scales = "free_y", space = "free_y") +
  facet_wrap(~ label, ncol = 1) +
  scale_color_manual(values = c("grey50", "orange2")) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term, color = sig), linewidth = 1.3, alpha = 0.9) +
  labs(y = "", x = "Estimate") +
  theme_minimal() +
  scale_y_discrete(limits = rev) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = .5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(linetype = "dashed", color = "seashell3"),
    panel.grid.major.y = element_line(linetype = "dashed", color = "seashell3"),
    panel.background = element_rect(fill = "seashell1", color = "seashell1"),
    strip.text = element_text(size = 10, face = "italic"),
    strip.background = element_rect(fill = "seashell2", color = "seashell2")
  )
p_est_dom_tax_sr

label_df_eve <- dt_res %>%
  filter(response %in% c("plant_richness_plot", 
                         "functional_nearerst_neighbour_distance_plot", 
                         "functional_diversity_plot",
                         "functional_dispersion_plot",
                         "functional_richness_plot"
  ) & 
    grepl("evenness", tier)) %>% 
  distinct(clean_response, .keep_all = TRUE) %>%
  mutate(
    label = paste0(clean_response, "\n(R²m = ", round(rsq_m, 2), "; R²c = ", round(rsq_c, 2)),
    label = factor(label, levels = label[order(clean_response)])
  ) %>%
  select(clean_response, label)

p_est_eve_tax <- dt_res %>%
  filter(response %in% c("plant_richness_plot", 
                         "functional_nearerst_neighbour_distance_plot", 
                         "functional_diversity_plot",
                         "functional_dispersion_plot", 
                         "functional_richness_plot"
  ) & grepl("evenness", tier)) %>% 
  dplyr::select(-label) %>% 
  left_join(label_df_eve) %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25") +
  #facet_grid2(rows = vars(clean_response), scales = "free_y", space = "free_y") +
  facet_wrap(~ label, ncol = 5) +
  scale_color_manual(values = c("grey50", "orange2")) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term, color = sig), linewidth = 1.3, alpha = 0.9) +
  labs(y = "", x = "Estimate") +
  theme_minimal() +
  scale_y_discrete(limits = rev) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = .5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(linetype = "dashed", color = "seashell3"),
    panel.grid.major.y = element_line(linetype = "dashed", color = "seashell3"),
    panel.background = element_rect(fill = "seashell1", color = "seashell1"),
    strip.text = element_text(size = 10, face = "italic"),
    strip.background = element_rect(fill = "seashell2", color = "seashell2")
  )
p_est_eve_tax


# 3. Prediction plots -----------------------------------

p_pred_dom_tax_raw <- dt_pred %>%
  filter(response %in% c("plant_richness_plot"
  ) & 
    grepl("dominance", tier)) %>%
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "seashell3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "seashell3") +
  geom_point(
    data = dt_points %>% filter(!term_name %in% c("plant_evenness_plot", 
                                                  "plant_richness_plot") & response_name %in% c("plant_richness_plot") ),
    aes(x = term_value, y = response_value),
    alpha = 0.15, color = "black", size = 1
  ) +
  geom_ribbon(aes(x = var_value, ymax = conf.high, ymin = conf.low, linetype = sig, fill = sig), alpha = 0.25) +
  geom_line(aes(x = var_value, y = predicted, linetype = sig, color = sig), linewidth = 1.05) +
  scale_linetype_manual(values = c("non-significant" = "dashed", "significant" = "solid")) +
  scale_fill_manual(values = c("grey50", "orange2")) +
  scale_color_manual(values = c("grey50", "orange2")) +
  facet_grid(rows = vars(clean_response), cols = vars(clean_term), scales = "free") +
  labs(y = "Response Value", x = "Predictor Value") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = .5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(linetype = "dashed", color = "seashell1"),
    panel.grid.major.y = element_line(linetype = "dashed", color = "seashell1"),
    panel.background = element_rect(fill = "seashell1", color = "seashell1"),
    strip.text = element_text(size = 10, face = "italic"),
    strip.background = element_rect(fill = "seashell2", color = "seashell2")
  )
p_pred_dom_tax_raw


p_pred_dom_fun <- dt_pred %>%
  filter(response %in% c("functional_richness_plot", 
                         "functional_nearerst_neighbour_distance_plot", 
                         "functional_diversity_plot",
                         "functional_dispersion_plot"
  ) & 
    grepl("dominance", tier)) %>%
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "seashell3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "seashell3") +
  geom_point(
    data = dt_points %>% filter(term_name != "plant_evenness_plot" & response_name %in% c("functional_richness_plot", 
                                                                                          "functional_nearerst_neighbour_distance_plot", 
                                                                                          "functional_diversity_plot",
                                                                                          "functional_dispersion_plot")),
    aes(x = term_value, y = response_value),
    alpha = 0.15, color = "black", size = 1
  ) +
  geom_ribbon(aes(x = var_value, ymax = conf.high, ymin = conf.low, linetype = sig, fill = sig), alpha = 0.25) +
  geom_line(aes(x = var_value, y = predicted, linetype = sig, color = sig), linewidth = 1.05) +
  scale_linetype_manual(values = c("non-significant" = "dashed", "significant" = "solid")) +
  scale_fill_manual(values = c("grey50", "orange2")) +
  scale_color_manual(values = c("grey50", "orange2")) +
  facet_grid(rows = vars(clean_response), cols = vars(clean_term), scales = "free") +
  labs(y = "Response Value", x = "Predictor Value") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = .5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(linetype = "dashed", color = "seashell1"),
    panel.grid.major.y = element_line(linetype = "dashed", color = "seashell1"),
    panel.background = element_rect(fill = "seashell1", color = "seashell1"),
    strip.text = element_text(size = 10, face = "italic"),
    strip.background = element_rect(fill = "seashell2", color = "seashell2")
  )
p_pred_dom_fun


# 4. Combine plots -------------------

p_empty <- ggplot() + theme_void()
# 4.1 Dominance ---------------------------
p_pred_dom_tax <- grid.arrange(p_pred_dom_tax_raw, p_empty, widths = c(3.6, 1))

p_pred_dom <- grid.arrange(p_pred_dom_tax, p_pred_dom_fun, heights = c(1, 3.5))
p_comb_dom <- grid.arrange(p_est_dom_tax, p_empty, p_pred_dom, ncol = 3, widths = c(1.25,0.1, 2))

ggsave(plot = p_comb_dom, "builds/plots/main/diversity_mechanism.png", dpi = 600, height = 8, width = 10)


## Components for inkscape ----------------

p_est_dom_tax_sr <- dt_res %>%
  filter(response %in% c("plant_richness_plot"
  ) & 
    grepl("dominance", tier)) %>%
  dplyr::select(-label) %>% 
  left_join(label_df_dom) %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25") +
  #facet_grid2(rows = vars(clean_response), scales = "free_y", space = "free_y") +
  facet_wrap(~ label, ncol = 1) +
  scale_color_manual(values = c("grey50", "orange2")) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term, color = sig), linewidth = 1.3, alpha = 0.9) +
  labs(y = "", x = "Estimate") +
  theme_minimal() +
  scale_y_discrete(limits = rev) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = .5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(linetype = "dashed", color = "seashell3"),
    panel.grid.major.y = element_line(linetype = "dashed", color = "seashell3"),
    panel.background = element_rect(fill = "seashell1", color = "seashell1"),
    strip.text = element_text(size = 10, face = "italic"),
    strip.background = element_rect(fill = "seashell2", color = "seashell2")
  )
p_est_dom_tax_sr

p_pred_dom_tax_sr <- dt_pred %>%
  filter(response %in% c("plant_richness_plot"
  ) & 
    grepl("dominance", tier)) %>%
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "seashell3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "seashell3") +
  geom_point(
    data = dt_points %>% filter(!term_name %in% c("plant_evenness_plot", 
                                                  "plant_richness_plot") & response_name %in% c("plant_richness_plot") ),
    aes(x = term_value, y = response_value),
    alpha = 0.15, color = "black", size = 1
  ) +
  geom_ribbon(aes(x = var_value, ymax = conf.high, ymin = conf.low, linetype = sig, fill = sig), alpha = 0.25) +
  geom_line(aes(x = var_value, y = predicted, linetype = sig, color = sig), linewidth = 1.05) +
  scale_linetype_manual(values = c("non-significant" = "dashed", "significant" = "solid")) +
  scale_fill_manual(values = c("grey50", "orange2")) +
  scale_color_manual(values = c("grey50", "orange2")) +
  facet_grid(rows = vars(clean_response), cols = vars(clean_term), scales = "free") +
  labs(y = "Response Value", x = "Predictor Value") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = .5),
   # axis.title.x = element_blank(),
    #axis.title.y = element_text(color = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(linetype = "dashed", color = "seashell1"),
    panel.grid.major.y = element_line(linetype = "dashed", color = "seashell1"),
    panel.background = element_rect(fill = "seashell1", color = "seashell1"),
    strip.text = element_text(size = 10, face = "italic"),
    strip.background = element_rect(fill = "seashell2", color = "seashell2")
  )
p_pred_dom_tax_sr



p_est_dom_tax_fd <- dt_res %>%
  filter(response %in% c(
                         "functional_nearerst_neighbour_distance_plot", 
                         "functional_diversity_plot",
                         "functional_dispersion_plot", 
                         "functional_richness_plot"
  ) & 
    grepl("dominance", tier)) %>%
  dplyr::select(-label) %>% 
  left_join(label_df_dom) %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25") +
  #facet_grid2(rows = vars(clean_response), scales = "free_y", space = "free_y") +
  facet_wrap(~ label, ncol = 1) +
  scale_color_manual(values = c("grey50", "orange2")) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term, color = sig), linewidth = 1.3, alpha = 0.9) +
  labs(y = "", x = "Estimate") +
  theme_minimal() +
  scale_y_discrete(limits = rev) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = .5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(linetype = "dashed", color = "seashell3"),
    panel.grid.major.y = element_line(linetype = "dashed", color = "seashell3"),
    panel.background = element_rect(fill = "seashell1", color = "seashell1"),
    strip.text = element_text(size = 10, face = "italic"),
    strip.background = element_rect(fill = "seashell2", color = "seashell2")
  )
p_est_dom_tax_fd

p_pred_dom_fd <- dt_pred %>%
  filter(response %in% c("functional_richness_plot", 
                         "functional_nearerst_neighbour_distance_plot", 
                         "functional_diversity_plot",
                         "functional_dispersion_plot"
  ) & 
    grepl("dominance", tier)) %>%
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "seashell3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "seashell3") +
  geom_point(
    data = dt_points %>% filter(term_name != "plant_evenness_plot" & response_name %in% c("functional_richness_plot", 
                                                                                          "functional_nearerst_neighbour_distance_plot", 
                                                                                          "functional_diversity_plot",
                                                                                          "functional_dispersion_plot")),
    aes(x = term_value, y = response_value),
    alpha = 0.15, color = "black", size = 1
  ) +
  geom_ribbon(aes(x = var_value, ymax = conf.high, ymin = conf.low, linetype = sig, fill = sig), alpha = 0.25) +
  geom_line(aes(x = var_value, y = predicted, linetype = sig, color = sig), linewidth = 1.05) +
  scale_linetype_manual(values = c("non-significant" = "dashed", "significant" = "solid")) +
  scale_fill_manual(values = c("grey50", "orange2")) +
  scale_color_manual(values = c("grey50", "orange2")) +
  facet_grid(rows = vars(clean_response), cols = vars(clean_term), scales = "free") +
  labs(y = "Response Value", x = "Predictor Value") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = .5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(linetype = "dashed", color = "seashell1"),
    panel.grid.major.y = element_line(linetype = "dashed", color = "seashell1"),
    panel.background = element_rect(fill = "seashell1", color = "seashell1"),
    strip.text = element_text(size = 10, face = "italic"),
    strip.background = element_rect(fill = "seashell2", color = "seashell2")
  )
p_pred_dom_fd


p_pred_d_sr <- grid.arrange(p_est_dom_tax_sr,
                            p_empty,
                            p_pred_dom_tax_sr,
                            p_empty, 
                            widths = c(1.25, 0.1, 2, 0.575))
ggsave(plot = p_pred_d_sr, 
       "builds/plots/inkscape/fig_3_components/diversity_mechanism_species_richness.png",
       dpi = 600, height = 2.5, width = 12)

p_pred_d_fd <- grid.arrange(p_est_dom_tax_fd,
                            p_empty,
                            p_pred_dom_fd,
                            widths = c(1.25, 0.1, 2.6))
ggsave(plot = p_pred_d_fd, 
       "builds/plots/inkscape/fig_3_components/diversity_mechanism_fun_div.png",
       dpi = 600, height = 7.5, width = 12)

## combine 

p_pred_ink <- grid.arrange(p_pred_d_sr, p_pred_d_fd, heights = c(1, 3.1))
ggsave(plot = p_pred_ink, 
       "builds/plots/inkscape/fig_3_components/diversity_mechanism.png",
       dpi = 600, height = 10, width = 12)

# 4.2 Evenness ---------------------------
ggsave(plot = p_est_eve_tax, "builds/plots/supplement/diversity_mechanism_estimates_evenness.png", dpi = 600, height = 3, width = 12)


#### check life form specific relationships 

label_df_dom_lf <- dt_res %>%
  filter(response %in% c("graminoid_richness_plot", 
                         "forb_richness_plot", 
                         "woody_richness_plot"
  ) & 
    grepl("dominance", tier)) %>% 
  distinct(clean_response, .keep_all = TRUE) %>%
  mutate(
    label = paste0(clean_response, "\n(R²m = ", round(rsq_m, 2), "; R²c = ", round(rsq_c, 2)),
    label = factor(label, levels = label[order(clean_response)])
  ) %>%
  select(clean_response, label)

p_est_dom_lf <- dt_res %>%
  filter(response %in% c("graminoid_richness_plot", 
                         "forb_richness_plot", 
                         "woody_richness_plot"
  ) & 
    grepl("dominance", tier)) %>%
  dplyr::select(-label) %>% 
  left_join(label_df_dom_lf) %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25") +
  #facet_grid2(rows = vars(clean_response), scales = "free_y", space = "free_y") +
  facet_wrap(~ label, ncol = 1) +
  scale_color_manual(values = c("grey50", "orange2")) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term, color = sig), linewidth = 1.3, alpha = 0.9) +
  labs(y = "", x = "Estimate") +
  theme_minimal() +
  scale_y_discrete(limits = rev) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = .5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(linetype = "dashed", color = "seashell3"),
    panel.grid.major.y = element_line(linetype = "dashed", color = "seashell3"),
    panel.background = element_rect(fill = "seashell1", color = "seashell1"),
    strip.text = element_text(size = 10, face = "italic"),
    strip.background = element_rect(fill = "seashell2", color = "seashell2")
  )
p_est_dom_lf
