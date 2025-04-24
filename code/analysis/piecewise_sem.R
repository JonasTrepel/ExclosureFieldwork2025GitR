library("lavaan")
library("piecewiseSEM")
library(data.table)
library(tidyverse)
library(MuMIn)
library(glmmTMB)

dt <- fread("data/processed/clean/long_data_with_lnrr.csv") %>% 
  mutate(biome = ifelse(setup_id %in% c("addo_nyathi_full", "addo_jack"), "Thicket", "Savanna")) %>% 
  dplyr::select(c(response_name, ln_rr, pair_id, cluster_id, exclosure_id, setup_id, biome)) %>% 
  pivot_wider(names_from = "response_name", values_from = "ln_rr", id_cols = c(pair_id, cluster_id, exclosure_id, setup_id, biome)) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .))) %>% 
  as.data.table() %>% 
  dplyr::select(plant_richness_plot, berger_parker_plot, mean_point_height_plot, point_return_fraction_plot, 
                functional_diversity_plot, functional_nearerst_neighbour_distance_plot, 
                functional_specialization_plot, functional_dispersion_plot,
                exclosure_id, cluster_id) %>%
  filter(complete.cases(.))


cor(dt %>% dplyr::select(where(is.numeric))) %>% round(2)

## To keep it simple, I'd just go with plant richness, fun div and functional distance for now. 


m_psem <- psem(
  
  #Plant Species Richness 
  glmmTMB(plant_richness_plot ~
            berger_parker_plot +
            mean_point_height_plot +
            point_return_fraction_plot +
            (1 | exclosure_id/cluster_id), na.action = na.omit,
          data = dt),
  
  #Functional Diversity 
  glmmTMB(functional_diversity_plot ~
            berger_parker_plot +
            mean_point_height_plot +
            point_return_fraction_plot +
            plant_richness_plot +
            (1 | exclosure_id/cluster_id), na.action = na.omit,
          data = dt),
 
  #Plant Functional Distance
  glmmTMB(functional_nearerst_neighbour_distance_plot ~
            berger_parker_plot +
            mean_point_height_plot +
            point_return_fraction_plot +
            plant_richness_plot +
            (1 | exclosure_id/cluster_id), na.action = na.omit,
          data = dt),
  
  #Dispersion
  glmmTMB(functional_dispersion_plot ~
            berger_parker_plot +
            mean_point_height_plot +
            point_return_fraction_plot +
            plant_richness_plot +
            (1 | exclosure_id/cluster_id), na.action = na.omit,
          data = dt),


  
  # Correlated errors
  functional_diversity_plot %~~% functional_nearerst_neighbour_distance_plot,
  functional_diversity_plot %~~% functional_dispersion_plot,
  functional_dispersion_plot %~~% functional_nearerst_neighbour_distance_plot,

  data = dt
)
s_m <- summary(m_psem)
s_m
LLchisq(m_psem)
AIC(m_psem)
plot(m_psem)

anova(m_psem)

### Let plant species richness be explained by life form specific richness 

dt_lf <- fread("data/processed/clean/long_data_with_lnrr.csv") %>% 
  mutate(biome = ifelse(setup_id %in% c("addo_nyathi_full", "addo_jack"), "Thicket", "Savanna")) %>% 
  dplyr::select(c(response_name, ln_rr, pair_id, cluster_id, exclosure_id, setup_id, biome)) %>% 
  pivot_wider(names_from = "response_name", values_from = "ln_rr", id_cols = c(pair_id, cluster_id, exclosure_id, setup_id, biome)) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .))) %>% 
  as.data.table() %>% 
  dplyr::select(plant_richness_plot, woody_richness_plot, forb_richness_plot, graminoid_richness_plot,
                exclosure_id, cluster_id) %>%
  filter(complete.cases(.))


summary(glmmTMB(plant_richness_plot ~ woody_richness_plot + 
                  forb_richness_plot + 
                  graminoid_richness_plot + 
                  (1 | exclosure_id/cluster_id), 
                data = dt_lf))

### Quick Plot

dt_est_raw <- s_m$coefficients %>% 
  as.data.table()
dt_est_raw$sig_sym <- dt_est_raw[, 9]
dt_est_raw[, 9] <- NULL


dt_rsq <- data.table(s_m$R2)

dt_est <- dt_est_raw %>% 
  filter(!grepl("~~", Response)) %>% 
  left_join(dt_rsq) %>% 
  rename(estimate = Estimate) %>% 
  mutate(
    clean_response = case_when(
      .default = NA,
      Response == "plant_richness_plot" ~ "Plant Richness",
      Response == "woody_richness_plot" ~ "Woody Richness",
      Response == "forb_richness_plot" ~ "Forb Richness",
      Response == "graminoid_richness_plot" ~ "Graminoid Richness",
      Response == "shannon_diversity_plot" ~ "Shannon Diversity",
      Response == "functional_nearerst_neighbour_distance_plot" ~ "Plant Functional Distance", 
      Response == "functional_diversity_plot" ~ "Plant Functional Diversity", 
      Response == "functional_specialization_plot" ~ "Plant Functional Specialization", 
      Response == "functional_dispersion_plot" ~ "Plant Functional Dispersion"
    ),
    clean_response = factor(clean_response, levels = c(
      "Plant Richness", "Shannon Diversity", "Graminoid Richness", "Forb Richness", "Woody Richness", 
      "Plant Functional Diversity", "Plant Functional Distance", "Plant Functional Specialization", "Plant Functional Dispersion"
    )),
    clean_term = case_when(
      .default = NA,
      Predictor == "plant_richness_plot" ~ "Plant Richness",
      Predictor == "berger_parker_plot" ~ "Plant Dominance",
      Predictor == "plant_evenness_plot" ~ "Plant Evenness",
      Predictor == "point_return_fraction_plot" ~ "Vegetation Density",
      Predictor == "mean_point_height_plot" ~ "Vegetation Height"
    ),
    clean_term = factor(clean_term, levels = c(
      "Plant Richness", "Plant Dominance", "Plant Evenness",
      "Vegetation Density", "Vegetation Height"
    )),
  ) %>% 
  mutate(
    std_error = as.numeric(Std.Error), 
    ci_lb = estimate - 1.96 * std_error, 
    ci_ub = estimate + 1.96 * std_error, 
    significance = case_when(
      .default = "Non significant", 
      estimate > 0 & P.Value < 0.05 ~ "Significantly positive", 
      estimate < 0 & P.Value < 0.05 ~ "Significantly negative"
    ), 
    Label = paste0(clean_response, "\n(R²m = ", Marginal, "; R²c = ", Conditional, ")"),
    Label = factor(Label, levels = c(unique(Label)))
  )

library(MetBrewer)
c(MetBrewer::met.brewer(name = "Archambault", n = 7))
#"#88a0dc" "#381a61" "#7c4b73" "#ed968c" "#ab3329" "#e78429" "#f9d14a"
p1 <- dt_est %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term, color = significance), 
                  linewidth = 1.3, size = 0.9, alpha = 0.75) +
  scale_color_manual(values = c("Non significant" = "grey", "Significantly positive" = "orange2","Significantly negative" = "orange2")) +
  facet_wrap(~ Label, ncol = 4) +
  labs(y = "", color = "") +
  scale_y_discrete(limits = rev) +
  theme_minimal() + 
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
p1

ggsave(plot = p1, "builds/plots/supplement/psem_estimates.png", dpi = 600, height = 3, width = 10)
