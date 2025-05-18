######## 

library(data.table)
library(tidyverse)
library("MuMIn")
library(tidyr)
library(broom)
library(brms)
library(tidybayes)
library(glmmTMB)
library("sjPlot")
library("tictoc")
library(broom.mixed)


dt_mod <- fread("data/processed/clean/long_data_with_lnrr.csv") %>% 
  mutate(biome = ifelse(setup_id %in% c("addo_nyathi_full", "addo_jack"), "Thicket", "Savanna")) %>% 
  dplyr::select(c(response_name, ln_rr, pair_id, cluster_id, exclosure_id, setup_id, biome)) %>% 
  pivot_wider(names_from = "response_name", values_from = "ln_rr", id_cols = c(pair_id, cluster_id, exclosure_id, setup_id, biome)) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .))) %>% 
  as.data.table()


dt_corr <- dt_mod %>% 
  dplyr::select(berger_parker_plot, plant_evenness_plot, point_return_fraction_plot, mean_point_height_plot) %>% 
  filter(complete.cases(.))
corr <- cor(dt_corr) 
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)

########################### BUILD MODEL GUIDE ############################

########################### Plot scale ####################################

responses_plot <- c(
  ## Taxonomic diversity 
  "plant_richness_plot",
  "shannon_diversity_plot",
  "graminoid_richness_plot",
  "forb_richness_plot",
  "woody_richness_plot", 
  
  ## Functional Diversity 
  "functional_dispersion_plot",
  "functional_diversity_plot", 
  "functional_specialization_plot",
  "functional_nearerst_neighbour_distance_plot"
)

vars_plot <- c(
  "berger_parker_plot + mean_point_height_plot + point_return_fraction_plot + (1 | exclosure_id/cluster_id)",
  "plant_evenness_plot + mean_point_height_plot + point_return_fraction_plot + (1 | exclosure_id/cluster_id)" 
  
)

## build
guide <- CJ(vars = vars_plot, 
            response = responses_plot, 
            scale = "plot") %>% 
  mutate(term_tier = ifelse(grepl("evenness", vars), "evenness", "dominance"),
         response_tier = ifelse(grepl("functional", response), "fun_div", "richness"), 
         tier = paste0(response_tier, "_", term_tier)
  ) %>% 
  mutate(formula_id = paste0("formula_", 1:nrow(.)), 
         formula = ifelse(response_tier == "richness", 
                          paste0(response, " ~ ", vars), 
                          paste0(response, " ~ plant_richness_plot + ", vars)),
         intercept_only_formula = paste0(response, " ~ 1 + (1 | exclosure_id/cluster_id)"))

table(guide$tier)




#### run models --------

i = 7
tic()
dt_res <- data.table()
dt_estimates <- data.table()
dt_pred <- data.table()

for(i in 1:nrow(guide)){
  
  formula_id <- guide[i,]$formula_id
  vars <- guide[i,]$vars
  tier <- guide[i,]$tier
  
  filter_resp <- unique(guide[i, ]$response)
  
  
  print(paste0(i," - response: ", filter_resp,"; tier: ", tier))
  
  guide <- guide %>% data.table() %>% as_tibble() %>% as.data.table() 
  
  
  #get formulas 
  formula <- as.formula(guide[i,]$formula)
  
  intercept_only_formula <- as.formula(guide[i,]$intercept_only_formula)
  
  if(grepl("dominance", tier)){
    m_prior <- c(
      set_prior("normal(0, 10)", class = "b", coef = "berger_parker_plot"),  
      set_prior("normal(0, 10)", class = "b", coef = "point_return_fraction_plot"),
      set_prior("normal(0, 10)", class = "b", coef = "mean_point_height_plot"),
      set_prior("normal(0, 10)", class = "Intercept")
    )
  }else if(grepl("evenness", tier)){
    m_prior <- c(
      set_prior("normal(0, 10)", class = "b", coef = "plant_evenness_plot"),
      set_prior("normal(0, 10)", class = "b", coef = "point_return_fraction_plot"),
      set_prior("normal(0, 10)", class = "b", coef = "mean_point_height_plot"),
      set_prior("normal(0, 10)", class = "Intercept")
    )
     }

  
  i_prior <- c(
    set_prior("normal(0, 10)", class = "Intercept")
  )
  
  
  m0 <- tryCatch({
    
    brm(intercept_only_formula, data = dt_mod, prior = i_prior)
    
  }, error = function(e) {cat("Model", i, "failed: ", e$message, "\n") 
    return(NULL) })
  
  m <- tryCatch({
    
    brm(formula, data = dt_mod, prior = m_prior)
    
  }, error = function(e) {cat("Model", i, "failed: ", e$message, "\n") 
    return(NULL) })
  
  if(is.null(m)){next}
  
  m_sum <- summary(m)
  
  tmp <- data.table(response = filter_resp)
  library(loo)
  
  delta_loo <- as.numeric(loo::loo(m0)$estimates[3]) - as.numeric(loo::loo(m)$estimates[3])

  bayes_R2(m)[1] 
  
  bayes_rsq <-  as.numeric(bayes_R2(m)[1])
  
  tmp <- tmp %>% 
    mutate(
      bayes_rsq = round(bayes_rsq, 3),
      formula = guide[i,]$formula, 
      response = guide[i,]$response,
      tier = guide[i,]$tier,
      scale = guide[i,]$scale,
      formula_id = guide[i,]$formula_id, 
      delta_loo = delta_loo)
  
  dt_res <- rbind(dt_res, tmp)
  
  ## extract estimates 
  tidy_m <- broom.mixed::tidy(m)
  
  
  ## bring in good shape 
  tmp_est <- tidy_m %>%
    filter(effect == "fixed") %>% 
    rename(ci_ub = conf.high, 
           ci_lb = conf.low) %>% 
    mutate(
      # ci_ub = estimate + (std.error*1.96),
      # ci_lb = estimate - (std.error*1.96), 
      response_tier = tier, 
      formula_id = guide[i,]$formula_id,
      response = filter_resp) %>% 
    filter(!effect == "ran_pars") %>% 
    mutate(group = NULL)
  
  dt_estimates <- rbind(dt_estimates, tmp_est)
  
  var_names <- tidy_m %>%
    dplyr::select(term) %>%
    filter(!grepl("ntercept", term) & term != "sd__Observation") %>%
    filter(!grepl("\\:", term))
  
  for(j in 1:nrow(var_names)){
    
    var <- var_names[j,] %>% pull()
    
    clean_var = paste0(gsub("_scaled", "", var))
    
    p <- plot_model(m, term = var, type = "pred", ci.lvl = 0.95, show.values = T)
    
    marg_tmp <- p$data %>% 
      as.data.table() %>% 
      rename(var_value = x) %>% 
      mutate(term = var) %>% 
      dplyr::select(-group, -group_col) 
    
    if(j==1){
      marg <- marg_tmp}else{
        marg <- rbind(marg, marg_tmp)}
  }
  
  tmp_pred <- marg %>% 
    mutate(tier = tier, 
           response = filter_resp,
           formula_id = formula_id)
  
  dt_pred <- rbind(dt_pred, tmp_pred)
  
  
}
toc()
print("loop done")

dt_model_res <- dt_estimates %>% left_join(dt_res) %>% unique(,)

fwrite(dt_model_res, "builds/model_outputs/diversity_brms_model_results.csv")
fwrite(dt_pred, "builds/model_outputs/diversity_brms_model_predictions.csv")
