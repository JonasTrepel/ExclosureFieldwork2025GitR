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
  dplyr::select(berger_parker_plot, plant_evenness_plot, point_return_fraction_plot, mean_point_height_plot, plant_richness_plot) %>% 
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
  mutate(formula_id = paste0("formula_", 1:nrow(.)), 
         formula = paste0(response, " ~ ", vars), 
         intercept_only_formula = paste0(response, " ~ 1 + (1 | exclosure_id/cluster_id)")) %>% 
  mutate(term_tier = ifelse(grepl("evenness", vars), "evenness", "dominance"),
         response_tier = ifelse(grepl("functional", response), "fun_div", "richness"), 
         tier = paste0(response_tier, "_", term_tier)
         )

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
  
  
  m0 <- tryCatch({
    
    glmmTMB(intercept_only_formula, data = dt_mod)
    
  }, error = function(e) {cat("Model", i, "failed: ", e$message, "\n") 
    return(NULL) })
  
  m <- tryCatch({
    
    glmmTMB(formula, data = dt_mod)
    
  }, error = function(e) {cat("Model", i, "failed: ", e$message, "\n") 
    return(NULL) })
  
  if(is.null(m)){next}
  
  m_sum <- summary(m)
  
  tmp <- data.table(response = filter_resp)
  
  delta_aicc <- AICc(m0) - AICc(m)
  delta_aic <- AIC(m0) - AIC(m)
  delta_bic <- BIC(m0) - BIC(m)
  
  rsq_m <-  as.numeric(r.squaredGLMM(m)[1])
  rsq_c <-  as.numeric(r.squaredGLMM(m)[2])
  
  
  tmp <- tmp %>% 
    mutate(
      rsq_m = round(rsq_m, 3),
      rsq_c = round(rsq_c, 3),
      formula = guide[i,]$formula, 
      response = guide[i,]$response,
      tier = guide[i,]$tier,
      scale = guide[i,]$scale,
      formula_id = guide[i,]$formula_id, 
      delta_aicc = delta_aicc, 
      delta_aic = delta_aic, 
      delta_bic = delta_bic)
  
  dt_res <- rbind(dt_res, tmp)
  
  ## extract estimates 
  tidy_m <- broom.mixed::tidy(m)
  
  
  ## bring in good shape 
  tmp_est <- tidy_m %>%
    filter(effect == "fixed") %>% 
    mutate(
      ci_ub = estimate + (std.error*1.96),
      ci_lb = estimate - (std.error*1.96), 
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

fwrite(dt_model_res, "builds/model_outputs/diversity_glmms_model_results.csv")
fwrite(dt_pred, "builds/model_outputs/diversity_glmms_model_predictions.csv")
