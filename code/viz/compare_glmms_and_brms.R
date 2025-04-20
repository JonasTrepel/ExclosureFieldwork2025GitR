## compare estimates 


library(tidyverse)
library(data.table)

### Full Dataset
brm_full <- fread("builds/model_outputs/raw_brm_estimates_full_dataset.csv")  %>% 
  rename(brm_estimate = estimate,
         brm_ci_ub = ci_ub, 
         brm_ci_lb = ci_lb, 
         brm_std_error = std.error)
glmm_full <- fread("builds/model_outputs/raw_glmm_estimates_full_dataset.csv")%>% 
  rename(glmm_estimate = estimate,
         glmm_ci_ub = ci_ub, 
         glmm_ci_lb = ci_lb, 
         glmm_std_error = std.error)

dt_full <- glmm_full %>% 
  dplyr::select(term, scale, response_name, glmm_estimate, glmm_ci_ub, glmm_ci_lb, glmm_std_error) %>% 
  left_join(brm_full %>% 
              dplyr::select(term, scale, response_name, brm_estimate, brm_ci_ub, brm_ci_lb, brm_std_error)) %>% 
  filter(scale == "plot")


cor <- cor.test(dt_full$glmm_estimate, dt_full$brm_estimate)
cor.test(dt_full$glmm_std_error, dt_full$brm_std_error)$estimate

p_fe <- dt_full %>% 
  ggplot() +
  geom_abline(color = "black", linetype = "dashed", alpha = 0.5)+
  annotate("text", x = -0.25, y = 0.25,
           label = paste0("cor = ", round(cor.test(dt_full$glmm_estimate, dt_full$brm_estimate)$estimate, 4))) +
  geom_point(aes(x = glmm_estimate, y = brm_estimate)) +
  labs(x = "GLMM Estimate", y = "BRM Estimate", title = "Estimate Full Dataset") +
  theme_classic()
p_fe

p_fse <- dt_full %>% 
  ggplot() +
  geom_abline(color = "black", linetype = "dashed", alpha = 0.5)+
  annotate("text", x = 0.05, y = 0.2,
           label = paste0("cor = ", round(cor.test(dt_full$glmm_std_error, dt_full$brm_std_error)$estimate, 4))) +
  geom_point(aes(x = glmm_std_error, y = brm_std_error)) +
  labs(x = "GLMM Standard Error", y = "BRM Standard Error",  title = "Standard Error Full Dataset") +
  theme_classic()
p_fse

### Site 
brm_sites <- fread("builds/model_outputs/raw_brm_estimates_sites.csv")  %>% 
  rename(brm_estimate = estimate,
         brm_ci_ub = ci_ub, 
         brm_ci_lb = ci_lb, 
         brm_std_error = std.error)
glmm_sites <- fread("builds/model_outputs/raw_glmm_estimates_sites.csv")%>% 
  rename(glmm_estimate = estimate,
         glmm_ci_ub = ci_ub, 
         glmm_ci_lb = ci_lb, 
         glmm_std_error = std.error)

dt_sites <- glmm_sites %>% 
  dplyr::select(term, scale, response_name, glmm_estimate, glmm_ci_ub, glmm_ci_lb, glmm_std_error) %>% 
  left_join(brm_sites %>% 
              dplyr::select(term, scale, response_name, brm_estimate, brm_ci_ub, brm_ci_lb, brm_std_error)) %>% 
  filter(scale == "plot")


cor.test(dt_sites$glmm_estimate, dt_sites$brm_estimate)
cor.test(dt_sites$glmm_std_error, dt_sites$brm_std_error)$estimate

p_se <- dt_sites %>% 
  ggplot() +
  geom_abline(color = "black", linetype = "dashed", alpha = 0.5)+
  annotate("text", x = -0.5, y = 0.5,
           label = paste0("cor = ", round(cor.test(dt_sites$glmm_estimate, dt_sites$brm_estimate)$estimate, 4))) +
  geom_point(aes(x = glmm_estimate, y = brm_estimate)) +
  labs(x = "GLMM Estimate", y = "BRM Estimate",  title = "Estimate Sites") +
  theme_classic()
p_se
p_sse <- dt_sites %>% 
  ggplot() +
  geom_abline(color = "black", linetype = "dashed", alpha = 0.5)+
  annotate("text", x = 0.1, y = 0.35,
           label = paste0("cor = ", round(cor.test(dt_sites$glmm_std_error, dt_sites$brm_std_error)$estimate, 4))) +
  geom_point(aes(x = glmm_std_error, y = brm_std_error)) +
  labs(x = "GLMM Standard Error", y = "BRM Standard Error",  title = "Standard Error Sites") +
  theme_classic()
p_sse

# Biome 

brm_biome <- fread("builds/model_outputs/raw_brm_estimates_biomes.csv")  %>% 
  rename(brm_estimate = estimate,
         brm_ci_ub = ci_ub, 
         brm_ci_lb = ci_lb, 
         brm_std_error = std.error)
glmm_biome <- fread("builds/model_outputs/raw_glmm_estimates_biomes.csv")%>% 
  rename(glmm_estimate = estimate,
         glmm_ci_ub = ci_ub, 
         glmm_ci_lb = ci_lb, 
         glmm_std_error = std.error)

dt_biome <- glmm_biome %>% 
  dplyr::select(term, scale, response_name, glmm_estimate, glmm_ci_ub, glmm_ci_lb, glmm_std_error) %>% 
  left_join(brm_biome %>% 
              dplyr::select(term, scale, response_name, brm_estimate, brm_ci_ub, brm_ci_lb, brm_std_error)) %>% 
  filter(scale == "plot")


cor.test(dt_biome$glmm_estimate, dt_biome$brm_estimate)
cor.test(dt_biome$glmm_std_error, dt_biome$brm_std_error)$estimate

p_be <- dt_biome %>% 
  ggplot() +
  geom_abline(color = "black", linetype = "dashed", alpha = 0.5)+
  annotate("text", x = -0.4, y = 0.5,
           label = paste0("cor = ", round(cor.test(dt_biome$glmm_estimate, dt_biome$brm_estimate)$estimate, 4))) +
  geom_point(aes(x = glmm_estimate, y = brm_estimate)) +
  labs(x = "GLMM Estimate", y = "BRM Estimate",  title = "Estimate Biome") +
  theme_classic()
p_be

p_bse <- dt_biome %>% 
  ggplot() +
  geom_abline(color = "black", linetype = "dashed", alpha = 0.5)+
  annotate("text", x = 0.1, y = 0.35,
           label = paste0("cor = ", round(cor.test(dt_biome$glmm_std_error, dt_biome$brm_std_error)$estimate, 4))) +
  geom_point(aes(x = glmm_std_error, y = brm_std_error)) +
  labs(x = "GLMM Standard Error", y = "BRM Standard Error",  title = "Standard Error Biome") +
  theme_classic()
p_bse

library(gridExtra)
p_comp <- grid.arrange(p_fe, p_se, p_be, p_fse, p_sse, p_bse, ncol = 3)
ggsave(plot=p_comp, "builds/plots/supplement/compare_brm_and_glmm.png", dpi = 600, height = 6, width = 9)
