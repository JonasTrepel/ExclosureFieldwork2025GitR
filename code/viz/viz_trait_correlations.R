library(scales)
library(tidyverse)
library(data.table)

dt_traits <- fread("data/processed/fragments/species_trait_data_exclosures_2025.csv") %>%
  mutate(leaf_area = ifelse(leaf_type == "a", 0, leaf_area)) %>% # absent leaves have an area of 0... 
  mutate(
    growth_form = as.factor(growth_form_simple),
    growth_form = factor(growth_form, levels = c(
      "round_herb",  "messy_herb", "creeping_herb",         
      "climbing_herb", "cushion_herb",        
      "parasitic_herb", "straight_herb", 
      "single_stemmed_woody", "multi_stemmed_woody")),
    spines = as.factor(spines),
    spines = factor(spines, levels = c("n", "<2", ">2")),
    biomass_density_ordinal = factor(biomass_density_ordinal,
                                     levels = sort(unique(biomass_density_ordinal)),
                                     ordered = TRUE),
    leaf_type = as.factor(leaf_type), 
    leaf_type = factor(leaf_type, levels = c(
      "absent", "linear", "simple", "lobed",
      "palmately_compound", "pinnately_compound", "succulent")),
    plant_height_max = as.numeric(plant_height_max), 
    leaf_area = as.numeric(leaf_area)) %>%
  dplyr::select(species,
                plant_height_max, leaf_area,
                growth_form, spines, biomass_density_ordinal, leaf_type) %>% 
  unique() %>%
  filter(complete.cases(.)) %>% 
  mutate(clean_growth_form = case_when(
    growth_form == "straight_herb"        ~ "Straight Herb",
    growth_form == "round_herb"           ~ "Round Herb",
    growth_form == "single_stemmed_woody" ~ "Single-stem Woody",
    growth_form == "messy_herb"           ~ "Messy Herb",
    growth_form == "climbing_herb"        ~ "Climbing Herb",
    growth_form == "multi_stemmed_woody"  ~ "Multi-stem Woody",
    growth_form == "creeping_herb"        ~ "Creeping Herb",
    growth_form == "cushion_herb"         ~ "Cushion Herb",
    growth_form == "parasitic_herb"       ~ "Parasitic Herb"), 
    clean_growth_form = factor(clean_growth_form, levels = c(
      "Round Herb", "Messy Herb","Creeping Herb",
      "Climbing Herb", "Cushion Herb",
      "Parasitic Herb", "Straight Herb", 
      "Single-stem Woody", "Multi-stem Woody"
    )))

unique(dt_traits$growth_form)

(p_gf_height <- dt_traits %>% 
  ggplot() +
  geom_boxplot(aes( x = clean_growth_form, y = plant_height_max), outlier.shape = NA) +
  geom_jitter(aes( x = clean_growth_form, y = plant_height_max), alpha = 0.5, size = 0.5) +
  labs(x = "Growth Form", y = "Plant Height (cm)") +
  theme_classic() +
  scale_x_discrete(labels = label_wrap(10)))

ggsave(plot = p_gf_height, "builds/plots/supplement/growth_form_vs_height.png", dpi = 600, height = 4, width = 8)


str(dt_traits)


cor.test(dt_traits$plant_height_max, dt_traits$leaf_area)
plot(dt_traits$plant_height_max, dt_traits$leaf_area)


#### Pairwise correlations --------------

library(lsr)     
library(vcd)     


# variables 

dt_traits2 <- dt_traits %>% 
  mutate(biomass_density_ordinal = as.numeric(biomass_density_ordinal))

num_vars <- c("plant_height_max", "leaf_area", "biomass_density_ordinal")
cat_vars <- c("growth_form", "spines", "leaf_type")


all_vars <- c(num_vars, cat_vars)

# all pairs 
pairs <- combn(all_vars, 2, simplify = FALSE)

# function to calculate correlation
pairwise_cor <- function(v1, v2, data) {
  x <- data[[v1]]
  y <- data[[v2]]
  
  # numeric vs numeric
  if ((v1 %in% c(num_vars)) & (v2 %in% c(num_vars))) {
    val <- suppressWarnings(cor(as.numeric(x), as.numeric(y), method = "spearman", use = "pairwise.complete.obs"))
    method <- "Spearman"
    
    # numeric vs categorical --> use anova and calulate effect size 
  } else if ((v1 %in% c(num_vars)) & (v2 %in% cat_vars)) {
   
#doing what mfD did
    mod <- stats::kruskal.test(as.numeric(x) ~ as.factor(y))
    data2 <- data.frame(x = x, y = y)
    val <- rstatix::kruskal_effsize(data = data2, 
                                    formula = as.numeric(x) ~ as.factor(y))[1, "effsize"]

    method <- "Kruskal-Wallis/Eta²"
  } else if ((v2 %in% c(num_vars)) & (v1 %in% cat_vars)) {
    
    mod <- stats::kruskal.test(as.numeric(y) ~ as.factor(x))
    data2 <- data.frame(x = x, y = y)
    val <- rstatix::kruskal_effsize(data = data2, 
                                    formula = as.numeric(y) ~ as.factor(x))[1, "effsize"]
    
    method <- "Kruskal-Wallis/Eta²"
    
    # categorical vs categorical 
  } else if ((v1 %in% cat_vars) & (v2 %in% cat_vars)) {
    val <- suppressWarnings(assocstats(table(x, y))$cramer)
    method <- "Cramer’s V"
    
  } else {
    val <- NA
    method <- "NA"
  }
  
  res <- data.frame(
    var1 = as.character(v1),
    var2 = as.character(v2),
    value = as.numeric(val),
    method = as.character(method),
    stringsAsFactors = FALSE
  )
  
  return(res)
}


# do it
results <- do.call(rbind, lapply(pairs, function(p) pairwise_cor(p[1], p[2], dt_traits2)))

dt_cor <- results %>%
  mutate(
    `Variable 1` = case_when(
      var1 == "plant_height_max" ~ "Plant Height Max",
      var1 == "leaf_area" ~ "Leaf Area",
      var1 == "growth_form" ~ "Growth Form",
      var1 == "spines" ~ "Spines",
      var1 == "biomass_density_ordinal" ~ "Biomass Density",
      var1 == "leaf_type" ~ "Leaf Type",
    ),
    `Variable 2` = case_when(
      var2 == "plant_height_max" ~ "Plant Height Max",
      var2 == "leaf_area" ~ "Leaf Area",
      var2 == "growth_form" ~ "Growth Form",
      var2 == "spines"~ "Spines",
      var2 == "biomass_density_ordinal" ~ "Biomass Density",
      var2 == "leaf_type" ~ "Leaf Type",
    ), 
    value = round(value, 3)
  ) %>% dplyr::select(
`Variable 1`, `Variable 2`, Value = value, Method = method)

fwrite(dt_cor, "builds/model_outputs/trait_correlations.csv")
