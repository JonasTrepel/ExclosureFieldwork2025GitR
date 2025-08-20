library(data.table)
library(tidyverse)
library(glmmTMB)
library(tidylog)



dt <- fread("data/processed/clean/long_data_with_lnrr.csv")

#what about species per family?
m1 <- glmmTMB(ln_rr ~ 1 + (1 | exclosure_id/cluster_id), data = dt[dt$response_name == "n_families_plot"])
summary(m1)
exp(0.19)
#mean number of species per family 
m2 <- glmmTMB(ln_rr ~ 1 + (1 | exclosure_id/cluster_id), data = dt[dt$response_name == "mean_species_per_family_plot"])
summary(m2)

exp(0.15)
