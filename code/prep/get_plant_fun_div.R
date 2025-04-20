### calculate plot functional diversity 


library(mFD)
library(tidyverse)
library(data.table)
library(vegan)
library(tidylog)



dt_sp <- fread("data/processed/fragments/plot_species_and_data.csv")
dt_traits <- fread("data/processed/fragments/species_trait_data_exclosures_2025.csv")


quantile(dt_sp$cover, na.rm = T)

### calc max cover #######
library("abdiv")
x <- c(15, 6, 4, 0, 3, 0)
max(x)/sum(x)
berger_parker_d(x) # 15 / 28

simpson(x)
vegan::diversity(x, index = "simpson")

max_3_plot <- dt_sp %>%
  dplyr::select(plot_id, cluster_id, site_id, cover) %>%
  arrange(site_id, cluster_id, plot_id, desc(cover)) %>%  
  group_by(site_id, cluster_id, plot_id) %>%
  slice_max(cover, n = 3) %>% 
  summarise(sum_cover = sum(cover, na.rm = TRUE))

max_cover_plot <- dt_sp %>% 
  dplyr::select(plot_id, cluster_id, site_id, cover) %>% 
  left_join(max_3_plot) %>% 
  group_by(site_id, cluster_id, plot_id) %>% 
  summarize(max_cover_plot = max(cover, na.rm = T), 
            community_dominance_plot = max(sum_cover)/sum(cover, na.rm = T), 
            berger_parker_plot = max(cover, na.rm = T)/sum(cover, na.rm = T), 
            simpson_dominance_plot = vegan::diversity(cover, index = "simpson"))

max_3_cluster <- dt_sp %>%
  dplyr::select(cluster_id, site_id,species, cover) %>%
  group_by(site_id, cluster_id, species) %>% 
  summarize(cover = sum(cover, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(site_id, cluster_id, desc(cover)) %>%  
  group_by(site_id, cluster_id) %>%
  slice_max(cover, n = 3) %>% 
  summarise(sum_cover = sum(cover, na.rm = TRUE))


max_cover_cluster <- dt_sp %>% 
  dplyr::select(cluster_id, species, cover, cluster_id, site_id) %>% 
  group_by(site_id, cluster_id, species) %>% 
  summarize(cover = sum(cover, na.rm = T)) %>% 
  dplyr::select(-species) %>% 
  left_join(max_3_cluster) %>%
  group_by(site_id, cluster_id) %>% 
  summarize(max_cover_cluster = max(cover, na.rm = T)/5, 
            community_dominance_cluster = max(sum_cover)/sum(cover, na.rm = T), 
            berger_parker_cluster = max(cover, na.rm = T)/sum(cover, na.rm = T), 
            simpson_dominance_cluster = vegan::diversity(cover, index = "simpson"))

max_3_site <- dt_sp %>%
  dplyr::select(site_id, species, cover) %>%
  group_by(site_id, species) %>% 
  summarize(cover = sum(cover, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(site_id, desc(cover)) %>%  
  group_by(site_id) %>%
  slice_max(cover, n = 3) %>% 
  summarise(sum_cover = sum(cover, na.rm = TRUE))

max_cover_site <- dt_sp %>% 
  dplyr::select(cluster_id, species, cover, cluster_id, site_id) %>% 
  group_by(site_id, species) %>% 
  summarize(cover = sum(cover, na.rm = T)) %>% 
  left_join(max_3_site) %>%
  group_by(site_id)  %>% 
  summarize(site_id_max_cover = max(cover, na.rm = T)/25, 
            community_dominance_site = max(sum_cover)/sum(cover, na.rm = T), 
            berger_parker_site = max(cover, na.rm = T)/sum(cover, na.rm = T), 
            simpson_dominance_site = vegan::diversity(cover, index = "simpson"))


dt_dominance <- max_cover_plot %>% 
  left_join(max_cover_cluster) %>% 
  left_join(max_cover_site)


###### calculate FD -----------------------------------

trait_data <- dt_traits %>%
  mutate(leaf_area = ifelse(leaf_type == "a", 0, leaf_area)) %>% # absent leaves have an area of 0... 
  mutate(
    growth_form = as.factor(growth_form_simple),
    spines = as.factor(spines), 
    biomass_density_ordinal = factor(biomass_density_ordinal,
                                     levels = sort(unique(biomass_density_ordinal)),
                                     ordered = TRUE),    leaf_type = as.factor(leaf_type), 
    plant_height_max = as.numeric(plant_height_max), 
    leaf_area = as.numeric(leaf_area)) %>%
  dplyr::select(species,
                plant_height_max, leaf_area,
                growth_form, spines, biomass_density_ordinal, leaf_type) %>% 
  unique() %>%
  filter(complete.cases(.))

#summary(trait_data)
#unique(trait_data$leaf_area)


### build trait data frame for functions 
sp_tr <-  trait_data %>% dplyr::select(species, growth_form,
                                       leaf_area, plant_height_max,
                                       spines, biomass_density_ordinal, leaf_type) %>% 
  filter(complete.cases(.)) %>% 
  remove_rownames %>% 
  column_to_rownames(var="species") 


### build trait categories 
tr_cat <- data.table(
  trait_name = c("growth_form","leaf_area", "plant_height_max", "spines", "biomass_density_ordinal", "leaf_type"), 
  trait_type = c("N", "Q", "Q", "N","O", "N"), 
  fuzzy_name = NA
)

#### as we're dealing with categorical traits we have to use the gower distance 
tr_cat_fdist <- tr_cat
tr_cat_fdist$trait_weight <- c(1, 1, 1, 1, 1, 1) ## 
fdist <- mFD::funct.dist(sp_tr = sp_tr, tr_cat = tr_cat_fdist, metric = "gower")

#### get functional spaces 

fspaces_quality <- mFD::quality.fspaces(
  sp_dist = fdist,
)

p <- quality.fspaces.plot(fspaces_quality = fspaces_quality,
                          quality_metric = "mad",
                          fspaces_plot = c("pcoa_3d", "pcoa_4d", "pcoa_5d", "pcoa_6d", "pcoa_7d", "pcoa_8d"))
p
ggsave(plot = p, "builds/plots/exploratory/fspaces_quality.png", dpi = 600, height = 8, width = 12)

round(fspaces_quality$"quality_fspaces", 3) %>% arrange(mad) # 6 dimensional space it is 

#### get matrix of species coordinates 

sp_faxes_coord <- fspaces_quality$"details_fspaces"$"sp_pc_coord"

dt_sp <- fread("data/processed/fragments/plot_species_and_data.csv")

##################### alpha diversity ########################

######------------------ plot level --------------------######

# ---------------------- all life forms -------------------- #

plants <- dt_sp %>% 
  dplyr::select(species, cover, plot_id)

### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
weight_mat <- plants %>% 
  dplyr::filter(species %in% c(unique(trait_data$species))) %>% 
  dplyr::select(plot_id, cover, species) %>% 
  pivot_wider(names_from = "species", values_from = "cover") %>% 
  as.data.table() %>% 
  mutate(across(where(is.list), ~ sapply(., toString)),
         across(where(is.character) & !all_of("plot_id"), ~ as.numeric(.)),
         across(everything(), ~ ifelse(is.na(.), 0, .))
  ) %>% 
  remove_rownames %>% 
  column_to_rownames(var="plot_id") %>% 
  as.matrix()

### Get the occurrence dataframe:
asb_sp_summ <- mFD::asb.sp.summary(asb_sp_w = weight_mat) 
asb_sp_occ <- asb_sp_summ$'asb_sp_occ'


#### distance based functional diversity metric 
fd_hill_res <- alpha.fd.hill(asb_sp_w = weight_mat, sp_dist = fdist)

dt_dbfd <- fd_hill_res$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "plot_id")
hist(dt_dbfd$FD_q2)


## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)


#### compute functional richeness (volume occupied in multidimensional space)
nono_plots <- unique(dt_sp[dt_sp$plant_richness_plot < 2, plot_id])
weight_mat_multi <- weight_mat[!(rownames(weight_mat) %in% c(nono_plots)), ]
#weight_mat_multi <- weight_mat[!(rownames(weight_mat) %in% c()), ]

alpha_fd_indices <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6")],
  asb_sp_w         = weight_mat_multi,
  ind_vect         = c("fdis", "fmpd", "fnnd", "fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


alpha_fd_indices$functional_diversity_indices
hist(alpha_fd_indices$functional_diversity_indices$fdis)
hist(alpha_fd_indices$functional_diversity_indices$fmpd)
hist(alpha_fd_indices$functional_diversity_indices$fnnd)
hist(alpha_fd_indices$functional_diversity_indices$fori)
hist(alpha_fd_indices$functional_diversity_indices$fspe)


## extract functional diversity metrics 
dt_afdi <- alpha_fd_indices$functional_diversity_indices %>% 
  rownames_to_column(var = "plot_id") %>% 
  as.data.table() %>% 
  dplyr::select("plot_id", "fdis", "fmpd", "fnnd", "fori", "fspe") 

dt_fd_plot <- dt_dbfd %>% 
  left_join(dt_afdi) %>% 
  as.data.table() %>% 
  rename(functional_diversity_plot = FD_q2, #q2 is recommended by Chao et al 2019 https://esajournals.onlinelibrary.wiley.com/doi/pdfdirect/10.1002/ecm.1343?casa_token=-QbN1gFpfe0AAAAA:7tp9ejf7BHdDGdlWJ5vaJUaj6PQwUTltt7fHDI927IXq-iom2DP7bZpkstN9bzFGDXw3mAuUUE10Txp_
         
         functional_dispersion_plot = fdis, #Functional Dispersion: the biomass weighted deviation of species traits values from the center of the functional space filled by the assemblage i.e. the biomass-weighted mean distance to the biomass-weighted mean trait values of the assemblage.
         functional_pairwise_distance_plot = fmpd, #Functional Mean Pairwise Distance: the mean weighted distance between all species pairs. 
         functional_nearerst_neighbour_distance_plot = fnnd, #Functional Mean Nearest Neighbour Distance: the weighted distance to the nearest neighbor within the assemblage.
         functional_originality_plot = fori, #Functional Originality: the weighted mean distance to the nearest species from the global species pool.
         functional_specialization_plot = fspe #Functional Specialization: the biomass weighted mean distance to the mean position of species from the global pool (present in all assemblages).
  ) %>% 
  dplyr::select(plot_id,
                functional_diversity_plot,
                functional_dispersion_plot, functional_pairwise_distance_plot,
                functional_nearerst_neighbour_distance_plot, functional_originality_plot, 
                functional_specialization_plot) 


##################### alpha diversity ########################
######------------------ Cluster level --------------------######

dt_sp_cluster_raw <- dt_sp %>% 
  dplyr::select(cluster_id, species, cover)

dt_sp_cluster <- dt_sp_cluster_raw %>% 
  group_by(species, cluster_id) %>% 
  summarize(cover = sum(cover, na.rm = T)) %>%
  unique() 

min(dt_sp_cluster$cover)

#---------------------all plants ---------------------------#

### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
weight_mat_cluster <- dt_sp_cluster %>% 
  dplyr::filter(species %in% c(all_of(unique(trait_data$species)))) %>% 
  dplyr::select(cluster_id, cover, species) %>% 
  pivot_wider(names_from = "species", values_from = "cover", values_fn = mean) %>% 
  as.data.table() %>% 
  mutate(across(where(is.list), ~ sapply(., toString)),
         across(where(is.character) & !all_of("cluster_id"), ~ as.numeric(.)),
         across(everything(), ~ ifelse(is.na(.), 0, .))
  ) %>% 
  remove_rownames %>% 
  column_to_rownames(var="cluster_id") %>% 
  as.matrix()

v2 <- colnames(weight_mat_cluster)
v1 <- unique(trait_data$species)

## identify cols that contain only 0
cols_only_zeros <- apply(weight_mat_cluster, 2, function(col) all(col == 0))
sum(cols_only_zeros, na.rm = T)
colnames_only_zeros <- colnames(weight_mat_cluster)[cols_only_zeros]
print(colnames_only_zeros)

setdiff(v1, v2)
setdiff(v2, v1)
#none - good. 

### Get the occurrence dataframe:
asb_sp_summ_cluster <- mFD::asb.sp.summary(asb_sp_w = weight_mat_cluster) 
asb_sp_occ_cluster <- asb_sp_summ_cluster$'asb_sp_occ'

#### distance based functional diversity metric 

fd_hill_res_cluster <- alpha.fd.hill(asb_sp_w = weight_mat_cluster, sp_dist = fdist)

## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)

dt_dbfd_cluster <- fd_hill_res_cluster$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "cluster_id")

#### compute alpha diversity indices in a multidimensional space 
nono_clusters <- unique(dt_sp[dt_sp$plant_richness_cluster < 2, cluster_id])
weight_mat_cluster_multi <- weight_mat_cluster[!(rownames(weight_mat_cluster) %in% c(nono_clusters)), ]


alpha_fd_indices_cluster <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6")],
  asb_sp_w         = weight_mat_cluster_multi,
  ind_vect         = c("fdis", "fmpd", "fnnd", "fori", "fspe"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


#alpha_fd_indices_cluster$functional_diversity_indices

## extract functional diversity metrics 
dt_afdi_cluster <- alpha_fd_indices_cluster$functional_diversity_indices %>% 
  rownames_to_column(var = "cluster_id") %>% 
  as.data.table() %>% 
  dplyr::select("cluster_id", "fdis", "fmpd", "fnnd", "fori", "fspe")


dt_dbfd_cluster <- fd_hill_res_cluster$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "cluster_id")

dt_fd_cluster <- dt_dbfd_cluster %>% 
  left_join(dt_afdi_cluster) %>% 
  as.data.table() %>% 
  rename(
         functional_diversity_cluster = FD_q2, 
         
         functional_dispersion_cluster = fdis, #Functional Dispersion: the biomass weighted deviation of species traits values from the center of the functional space filled by the assemblage i.e. the biomass-weighted mean distance to the biomass-weighted mean trait values of the assemblage.
         functional_pairwise_distance_cluster = fmpd, #Functional Mean Pairwise Distance: the mean weighted distance between all species pairs. 
         functional_nearerst_neighbour_distance_cluster = fnnd, #Functional Mean Nearest Neighbour Distance: the weighted distance to the nearest neighbor within the assemblage.
         functional_originality_cluster = fori, #Functional Originality: the weighted mean distance to the nearest species from the global species pool.
         functional_specialization_cluster = fspe #Functional Specialization: the biomass weighted mean distance to the mean position of species from the global pool (present in all assemblages).
  ) %>% 
  dplyr::select(cluster_id, 
                functional_diversity_cluster,
                functional_dispersion_cluster, functional_pairwise_distance_cluster,
                functional_nearerst_neighbour_distance_cluster, functional_originality_cluster, 
                functional_specialization_cluster) 

hist(dt_fd_cluster$functional_diversity_cluster)

##################### alpha diversity ########################
######------------------ site_id level -------------------#####

dt_sp_site_raw <- dt_sp %>% 
  dplyr::select(life_form, site_id, species, cover)

dt_sp_site <- dt_sp_site_raw %>% 
  group_by(species, site_id) %>% 
  summarize(cover = sum(cover, na.rm = T)) %>% 
  unique() %>% 
  mutate(cover = as.numeric(ifelse(is.na(cover), 1, cover)),
         cover = as.numeric(ifelse(cover == 0, 1, cover))) %>% unique()
min(dt_sp_site$cover)
#---------------------all plants ---------------------------#

### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
which(duplicated(dt_sp_site))

weight_mat_site <- dt_sp_site %>% 
  dplyr::filter(species %in% c(all_of(unique(trait_data$species)))) %>% 
  dplyr::select(site_id, cover, species) %>% 
  pivot_wider(names_from = "species", values_from = "cover", values_fn = mean) %>% 
  as.data.table() %>% 
  mutate(across(where(is.list), ~ sapply(., toString)),
         across(where(is.character) & !all_of("site_id"), ~ as.numeric(.)),
         across(everything(), ~ ifelse(is.na(.), 0, .))
  ) %>% 
  remove_rownames %>% 
  column_to_rownames(var="site_id") %>% 
  as.matrix()

## identify cols that contain only 0
cols_only_zeros <- apply(weight_mat_site, 2, function(col) all(col == 0))
sum(cols_only_zeros, na.rm = T)
colnames_only_zeros <- colnames(weight_mat_site)[cols_only_zeros]
print(colnames_only_zeros)


### Get the occurrence dataframe:
asb_sp_summ_site <- mFD::asb.sp.summary(asb_sp_w = weight_mat_site) 
asb_sp_occ_site <- asb_sp_summ_site$'asb_sp_occ'

#### distance based functional diversity metric 

fd_hill_res_site <- alpha.fd.hill(asb_sp_w = weight_mat_site, sp_dist = fdist)

## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)
dt_dbfd_site <- fd_hill_res_site$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "site_id")

#### compute alpha diversity indices in a multidimensional space 

alpha_fd_indices_site <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6")],
  asb_sp_w         = weight_mat_site,
  ind_vect         = c("fdis", "fmpd", "fnnd", "fori", "fspe"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


alpha_fd_indices_site$functional_diversity_indices

## extract functional diversity metrics 
dt_afdi_site <- alpha_fd_indices_site$functional_diversity_indices %>% 
  rownames_to_column(var = "site_id") %>% 
  as.data.table() %>% 
  dplyr::select("site_id", "fdis", "fmpd", "fnnd", "fori", "fspe")

dt_dbfd_site <- fd_hill_res_site$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "site_id")

dt_fd_site <- dt_dbfd_site %>% 
  left_join(dt_afdi_site) %>% 
  as.data.table() %>% 
  rename(
         functional_diversity_site = FD_q2, 
         
         functional_dispersion_site = fdis, #Functional Dispersion: the biomass weighted deviation of species traits values from the center of the functional space filled by the assemblage i.e. the biomass-weighted mean distance to the biomass-weighted mean trait values of the assemblage.
         functional_pairwise_distance_site = fmpd, #Functional Mean Pairwise Distance: the mean weighted distance between all species pairs. 
         functional_nearerst_neighbour_distance_site = fnnd, #Functional Mean Nearest Neighbour Distance: the weighted distance to the nearest neighbor within the assemblage.
         functional_originality_site = fori, #Functional Originality: the weighted mean distance to the nearest species from the global species pool.
         functional_specialization_site = fspe #Functional Specialization: the biomass weighted mean distance to the mean position of species from the global pool (present in all assemblages).
  ) %>% 
  dplyr::select(site_id,
                functional_diversity_site,
                functional_dispersion_site, functional_pairwise_distance_site,
                functional_nearerst_neighbour_distance_site, functional_originality_site, 
                functional_specialization_site) 

hist(dt_fd_site$functional_diversity_site)

########################## beta diversity within clusters ##################################

# Has to be calculated for each cluster seperately
# Preparation steps are as usual, but we'll loop through the sites

dt_beta_cluster <- data.table(
  functional_beta_diversity_q0_cluster = NA,
  functional_beta_diversity_q1_cluster = NA,
  functional_beta_diversity_q2_cluster = NA,
  sorenson_dissimilarity_cluster = NA, 
  cluster_id = NA
) %>% filter(!is.na(cluster_id))

for(cluster in unique(dt_sp$cluster_id)){
  
  cluster_data_raw <- dt_sp %>% filter(cluster_id %in% c(cluster)) %>% 
    dplyr::select(plot_id, species, cover)
  
  cluster_data <- cluster_data_raw %>% 
    group_by(species, plot_id) %>% 
    summarize(cover = sum(cover, na.rm = T)) %>% 
    unique() 
  
  
  ### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
  cluster_weights <- cluster_data %>% 
    dplyr::filter(species %in% c(all_of(unique(trait_data$species)))) %>% 
    dplyr::select(plot_id, cover, species) %>% 
    pivot_wider(names_from = "species", values_from = "cover") %>% 
    as.data.table() %>% 
    mutate(across(where(is.list), ~ sapply(., toString)),
           across(where(is.character) & !all_of("plot_id"), ~ as.numeric(.)),
           across(everything(), ~ ifelse(is.na(.), 0, .))
    ) %>% 
    remove_rownames() %>% 
    column_to_rownames(var="plot_id") %>% 
    as.matrix()
  
  
  ## calculate 
  tr_cat_fdist <- tr_cat
  tr_cat_fdist$trait_weight <- c(1, 1, 1, 1, 1, 1) #
  fdist <- mFD::funct.dist(sp_tr = sp_tr, tr_cat = tr_cat_fdist, metric = "gower")
  
  
  #### distance based functional diversity metric 
  
  beta_div_cluster <- beta.fd.hill(asb_sp_w = cluster_weights, sp_dist = fdist)
  
  ## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)
  mFD::dist.to.df(list_dist = list("FDq1" = beta_div_cluster$"beta_fd_q"$"q1"))
  
  
  ### calculate beta diversity using the vegan approach (Soerensen dissimilarity index)
  
  tmp_veg_cluster <- betadiver(cluster_weights, method = "w") %>% mean(na.rm = T)
  
  
  ## extract mean beta div
  tmp <- data.table(
    functional_beta_diversity_q0_cluster = mean(beta_div_cluster$beta_fd_q$q0),
    functional_beta_diversity_q1_cluster = mean(beta_div_cluster$beta_fd_q$q1),
    functional_beta_diversity_q2_cluster = mean(beta_div_cluster$beta_fd_q$q2),
    sorenson_dissimilarity_cluster = tmp_veg_cluster, 
    cluster_id = cluster
  )
  
  dt_beta_cluster <- rbind(dt_beta_cluster, tmp)
  
  print(paste0(cluster, " done"))
  
}

dt_beta_cluster
plot(dt_beta_cluster$sorenson_dissimilarity_cluster, dt_beta_cluster$functional_beta_diversity_q1_cluster)
hist(dt_beta_cluster$sorenson_dissimilarity_cluster)

########################## beta diversity within site_id ##################################

# Has to be calculated for each site separately
# Preparation steps are as usual, but we'll loop through the site_ids 

dt_beta <- data.table(
  functional_beta_diversity_q0_site = NA,
  functional_beta_diversity_q1_site = NA,
  functional_beta_diversity_q2_site = NA,
  sorenson_dissimilarity_site = NA, 
  site_id = NA
) %>% filter(!is.na(site_id))

for(site in unique(dt_sp$site_id)){
  
  site_id_data_raw <- dt_sp %>% filter(site_id %in% c(site)) %>% 
    dplyr::select(cluster_id, species, cover)
  
  site_id_data <- site_id_data_raw %>% 
    group_by(species, cluster_id) %>% 
    summarize(cover = sum(cover, na.rm = T)) %>% 
    unique() %>% 
    mutate(cover = as.numeric(ifelse(is.na(cover), 1, cover)),
           cover = as.numeric(ifelse(cover == 0, 1, cover))) %>% unique()
  
  
  ### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
  site_id_weights <- site_id_data %>% 
    dplyr::filter(species %in% c(all_of(unique(trait_data$species)))) %>% 
    dplyr::select(cluster_id, cover, species) %>% 
    pivot_wider(names_from = "species", values_from = "cover") %>% 
    as.data.table() %>% 
    mutate(across(where(is.list), ~ sapply(., toString)),
           across(where(is.character) & !all_of("cluster_id"), ~ as.numeric(.)),
           across(everything(), ~ ifelse(is.na(.), 0, .))
    ) %>% 
    remove_rownames() %>% 
    column_to_rownames(var="cluster_id") %>% 
    as.matrix()
  
  
  ## calculate functional beta div 
  tr_cat_fdist <- tr_cat
  tr_cat_fdist$trait_weight <- c(1, 1, 1, 1, 1, 1) #
  fdist <- mFD::funct.dist(sp_tr = sp_tr, tr_cat = tr_cat_fdist, metric = "gower")
  
  
  #### distance based functional diversity metric 
  
  beta_div <- beta.fd.hill(asb_sp_w = site_id_weights, sp_dist = fdist)
  
  ## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)
  mFD::dist.to.df(list_dist = list("FDq1" = beta_div$"beta_fd_q"$"q1"))
  ##
  
  
  ### calculate beta diversity using the vegan approach (Soerensen dissimilarity index)
  
  tmp_veg_site <- betadiver(site_id_weights, method = "w") %>% mean(na.rm = T)
  
  ## extract mean beta div
  tmp <- data.table(
    functional_beta_diversity_q0_site = mean(beta_div$beta_fd_q$q0),
    functional_beta_diversity_q1_site = mean(beta_div$beta_fd_q$q1),
    functional_beta_diversity_q2_site = mean(beta_div$beta_fd_q$q2),
    sorenson_dissimilarity_site = tmp_veg_site, 
    site_id = site
  )
  
  dt_beta <- rbind(dt_beta, tmp)
  
  print(paste0(site, " done"))
  
}

dt_beta_site <- dt_beta



######### calculate evenness and shannon diversity ########################
library("chemodiv")
library(vegan)

### plot 

evenness_raw_plot <- chemodiv::calcDiv(weight_mat, type = "PielouEven")

plot_ids <- weight_mat %>% 
  as.data.frame() %>%
  rownames_to_column(var = "plot_id") %>% 
  dplyr::select(plot_id)

evenness_plot <- cbind(evenness_raw_plot, plot_ids) %>% 
  rename(plant_evenness_plot = PielouEven)

shannon_diversity_plot <- diversity(weight_mat)
dt_shan_plot <- data.table(
  plot_id = names(shannon_diversity_plot),
  shannon_diversity_plot = unname(shannon_diversity_plot)
)

### cluster 
evenness_raw_cluster <- chemodiv::calcDiv(weight_mat_cluster, type = "PielouEven")

cluster_ids <- weight_mat_cluster %>% 
  as.data.frame() %>%
  rownames_to_column(var = "cluster_id") %>% 
  dplyr::select(cluster_id)

evenness_cluster <- cbind(evenness_raw_cluster, cluster_ids) %>% 
  rename(plant_evenness_cluster = PielouEven)

shannon_diversity_cluster <- vegan::diversity(weight_mat_cluster)

dt_shan_cluster <- data.table(
  cluster_id = names(shannon_diversity_cluster),
  shannon_diversity_cluster = unname(shannon_diversity_cluster)
)


### site_id 
evenness_raw_site <- chemodiv::calcDiv(weight_mat_site, type = "PielouEven")

site_ids <- weight_mat_site %>% 
  as.data.frame() %>%
  rownames_to_column(var = "site_id") %>% 
  dplyr::select(site_id)

evenness_site <- cbind(evenness_raw_site, site_ids) %>% 
  rename(plant_evenness_site = PielouEven)

shannon_diversity_site <- diversity(weight_mat_site)
dt_shan_site <- data.table(
  site_id = names(shannon_diversity_site),
  shannon_diversity_site = unname(shannon_diversity_site)
)


####### conbine to one functional diversity dataframe ######

dt_dominance
dt_fd_plot
dt_fd_site
dt_fd_site
dt_beta_site_fin <- dt_beta_site %>% dplyr::select(functional_beta_diversity_site = functional_beta_diversity_q1_site, 
                                                   sorenson_dissimilarity_site, 
                                                   site_id)

dt_beta_cluster_fin <- dt_beta_cluster %>% dplyr::select(functional_beta_diversity_cluster = functional_beta_diversity_q1_cluster, 
                                                      sorenson_dissimilarity_cluster, 
                                                   cluster_id)
evenness_plot
evenness_cluster
evenness_site
dt_shan_plot
dt_shan_site
dt_shan_cluster

dt_fd_all <- dt_dominance %>% 
  left_join(dt_fd_plot) %>% 
  left_join(dt_fd_cluster) %>% 
  left_join(dt_fd_site) %>% 
  left_join(dt_beta_cluster_fin) %>% 
  left_join(dt_beta_site_fin) %>% 
  left_join(evenness_plot) %>% 
  left_join(evenness_cluster) %>% 
  left_join(evenness_site) %>% 
  left_join(dt_shan_plot) %>% 
  left_join(dt_shan_cluster) %>% 
  left_join(dt_shan_site) 

summary(dt_fd_all)

dt_fd_all %>% 
  ggplot() +
  geom_point(aes(functional_diversity_cluster, functional_pairwise_distance_cluster))
cor.test(dt_fd_all$berger_parker_plot , dt_fd_all$berger_parker_cluster)

fwrite(dt_fd_all, "data/processed/fragments/functional_diversity_metrics.csv")
