### calculate plot functional diversity 


library(mFD)
library(tidyverse)
library(data.table)
library(vegan)
library(tidylog)



dt_sp <- fread("data/processed/fragments/plot_species_and_data.csv")
dt_traits <- fread("data/processed/fragments/species_trait_data_exclosures_2025.csv")


###### calculate FD -----------------------------------

### categorical traits have to be coded as factor 
n <- n_distinct(dt_sp$species) #number of species

#number of breakpoints using Sturges rule
k <- ceiling(log2(n) + 1)

glimpse(dt_traits)

trait_data <- dt_traits %>%
  mutate(leaf_area = ifelse(leaf_type == "a", 0, leaf_area)) %>% # absent leaves have an area of 0... 
  mutate(
    growth_form = as.factor(growth_form),
    spines = as.factor(spines), 
    biomass_density = as.factor(biomass_density),
    leaf_type = as.factor(leaf_type), 
    
    plant_height_max = as.numeric(plant_height_max), 
    leaf_area = as.numeric(leaf_area)) %>%
  mutate(height_bin = as.factor(cut_number(plant_height_max, n = k)),
         leaf_area_bin = as.factor(cut_number(leaf_area, n = k))) %>%
  dplyr::select(species,
                height_bin, leaf_area_bin,
                growth_form, spines, biomass_density, leaf_type) %>% 
  unique() %>%
  filter(complete.cases(.))

summary(trait_data)
unique(trait_data$leaf_area_bin)


### build trait data frame for functions 
sp_tr <-  trait_data %>% dplyr::select(species, growth_form,
                                       leaf_area_bin, height_bin,
                                       spines, biomass_density, leaf_type) %>% 
  filter(complete.cases(.)) %>% 
  remove_rownames %>% 
  column_to_rownames(var="species") 


### build trait categories 
tr_cat <- data.table(
  trait_name = c("growth_form","leaf_area_bin", "height_bin", "spines", "biomass_density", "leaf_type"), 
  trait_type = c("N", "N", "N", "N","N", "N"), 
  fuzzy_name = NA
)

### compute functional entities 
fe <- mFD::sp.to.fe(sp_tr = sp_tr, tr_cat = tr_cat)
fe$fe_nb_sp

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


### compute metrics for alpha functional diversity 
alpha_fd_res <- mFD::alpha.fd.fe(
  asb_sp_occ       = asb_sp_occ, 
  sp_to_fe         = fe,
  ind_nm           = c('fred', 'fored', 'fvuln'),
  check_input      = TRUE, 
  details_returned = TRUE)


## extract simple functional diversity metrics
dt_sfd <- alpha_fd_res$asb_fdfe %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "plot_id")

summary(dt_sfd)
hist(dt_sfd$fred)
## distance based functional diversity metrics. 


#### as we're dealing with categorical traits we have to use the gower distance 
tr_cat_fdist <- tr_cat
tr_cat_fdist$trait_weight <- c(1, 1, 1, 1, 1, 1) ## 
fdist <- mFD::funct.dist(sp_tr = sp_tr, tr_cat = tr_cat_fdist, metric = "gower")

df_fdist <- dist.to.df(list(dA = fdist))

#### distance based functional diversity metric 
fd_hill_res <- alpha.fd.hill(asb_sp_w = weight_mat, sp_dist = fdist)

## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)

#### get functional spaces 

fspaces_quality <- mFD::quality.fspaces(
  sp_dist = fdist,
)

quality.fspaces.plot(fspaces_quality = fspaces_quality,
                     quality_metric = "mad",
                     fspaces_plot = c("pcoa_3d", "pcoa_4d", "pcoa_5d", "pcoa_6d", "pcoa_7d"))

round(fspaces_quality$"quality_fspaces", 3) %>% arrange(mad)
### lowest number is the best one

#### get matrix of species coordinates 

sp_faxes_coord <- fspaces_quality$"details_fspaces"$"sp_pc_coord"

#### compute functional richeness (volume occupied in multidimensional space)
nono_plots <- unique(dt_sp[dt_sp$plant_richness_plot < 2, plot_id])
weight_mat_multi <- weight_mat[!(rownames(weight_mat) %in% c(nono_plots)), ]
#weight_mat_multi <- weight_mat[!(rownames(weight_mat) %in% c()), ]

alpha_fd_indices <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")],
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


dt_dbfd <- fd_hill_res$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "plot_id")
hist(dt_dbfd$FD_q1)

dt_fd_plot <- dt_dbfd %>% 
  left_join(dt_sfd) %>% 
  left_join(dt_afdi) %>% 
  as.data.table() %>% 
  rename(functional_redundancy_plot = fred,
         functional_vulnerability_plot = fvuln, 
         
         functional_diversity_plot = FD_q1, 
         
         functional_dispersion_plot = fdis, # 1 -- main functional diversity metric distance to local pool    #Functional Dispersion: the biomass weighted deviation of species traits values from the center of the functional space filled by the assemblage i.e. the biomass-weighted mean distance to the biomass-weighted mean trait values of the assemblage.
         functional_pairwise_distance_plot = fmpd, #Functional Mean Pairwise Distance: the mean weighted distance between all species pairs. 
         functional_nearerst_neighbour_distance_plot = fnnd, #1 #Functional Mean Nearest Neighbour Distance: the weighted distance to the nearest neighbor within the assemblage.
         functional_originality_plot = fori, #nearest neighbour to global pool.  #Functional Originality: the weighted mean distance to the nearest species from the global species pool.
         functional_specialization_plot = fspe #1  #distance to global pool. #Functional Specialization: the biomass weighted mean distance to the mean position of species from the global pool (present in all assemblages).
  ) %>% 
  dplyr::select(plot_id, functional_redundancy_plot, functional_vulnerability_plot,
                functional_diversity_plot,
                functional_dispersion_plot, functional_pairwise_distance_plot,
                functional_nearerst_neighbour_distance_plot, functional_originality_plot, 
                functional_specialization_plot) 
