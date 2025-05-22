#### This repository contains R scripts and data used for the manuscript "Wild large herbivores promote plant diversity and functional redundancy by reducing dominance" (in review)

---
### `code/cleaning`

Scripts to clean raw species data and prepare it for analysis:

- `clean_and_summarize_species_traits.R`  
  Cleans raw species trait data and generates summary metrics for analysis.

- `clean_plot_species_data.R`  
  Prepares species occurrence and abundance data at the plot level.

---

### `code/prep`

Scripts for preparing the main dataset (calculating plant functional composition, preparing lidar and environmental covariate):

1. `convert_blk_to_laz_via_e57.R`  
   Converts raw lidar `.blk` files to `.laz` via intermediate `.e57` format.

2. `get_lidar_results.R`  
   Extracts structural metrics (e.g., vegetation height, density) from lidar point cloud data.

3. `get_herbivore_biomass.R`  
   Estimates herbivore biomass across sites based on reserve specific population counts (provided by land managers).

4. `get_plant_fun_div.R`  
   Calculates functional diversity indices using species traits.

5. *(optional)* `initialise_rgee.R`  
   Initializes Earth Engine authentication for remote sensing data extraction.

6. `extract_environmental_covariates.R`  
   Extracts environmental variables (e.g., temperature, precipitation, NDVI).

7. `combine_all.R`  
   Combines all  data fragments and calculates effect sizes.

---

### `code/analysis`

Main modeling scripts:

- `glmms_full_dataset.R`  
  Fits GLMMs to test effects of herbivores on plant diversity, dominance, vegetation structure and funcitonal composition.

- `glmms_biomes.R`  
  Tests biome-specific responses to large herbivores.

- `glmms_sites.R`  
  Tests site-specific responses to large herbivores.

- `glmm_diversity_hypotheses.R`  
  Tests the relationshhips between herbivore effects on dominance, vegetation structure, plant species richness and funcitonal characteristics 

- `life_form_dominance_glmms.R`  
  Models life form–specific dominance responses (e.g., graminoids, shrubs) to herbivores.

---

### `code/analysis/robustness_tests`

Robustness analysis (e.g., repeating the main analysis in a bayesian framework) and test diversity hypothesis in a confirmatory path analysis:

- `brm_full_dataset.R`  
  Bayesian models (via `brms`) for full dataset hypotheses.

- `brm_biomes.R`, `brm_sites.R`, `brm_diversity_hypotheses.R`  
  Bayesian analogues of GLMMs for biome-, site-, and diversity-specific models.

- `glmms_with_offset_ln_rr.R`  
  Glmms for overall and biome specific effects using log response ratio where 0s in species richness have been replaced with 0.1 to be included in the model.

- `piecewise_sem.R`  
  Confirmatory path analysis / piecewise structural equation modeling to evaluate hypothesized causal networks.

---

### `code/viz`

Scripts for generating publication-quality figures and exploratory plots:

- `compare_glmms_and_brms.R`  
  Visualizes and compares estimates and standard errors across GLMM and BRM outputs.

- `fig_1_components.R`  
  Builds individual components of Figure 1.

- `si_figures.R`  
  Produces Supplementary Information figures.

- `viz_diversity_hypotheses_results_brm.R`  
  Visualizations of BRM results for diversity hypotheses (also in SI).

- `viz_diversity_hypotheses_results_glmm.R`  
  Visualization of GLMM results, often including predicted effects and confidence intervals.


## Data Descriptions
---

### `data/processed/clean/long_data_with_lnrr.csv`

This is the **main effect size dataset** used in the analyses. Each row represents a paired comparison (inside vs. outside exclosure) for a specific response variable.

#### Key Columns:

| Column Name                 | Description                                                                 |
|----------------------------|-----------------------------------------------------------------------------|
| `response_name`            | Name of the ecological response variable (e.g., richness on plot or site scale) |
| `response_value_in`        | Response value inside the exclosure (no herbivore impact)                          |
| `response_value_out`       | Response value outside the exclosure (herbivore impact)                       |
| `ln_rr`                    | log response ratio: `ln(response_in / response_out)`               |
| `rr`                       | response ratio: `response_in / response_out`                           |
| `ln_rr_offset`, `rr_offset`| Versions of response ratios where 0s in species richness have been replaced with 0.1 to avoid NAs         |
| `scale`                    | variable scale (e.g., plot, cluster, site)                                 |
| `mat_pair`, `map_pair`, `npp_pair`, `ndvi_pair`, `evi_pair` | Environmental covariates (mean annual temperature, precipitation, etc.) |
| `pair_id`, `setup_id`, `exclosure_id`, `cluster_number`, `cluster_id` | Identifiers for sampling design |

---

### `data/processed/clean/all_vars.csv`

This dataset contains the **raw or semi-processed variables** at the plot, cluster, and site level before log ratios are computed. 

#### Example Variable Categories:

- **Identifiers and Metadata**:
  - `plot_id`, `site_id`, `date`, `time`, `exclosure_id`, `pair_id`, `cluster_id`, etc.

- **Plant Community Composition**:
  - Richness and cover for functional groups (e.g., `graminoid_richness_plot`, `forb_cover_percent`)
  - Diversity indices (e.g., `shannon_diversity_plot`, `evenness`, `dominance` metrics)

- **Functional Diversity Metrics**:
  - Plot, cluster, and site level:  
    `functional_richness_plot`, `functional_dispersion_cluster`, `functional_specialization_site`, etc.

- **Vegetation Structure (from lidar)**:
  - `mean_point_height_plot` (average vegetation height), `point_return_fraction_plot` (vegetation density), etc.

- **Abiotic Covariates**:
  - `mat`, `map`, `npp`, `ndvi`, `evi`

---


