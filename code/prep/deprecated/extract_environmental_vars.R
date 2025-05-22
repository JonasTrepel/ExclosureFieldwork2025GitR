### extract environmental variables 
library(mapview)
library(terra) 
library(tidyverse)
library(data.table)
library(sf)
library(exactextractr)
library(rgee)

# plots <- st_read("data/spatial/raw_plot_locations_south_africa_exclosures_2025.gpkg")
# unique(plots$site_id)
# 
# mapview(plots)
# 
# plots_clean <- plots %>% 
#   mutate(site_id = case_when(
#     .default = site_id, 
#     site_id %in% c("addo_nyathi", "add_nyathi_full_in", "addo_full_nyathi_in") ~ "addo_nyathi_full_in", 
#     site_id %in% c("addo_jacks_in") ~ "addo_jack_in", 
#     site_id %in% c("addo_jack_oit") ~ "addo_jack_out", 
#     site_id %in% c("kno_roan_in") ~ "knp_roan_in",
#     plot_id == "addo_jack_in_13" & photo ==  "DCIM/JPEG_20250207093437349.jpg" ~ "addo_jack_out", 
#     
#   ), 
#   plot_id = case_when(
#     .default = plot_id, 
#     plot_id == "addo_jack_in_13" & photo ==  "DCIM/JPEG_20250206130910644.jpg" ~ "addo_jack_in_14", 
#     plot_id == "addo_jack_in_17" & photo ==  "DCIM/JPEG_20250207072757531.jpg" ~ "addo_jack_in_18", 
#     plot_id == "addo_jack_in_13" & photo ==  "DCIM/JPEG_20250207093437349.jpg" ~ "addo_jack_out_13",
#     plot_id == "addo_n1_full_out_3" & photo ==  "DCIM/JPEG_20250210094926730.jpg" ~ "addo_n1_full_out_0",
#     plot_id == "knp_roan_in_1" & photo ==  "DCIM/JPEG_20250226081142721.jpg" ~ "knp_roan_in_6",
#     plot_id == "knp_satara_in_15" & photo ==  "DCIM/JPEG_20250222132932075.jpg" ~ "knp_satara_in_16",
#     plot_id == "pnr_out" & photo ==  "DCIM/JPEG_20250214082927287.jpg" ~ "pnr_out_12" 
#     
#   ))
# 
# 
# st_write(plots_clean, "data/spatial/clean_plot_locations_south_africa_exclosures_2025.gpkg", append = FALSE)

plots <- st_read("data/spatial/clean_plot_locations_south_africa_exclosures_2025.gpkg") %>% 
  st_buffer(10)
mapview(plots)
exclosures <- st_read("data/spatial/south_africa_exclosure_boundaries_2025.gpkg") %>% 
  filter(!grepl("Nyathi Partial", name))


## elevation --------

ele <- rast("../../../../Resources/spatial/Elevation_ZAF/Elevation_SA_90m.tif")
crs(ele) # 

## Plots 
plots_ele <- st_transform(plots, crs = crs(ele))
plots_ele_extr <- exactextractr::exact_extract(ele, plots_ele, 
                                         append_cols = c("plot_id", "site_id"),
                                         fun = "mean")
setnames(plots_ele_extr, "mean", "elevation_plot")

## Exclosures 
exclosures_ele <- st_transform(exclosures, crs = crs(ele))
exclosures_ele_extr <- exactextractr::exact_extract(ele, exclosures_ele, 
                                               append_cols = c("name"),
                                               fun = "mean")
setnames(exclosures_ele_extr, "mean", "elevation_exclosure")

## MAP---------------
map <- rast("../../../../Resources/spatial/Chelsa_Climate/CHELSA_bio12_1981-2010_V.2.1.tif") 


## Plots 
plots_map <- st_transform(plots, crs = crs(map))
plots_map_extr <- exactextractr::exact_extract(map, plots_map, 
                                               append_cols = c("plot_id", "site_id"),
                                               fun = "mean")
setnames(plots_map_extr, "mean", "map_plot")

## Exclosures 
exclosures_map <- st_transform(exclosures, crs = crs(map))
exclosures_map_extr <- exactextractr::exact_extract(map, exclosures_map, 
                                                    append_cols = c("name"),
                                                    fun = "mean")
setnames(exclosures_map_extr, "mean", "map_exclosure")

## MAP---------------
mat <- rast("../../../../Resources/spatial/Chelsa_Climate/CHELSA_bio1_1981-2010_V.2.1.tif") 


## Plots 
plots_mat <- st_transform(plots, crs = crs(mat))
plots_mat_extr <- exactextractr::exact_extract(mat, plots_mat, 
                                               append_cols = c("plot_id", "site_id"),
                                               fun = "mean")
setnames(plots_mat_extr, "mean", "mat_plot")

## Exclosures 
exclosures_mat <- st_transform(exclosures, crs = crs(mat))
exclosures_mat_extr <- exactextractr::exact_extract(mat, exclosures_mat, 
                                                    append_cols = c("name"),
                                                    fun = "mean")
setnames(exclosures_mat_extr, "mean", "mat_exclosure")

# rgee -------------

ee_Initialize(project = "ee-jonastrepel", drive = TRUE)

### EVI ------

start_date <- paste0("2001-01-01")
end_date <- paste0("2024-12-31")

evi_img <- ee$
  ImageCollection('MODIS/061/MOD13A1')$
  map(function(img) {
    qa <- img$select("SummaryQA")
    img$updateMask(qa$eq(0))})$ ## select only high quality data 
  select('EVI')$
  filterDate(start_date, end_date)$
  mean()


plot_extr_evi <- ee_extract(x = evi_img, y = plots["plot_id"] %>% st_buffer(100), sf = FALSE, scale = 500)%>% 
  rename(evi_plot = EVI)

ndvi_img <- ee$
  ImageCollection('MODIS/061/MOD13A1')$
  map(function(img) {
    qa <- img$select("SummaryQA")
    img$updateMask(qa$eq(0))})$ ## select only high quality data 
  select('NDVI')$
  filterDate(start_date, end_date)$
  mean()


plot_extr_ndvi <- ee_extract(x = ndvi_img, y = plots["plot_id"] %>% st_buffer(100), sf = FALSE, scale = 500)%>% 
  rename(ndvi_plot = NDVI)

npp_img <- ee$
  ImageCollection('MODIS/061/MOD17A3HGF')$
  select('Npp')$
  filterDate(start_date, end_date)$
  mean()


plot_extr_npp <- ee_extract(x = npp_img, y = plots["plot_id"] %>% st_buffer(100), sf = FALSE, scale = 500) %>% 
  rename(npp_plot = Npp)



plot_env <- plots_ele_extr %>% 
  left_join(plots_map_extr) %>% 
  left_join(plots_mat_extr) %>% 
  left_join(plot_extr_npp) %>% 
  left_join(plot_extr_ndvi) %>% 
  left_join(plot_extr_evi)

fwrite(plot_env, "data/processed/fragments/plot_environmental.csv")

plot(plot_env$evi_plot, plot_env$map_plot)  

cor.test(plot_env$evi_plot, plot_env$map_plot)  

ggplot() +
  geom_point(data = plot_env, aes(x = npp_plot, y = npp_plot, color = site_id))
