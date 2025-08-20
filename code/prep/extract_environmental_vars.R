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
ee$Initialize(project='jonas-trepel') 

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

################## FIRE ########################
#fire frequency 

year_list <- ee$List$sequence(2001, 2024)
n_years <- 2024-2001

modis_burndate <- ee$ImageCollection("MODIS/061/MCD64A1")$
  select("BurnDate")

modis_qa <- ee$ImageCollection("MODIS/061/MCD64A1")$
  select("QA")

# Bitwise extraction function (bit 0: valid burn)
bitwise_extract <- function(input, from_bit, to_bit) {
  mask_size <- ee$Number(1)$add(to_bit)$subtract(from_bit)
  mask <- ee$Number(1)$leftShift(mask_size)$subtract(1)
  input$rightShift(from_bit)$bitwiseAnd(mask)
}

# Function to get annual binary burn map (1 = burned, 0 = unburned)
get_annual_binary <- function(year) {
  year <- ee$Number(year)
  next_year <- year$add(1)
  
  # BurnDate and QA filtered by July 1 - June 30
  start_date <- ee$Date$fromYMD(year, 7, 1)
  end_date <- ee$Date$fromYMD(next_year, 6, 30)
  
  burn_img <- modis_burndate$
    filterDate(start_date, end_date)$
    select("BurnDate")$
    max()
  
  qa_img <- modis_qa$
    filterDate(start_date, end_date)$
    select("QA")$
    max()
  
  # Mask: burn pixels with good quality
  mask <- bitwise_extract(qa_img, 0, 0)$eq(1)
  
  # Burned = 1, Unburned = 0
  burned_bin <- burn_img$
    where(burn_img$neq(0), 1)$
    unmask(0)$
    updateMask(mask)$
    rename("Burned")$
    set("system:time_start", start_date)
  
  return(burned_bin)
}

# Build the annual binary image collection
burned_col <- ee$ImageCollection$fromImages(
  year_list$map(ee_utils_pyfunc(function(yr) {
    get_annual_binary(yr)
  }))
)

# Sum the collection to get fire frequency
fire_frequency <- burned_col$sum()$divide(n_years)

# Visualization
vis_params <- list(
  min = 0,
  max = 1,
  palette = c("#ffffff", "#ffffb2", "#fd8d3c", "#e31a1c", "#b10026")
)
Map$addLayer(fire_frequency, vis_params, "Fire Frequency")

#### time since last fire 

# Create an image collection with the burn year (instead of just 1/0)
get_annual_burnyear <- function(year) {
  year <- ee$Number(year)
  next_year <- year$add(1)
  
  # July 1 – June 30
  start_date <- ee$Date$fromYMD(year, 7, 1)
  end_date <- ee$Date$fromYMD(next_year, 6, 30)
  
  burn_img <- modis_burndate$
    filterDate(start_date, end_date)$
    select("BurnDate")$
    max()
  
  qa_img <- modis_qa$
    filterDate(start_date, end_date)$
    select("QA")$
    max()
  
  mask <- bitwise_extract(qa_img, 0, 0)$eq(1)
  
  # Where burned, assign the year; otherwise 0
  burn_year_img <- burn_img$
    where(burn_img$neq(0), year)$
    unmask(0)$
    updateMask(mask)$
    rename("BurnYear")$
    set("system:time_start", start_date)
  
  return(burn_year_img)
}

# Build annual burn-year collection
burnyear_col <- ee$ImageCollection$fromImages(
  year_list$map(ee_utils_pyfunc(function(yr) {
    get_annual_burnyear(yr)
  }))
)

# Last fire year (per pixel)
last_fire_year <- burnyear_col$max()

# Reference year as an image
ref_year_img <- ee$Image$constant(2024)$rename("ref_year")

# Time since last fire
tslf <- ref_year_img$subtract(last_fire_year)$rename("TSLF")

# Mask out unburned pixels
tslf <- tslf$updateMask(last_fire_year$gt(0))

# Visualization
tslf_vis <- list(
  min = 0,
  max = 23,  # since 2001–2024
  palette = c("#084081","#0868ac","#2b8cbe","#4eb3d3",
              "#7bccc4","#a8ddb5","#ccebc5","#f7fcf0")
)

Map$addLayer(tslf, tslf_vis, "Time Since Last Fire")


## extract 
plot_extr_ff <- ee_extract(x = fire_frequency, y = plots["plot_id"] %>% st_buffer(100), sf = FALSE, scale = 500) %>% 
  rename(fire_frequency = Burned)

plot_extr_tslf <- ee_extract(x = tslf, y = plots["plot_id"] %>% st_buffer(100),
                             sf = FALSE, scale = 500) %>% rename(years_since_last_fire = TSLF)

plot_extr_lfy <- ee_extract(x = last_fire_year, y = plots["plot_id"] %>% st_buffer(100),
                            sf = FALSE, scale = 500) %>% rename(last_fire_year = BurnYear)
###### combine 

plot_env <- plots_ele_extr %>% 
  left_join(plots_map_extr) %>% 
  left_join(plots_mat_extr) %>% 
  left_join(plot_extr_npp) %>% 
  left_join(plot_extr_ndvi) %>% 
  left_join(plot_extr_evi) %>% 
  left_join(plot_extr_ff) %>% 
  left_join(plot_extr_tslf) %>% 
  left_join(plot_extr_lfy)

fwrite(plot_env, "data/processed/fragments/plot_environmental.csv")

plot(plot_env$evi_plot, plot_env$map_plot)  

cor.test(plot_env$evi_plot, plot_env$map_plot)  

ggplot() +
  geom_point(data = plot_env, aes(x = npp_plot, y = npp_plot, color = site_id))




