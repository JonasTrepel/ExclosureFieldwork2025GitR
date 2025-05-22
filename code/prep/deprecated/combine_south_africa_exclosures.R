library(sf)
library(mapview)
library(tidyverse)

nyathi <- read_sf("/Users/jonas/Library/CloudStorage/Dropbox/PhD/Projects/Fieldwork/2025/gis/gis_data/addo/nyathi_exclosures.gpkg") %>% 
  mutate(treatment = ifelse(grepl("Full", Name), "full_exclosure", "partial_exclosure")) %>% 
  dplyr::select(name = Name, treatment) %>% 
  st_transform(crs = 4326) %>% 
  st_as_sf() %>% 
  rename(geometry = geom) %>% 
  st_make_valid()

mapview(nyathi)

jack <- read_sf("/Users/jonas/Library/CloudStorage/Dropbox/PhD/Projects/Fieldwork/2025/gis/gis_data/addo/jacks_exclosure.gpkg") %>% 
  mutate(treatment = "partial_exclosure") %>% 
  dplyr::select(name = Name, treatment) %>% 
  st_transform(crs = 4326) %>% 
  rename(geometry = geom) %>% 
  st_as_sf() %>% 
  st_make_valid()

mapview(jack)


pnr <- read_sf("/Users/jonas/Library/CloudStorage/Dropbox/PhD/Projects/Fieldwork/2025/gis/gis_data/pongola_nature_reserve/pnr_border_strip.gpkg") %>% 
  mutate(treatment = "full_exclosure", 
         name = "PNR Border Strip") %>% 
  dplyr::select(name, treatment) %>% 
  st_transform(crs = 4326)
mapview(pnr)

knp <- read_sf("/Users/jonas/Library/CloudStorage/Dropbox/PhD/Projects/Fieldwork/2025/gis/gis_data/kruger/knp_exclosures_with_satara.gpkg") %>% 
  filter(NAME %in% c("Nwaswitshumbe Enclosure", "Nkuhlu Exclosure", "Satara")) %>% 
  mutate(treatment = ifelse(Treatment %in% c("Partial Exclosure"), "partial_exclosure", "full_exclosure")) %>% 
  dplyr::select(name = NAME, treatment) %>% 
  group_by(name, treatment) %>% 
  summarize() %>% 
  st_transform(crs = 4326) %>% 
  rename(geometry = geom)

mapview(knp)


exclosures <- rbind(nyathi, jack, pnr, knp) %>% 
  st_sf() %>% st_make_valid()

mapview(exclosures)

plots <- st_read("/Users/jonas/Downloads/plots.gpkg")
st_write(plots, "data/spatial/plot_locations_south_africa_exclosures_2025.gpkg", append = FALSE)


st_write(exclosures, "data/spatial/south_africa_exclosure_boundaries_2025.gpkg", append = FALSE)


### plot 
library(rnaturalearth)
south_africa <- ne_countries(scale = 50, country = "South Africa") %>% st_transform(crs = 4326)
plots <- read_sf("/Users/jonas/Library/CloudStorage/Dropbox/PhD/Projects/Fieldwork/2025/gis/gis_data/plots/plots.gpkg")
mapview::mapview(plots)

p_south_africa <- ggplot() +
  geom_sf(data = south_africa, fill = "grey95", color = "grey50") + 
  geom_sf(data = exclosures, fill = "grey95", color = "black") + 
  xlim(c(17, 34)) +
  ylim(c(35, 22)) +
  theme_void()
p_south_africa


p_excl <- ggplot() +
  geom_sf(data = exclosures %>% 
            filter(grepl("Nwaswitshumbe", name)), fill = "grey95", color = "black") + 
  geom_sf(data = plots %>% 
            filter(grepl("roan", site_id)), fill = "black") +
  #xlim(c(17, 34)) +
  #ylim(c(35, 22)) +
  theme_void()
p_excl
