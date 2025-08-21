
library(tidyverse)
library(ggplot2)
library(data.table)
library(ggridges)
library(gridExtra)
library(sf)
library(ggspatial)
library(mapview)
library(rnaturalearth)
library(scico)


plots <- st_read("data/spatial/clean_plot_locations_south_africa_exclosures_2025.gpkg")
exclosures <- st_read("data/spatial/south_africa_exclosure_boundaries_2025.gpkg")

knp <- st_read("data/spatial/knp_shape.gpkg")
pnr <- st_read("data/spatial/pnr_shape.gpkg")
addo <- st_read("data/spatial/aenp_shape.gpkg")

pas <- rbind(knp, pnr, addo)


#1. South Africa -----------------
south_africa <- ne_countries(scale = 50, country = "South Africa") %>% st_transform(crs = 4326)
mapview::mapview(south_africa)

p_south_africa <- ggplot() +
  geom_sf(data = south_africa, fill = "seashell2", color = "grey25") + 
  geom_sf(data = pas, fill = "seashell3" ) +
  geom_sf(data = exclosures, fill = "black" ) +
  xlim(c(17, 34)) +
  ylim(c(35, 22)) +
  annotation_scale(style = "ticks") +
  theme_void()
p_south_africa

ggsave(plot = p_south_africa, "builds/plots/inkscape/fig_1_components/south_africa.png", dpi = 600)

#2. Exlosures and Plots ---------------

p_nyathi <- ggplot() +
  geom_sf(data = exclosures %>% 
            filter(grepl("Nyathi Full", name)), fill = "white", color = "grey25", linetype = "dashed", linewidth = .5) + 
  geom_sf(data = plots %>% 
            filter(grepl("addo_n", plot_id)) , fill = "black", shape = 22, alpha = 0.9) +
  theme_void()
p_nyathi

ggsave(plot = p_nyathi, "builds/plots/inkscape/fig_1_components/addo_nyathi.png", height = 3, width = 3, dpi = 600)

p_jack <- ggplot() +
  geom_sf(data = exclosures %>% 
            filter(grepl("Jack's", name)), fill = "white", color = "grey25", linetype = "dashed", linewidth = .5) + 
  geom_sf(data = plots %>% 
            filter(grepl("addo_jack", plot_id)) , fill = "black", shape = 22, alpha = 0.9) +
  theme_void()
p_jack

ggsave(plot = p_jack, "builds/plots/inkscape/fig_1_components/addo_jack.png", height = 3, width = 3, dpi = 600)

p_pnr <- ggplot() +
  geom_sf(data = exclosures %>% 
            filter(grepl("Border", name)), fill = "white", color = "grey25", linetype = "dashed", linewidth = .5) + 
  geom_sf(data = plots %>% 
            filter(grepl("pnr", plot_id)) , fill = "black", shape = 22, alpha = 0.9) +
  theme_void()
p_pnr

ggsave(plot = p_pnr, "builds/plots/inkscape/fig_1_components/pnr.png", height = 3, width = 3, dpi = 600)

p_nkuhlu <- ggplot() +
  geom_sf(data = exclosures %>% 
            filter(grepl("Nkuhlu", name) & treatment == "full_exclosure"), fill = "white", color = "grey25", linetype = "dashed", linewidth = .5) + 
  geom_sf(data = plots %>% 
            filter(grepl("knp_nkuhlu", plot_id)) , fill = "black", shape = 22, alpha = 0.9) +
  theme_void()
p_nkuhlu

ggsave(plot = p_nkuhlu, "builds/plots/inkscape/fig_1_components/knp_nkuhlu.png", height = 3, width = 3, dpi = 600)

p_satara <- ggplot() +
  geom_sf(data = exclosures %>% 
            filter(grepl("Satara", name) & treatment == "full_exclosure"), fill = "white", color = "grey25", linetype = "dashed", linewidth = .5) + 
  geom_sf(data = plots %>% 
            filter(grepl("knp_satara", plot_id)) , fill = "black", shape = 22, alpha = 0.9) +
  theme_void()
p_satara

ggsave(plot = p_satara, "builds/plots/inkscape/fig_1_components/knp_satara.png", height = 3, width = 3, dpi = 600)

p_roan <- ggplot() +
  geom_sf(data = exclosures %>% 
            filter(grepl("Nwaswitshumbe", name)), fill = "white", color = "grey25", linetype = "dashed", linewidth = .5) + 
  geom_sf(data = plots %>% 
            filter(grepl("knp_roan", plot_id)) , fill = "black", shape = 22, alpha = 0.9) +
  theme_void()
p_roan

ggsave(plot = p_roan, "builds/plots/inkscape/fig_1_components/knp_roan.png", height = 3, width = 3, dpi = 600)

## north arrow 

p_na <- ggplot() +
  geom_sf(data = exclosures %>% 
            filter(grepl("Jack's", name)), fill = "white", color = "white", linetype = "dashed", linewidth = .5) + 
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(2.5, "cm"), width = unit(2.5, "cm")) +  theme_void()
p_na
ggsave(plot = p_na, "builds/plots/inkscape/fig_1_components/north_arrow.png", height = 3, width = 3, dpi = 600)
