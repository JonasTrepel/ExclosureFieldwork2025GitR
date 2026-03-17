#viz lidar 

library(data.table)
library(tidyverse)
library(rlas)
library(gridExtra)
library(grid)
library(scico)

plot_meta <-  fread("data/raw/plot_level_data_2025.csv")

summary(plot_meta)

## load lidar ----------------


### adapt file_path 
files_raw <- as.data.table(list.files("/Volumes/Untitled/lidar_exclosures_2025", pattern = ".laz", full.names = TRUE))


files <- files_raw %>% 
  rename(file_path = V1) %>% 
  mutate(file_name = gsub("/Volumes/Untitled/lidar_exclosures_2025/", "", file_path), 
         plot_id = gsub(".laz", "", file_name)) 


for(i in 1:nrow(files)){
  
  file <- files[i, ]$file_name
  path <- files[i, ]$file_path
  plot_id <- files[i, ]$plot_id
  
  
  # read file
  df <- read.las(path) %>%
    # discard irrelevant variables
    select(X:Z) %>%
    # remove duplicates
    unique() %>%
    # calculate horizontal and 3d distance
    mutate(
      distance_2d = sqrt(X^2 + Y^2),
      distance_3d = sqrt(distance_2d^2 + Z^2),
      # get angle 
      angle_raw = (atan(X / Y) * 180 / pi),
      angle = case_when(
        Y >= 0 ~ angle_raw + 90,
        Y < 0 ~ angle_raw + 270
      ),
      # split scan in 4 equal parts  
      scan_section = case_when(
        angle >= 0 & angle < 90 ~ 1,
        angle >= 90 & angle < 180 ~ 2,
        angle >= 180 & angle < 270 ~ 3,
        angle >= 270 & angle <= 360 ~ 4
      )
    ) %>%
    mutate(plot_id = paste0(plot_id)) %>%
    left_join(plot_meta) %>%
    mutate(
      point_height = Z + ((lidar_scanner_height) / 100)) %>% 
    filter(distance_2d < 7.5) %>% 
    filter(!point_height > 15 & !point_height < -0.5)
  
  
  # Vizualize
  p_a <- df %>%
    sample_n(100000) %>%
    arrange(point_height) %>%
    ggplot(aes(x = X, y = Y, color = point_height)) +
    geom_point(alpha = 0.5, size = 0.25) +
    scale_color_viridis_c(option = "H") +
    labs(x = "X Plane", y = "Y Plane") +
    theme_minimal() +
    theme(legend.position = "none", 
          panel.grid = element_blank())
  p_a
  
  p_b <- df %>%
    sample_n(100000) %>%
    arrange(point_height) %>%
    ggplot(aes(x = X, y = point_height, color = point_height)) +
    geom_point(alpha = 0.5, size = 0.25) +
    scale_color_viridis_c(option = "H") +
    theme_minimal() +
    labs(x = "X Plane", y = "Height") +
    theme(legend.position = "none", 
          panel.grid = element_blank())
  p_b
  
  p_c <- df %>%
    sample_n(100000) %>%
    arrange(point_height) %>%
    ggplot(aes(x = Y, y = point_height, color = point_height)) +
    geom_point(alpha = 0.5, size = 0.25) +
    scale_color_viridis_c(option = "H") +
    theme_minimal() +
    labs(x = "Y Plane", y = "Height") +
    theme(legend.position = "none", 
          panel.grid = element_blank())
  p_c
  
  print(grid.arrange(p_a, p_b, p_c, ncol = 3, top=textGrob(paste0(plot_id), gp = gpar(fontsize = 20))))
  
  p_comb <- grid.arrange(p_a, p_b, p_c, ncol = 3, top=textGrob(paste0(plot_id), gp = gpar(fontsize = 20)))
  filename <- paste0("builds/plots/scans/", plot_id, "_750cm.png")
  ggsave(plot = p_comb, filename = filename, dpi = 600, height = 4.5, width = 12)
  
  
}
