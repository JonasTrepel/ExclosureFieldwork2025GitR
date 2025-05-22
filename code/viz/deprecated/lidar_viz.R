#viz lidar 

rm(list = ls())

library(data.table)
library(tidyverse)
library(sf)
library(lidR)
library(rlas)
library(gridExtra)
library(grid)

plot_meta <-  fread("data/raw/plot_level_data_2025.csv")
summary(plot_meta)

## load lidar ----------------


### adapt file_path 
files_raw <- as.data.table(list.files("/Volumes/Untitled/lidar_exclosures_2025", pattern = ".laz", full.names = TRUE))


files <- files_raw %>% 
  rename(file_path = V1) %>% 
  mutate(file_name = gsub("/Volumes/Untitled/lidar_exclosures_2025/", "", file_path), 
         plot_id = gsub(".laz", "", file_name)) 

unique(files$plot_id)


for(i in 1:nrow(files)){
  
  file <- files[i,]$plot_id
  path <- files[i,]$file_path
  
  df <- read.las(path) %>%
    # discard irrelevant variables
    select(X:Z) %>% 
    ## remove duplicates
    # unique() %>%
    # calculate horizontal and 3d distance
    mutate(distance_2d = sqrt(X^2 + Y^2),
           distance_3d = sqrt(distance_2d^2 + Z^2), 
           angle_raw = (atan(X/Y)*180/pi),
           angle = case_when(
             X > 0 & Y > 0 ~ angle_raw,
             X > 0 & Y < 0 ~ 90 + abs(angle_raw),
             X < 0 & Y < 0 ~ 180 + abs(angle_raw), 
             X < 0 & Y > 0 ~ 270 + abs(angle_raw),
           )) %>% 
    # filter out observations more than 7.5 m away 
    filter(distance_2d <= 7.5) %>% 
    dplyr::select(-angle_raw) %>% 
    mutate(plot_id = paste0(file)) %>% 
    left_join(plot_meta %>% dplyr::select(plot_id, lidar_scanner_height)) %>% 
    mutate(point_height = Z + (lidar_scanner_height/100))
  
  
  # Vizualize
  p_a <- df %>%
    sample_n(100000) %>%
    arrange(point_height) %>%
    ggplot(aes(x = X, y = Y, color = point_height)) +
    geom_point(alpha = 0.5) +
    scale_color_viridis_c() +
    theme_minimal() +
    theme(legend.position = "none")
  p_a
  
  p_b <- df %>%
    sample_n(100000) %>%
    arrange(point_height) %>%
    ggplot(aes(x = X, y = point_height, color = point_height)) +
    geom_point(alpha = 0.5) +
    scale_color_viridis_c() +
    theme_minimal() +
    labs(x = "X", y = "Height") +
    theme(legend.position = "none")
  p_b
  
  p_c <- df %>%
    sample_n(100000) %>%
    arrange(point_height) %>%
    ggplot(aes(x = Y, y = point_height, color = point_height)) +
    geom_point(alpha = 0.5) +
    scale_color_viridis_c() +
    theme_minimal() +
    labs(x = "Y", y = "Height") +
    theme(legend.position = "none")
  p_c
  
  print(grid.arrange(p_a, p_b, p_c, ncol = 3, top=textGrob(paste0(file), gp = gpar(fontsize = 20))))
  
  
}
