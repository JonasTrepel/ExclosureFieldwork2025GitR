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


### adapt file_path! 
files_raw <- as.data.table(list.files("/Volumes/Untitled/lidar_exclosures_2025", pattern = ".laz", full.names = TRUE))


files <- files_raw %>% 
  rename(file_path = V1) %>% 
  mutate(file_name = gsub("/Volumes/Untitled/lidar_exclosures_2025/", "", file_path), 
         plot_id = gsub(".laz", "", file_name)) 

unique(files$plot_id)

## prep loop 

res <- data.frame()
#Scan time is 3 min, 40 sec. = 220 sec
#360,000 points/sec
# 
#possible_points <- 220*360000

#however, it says in the manual that a high res scan has about 65 mio points. 
# page 37, https://shop.leica-geosystems.com/sites/default/files/2022-01/853811_Leica_BLK360_UM_v4.0.0_en.pdf

possible_points <- 65000000

#when dividing the mean by the fraction of points, we reach higher values at lower fraction of point returns - 
### consistent with higher means represent more open systems 


#loooooop

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
  
  (nrow(df)/possible_points)
  #hist(df$Z)
  #hist(df$point_height)
  #summary(df$point_height)
  
  
  ## Vizualize 
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
  
  
  if(nrow(df)<1){next}
  
  tmp_full <- df %>% 
    summarise(mean_2d = mean(distance_2d),
              mean_3d = mean(distance_3d), 
              mean_Z = mean(Z), 
              mean_height = mean(point_height), 
              
              adjusted_mean_2d = mean(distance_2d)/(nrow(.)/possible_points),
              adjusted_mean_3d = mean(distance_3d)/(nrow(.)/possible_points), 
              
              median_2d = median(distance_2d),
              median_3d = median(distance_3d), 
              median_Z = median(Z), 
              median_height = median(point_height),
              
              sd_2d = sd(distance_2d),
              sd_3d = sd(distance_3d),
              sd_Z = sd(Z), 
              sd_height = median(point_height),
              
              point_fraction = (nrow(.)/possible_points)
    ) %>% 
    mutate(plot_id = file)
  

  
  res <- rbind(tmp_full, res)
  
  print(paste0(file, " done (", i, "/", nrow(files), ")"  ))  
  
}



reserve_lidar <- res %>% unique()

#fwrite(reserve_lidar, "data/processedData/dataFragments/LidarResultsWaterberg2024Radius10m.csv")