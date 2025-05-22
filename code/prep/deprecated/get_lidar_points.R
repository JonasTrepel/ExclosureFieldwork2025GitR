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

i = 1
points <- data.table()
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
    left_join(plot_meta %>% dplyr::select(plot_id, site_id, lidar_scanner_height)) %>% 
    mutate(point_height = Z + (lidar_scanner_height/100))
  
  set.seed(161)
  tmp_points <- df %>%
    dplyr::select(distance_2d, distance_3d, point_height, plot_id, site_id) %>%
    sample_n(100000)
  
  points <- rbind(tmp_points, points)
  
  print(paste0(file, " done (", i, "/", nrow(files), ")"))
  
}

fwrite(points, "data/processed/fragments/lidar_points_100000_per_plot.csv")
setDT(points)

dt_all <- fread("data/processed/clean/all_vars.csv")
mean_in <- mean(dt_all[in_or_out == "inside", ]$mean_point_height_plot, na.rm = T)
sd_in <- sd(dt_all[in_or_out == "inside", ]$mean_point_height_plot, na.rm = T)
mean_out <- mean(dt_all[in_or_out == "outside", ]$mean_point_height_plot, na.rm = T)
sd_out <- sd(dt_all[in_or_out == "outside", ]$mean_point_height_plot, na.rm = T)


ins <- points %>%
  mutate(in_or_out = ifelse(grepl("in", plot_id), "inside", "outside")) %>%
  filter(in_or_out == "inside") %>% 
  ggplot(aes(x = point_height, y = after_stat(count / sum(count)))) +
  geom_histogram(binwidth = 0.25, fill = "seashell4") +
  geom_vline(xintercept = mean_in,
             linetype = "dashed", color = "orange2", linewidth = 1.01) +
  labs(x = "Vegetation Height", y = "Proportion") +
  annotate("text", 
           x = mean_in+0.5, y = 0.28,  
           label = paste0("Average Height = ", round(mean_in, 2), " ± ", round(sd_in, 2)), 
           hjust = 1, size = 2.5, fontface = "italic", color = "black") +
  xlim(0, 12) +
  ylim(0, 0.3) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
                         axis.ticks = element_line())
ins
ggsave(plot = ins, "builds/plots/inkscape/fig_3_components/height_inside.png", dpi = 600, height = 3, width = 3)


out <- points %>%
  mutate(in_or_out = ifelse(grepl("in", plot_id), "inside", "outside")) %>%
  filter(in_or_out == "outside") %>% 
  ggplot(aes(x = point_height, y = after_stat(count / sum(count)), fill = point_height)) +
  geom_histogram(binwidth = 0.25, fill = "seashell4") +
  geom_vline(xintercept = mean(dt_all[in_or_out == "outside", ]$mean_point_height_plot, na.rm = T),
             linetype = "dashed", color = "orange2", linewidth = 1.01) +
  labs(x = "Vegetation Height", y = "Proportion") +
  annotate("text", 
           x = mean_out+0.5, y = 0.28,  
           label = paste0("Average Height = ", round(mean_out, 2), " ± ", round(sd_out, 2)), 
           hjust = 1, size = 2.5, fontface = "italic", color = "black") +
  xlim(0, 12) +
  ylim(0, 0.3) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.ticks = element_line())
out

ggsave(plot = out, "builds/plots/inkscape/fig_3_components/height_outside.png", dpi = 600, height = 3, width = 3)


points %>%
  mutate(in_or_out = ifelse(grepl("in", plot_id), "inside", "outside")) %>%
  ggplot(aes(x = distance_3d, y = after_stat(count / sum(count)))) +
  geom_histogram(binwidth = 0.5, fill = "steelblue") +
  xlim(0, 7.5) +
  facet_wrap(~ in_or_out) +
  coord_flip() +
  theme_minimal()
