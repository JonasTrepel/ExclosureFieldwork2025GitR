### Process lidar files 

# Define paths
blk_dir <- "O:\Nat_Ecoinformatics\C_Write\_User\JonasTrepel_au713983\data_and_resources\lidar\exclosures_2025\blk"
laz_dir <- "O:\Nat_Ecoinformatics\C_Write\_User\JonasTrepel_au713983\data_and_resources\lidar\exclosures_2025\laz"
e57_dir  <- "O:\Nat_Ecoinformatics\C_Write\_User\JonasTrepel_au713983\data_and_resources\lidar\exclosures_2025\e57"
staging_dir <- "C:/Users/au713983/lidar_staging"
cyclone_path <- "C:/Program Files/Leica Geosystems/Cyclone REGISTER 360 2025.0.0/register360.exe"

# Make sure necessary directories exist
dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(e57_dir, recursive = TRUE, showWarnings = FALSE)

# List .blk files in source directory
blk_files <- list.files(blk_dir, pattern = "\\.blk$", full.names = TRUE)

blk_file <- blk_files[1]
# Loop through each .blk file
for (blk_file in blk_files) {
 
  # Extract base name
  blk_name <- tools::file_path_sans_ext(basename(blk_file))
  
  # Set up staging subfolder
  staging_subfolder <- file.path(staging_dir, blk_name)
  dir.create(staging_subfolder, showWarnings = FALSE)
  
  # Copy .blk file into staging subfolder
  staged_blk <- file.path(staging_subfolder, basename(blk_file))
  file.copy(from = blk_file, to = staged_blk, overwrite = TRUE)
  
  # Build and run Cyclone CLI command
  cyclone_cmd <- paste(shQuote(cyclone_path), "-regexp2e57", shQuote(staging_subfolder), "deleteproject=Y")
  cat("Converting:", blk_name, "\n")
  system(cyclone_cmd, wait = TRUE)
  
  # Wait for .e57 output to appear
  e57_file <- file.path(staging_subfolder, paste0(blk_name, ".e57"))
  
  if (file.exists(e57_file)) {
    # Move .e57 file to final destination
    
    final_dest <- file.path(e57_dir, basename(e57_file))
    
    file.rename(from = e57_file, to = final_dest)
    
    cat("Converted and moved:", final_dest, "\n")
  } else {
    cat("WARNING: No .e57 file found for:", blk_file, "\n")
  }
  
  #Clean up the staging subfolder
  unlink(staging_subfolder, recursive = TRUE)
}

cat("All files processed.\n")