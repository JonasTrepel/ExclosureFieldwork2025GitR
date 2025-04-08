
#1. Define Paths ----------------------------------------

# Folders
blk_dir <- "O:/Nat_Ecoinformatics/C_Write/_User/JonasTrepel_au713983/data_and_resources/lidar/exclosures_2025/blk"
laz_dir <- "O:/Nat_Ecoinformatics/C_Write/_User/JonasTrepel_au713983/data_and_resources/lidar/exclosures_2025/laz"
e57_dir <- "O:/Nat_Ecoinformatics/C_Write/_User/JonasTrepel_au713983/data_and_resources/lidar/exclosures_2025/e57"
staging_dir <- "C:/Users/au713983/lidar_staging"

# Tools
cyclone_path <- "C:/Program Files/Leica Geosystems/Cyclone REGISTER 360 2025.0.0/register360.exe"
#e572las_path <- "O:/Nat_Ecoinformatics/C_Write/_User/JonasTrepel_au713983/data_and_resources/lidar/exclosures_2025/e57/e572las64.exe" 
e572las_path <- "O:/Nat_Ecoinformatics/C_Write/_User/JonasTrepel_au713983/data_and_resources/lidar/lastools/e572las64.exe" 


#2. Convert From .blk To .e57 ----------------------------------------

# List .blk files in source directory
blk_files <- list.files(blk_dir, pattern = "\\.blk$", full.names = TRUE)

#blk_files <- blk_files[196:215]

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
  
  # Run Cyclone conversion
  cyclone_cmd <- paste(shQuote(cyclone_path), "-regexp2e57", shQuote(staging_subfolder), "deleteproject=Y")
  cat("Start converting .blk to .e57:", blk_name, "\n")
  system(cyclone_cmd, wait = TRUE, show.output.on.console = FALSE)
  
  # Look for .e57 result
  e57_file <- file.path(staging_subfolder, paste0(blk_name, ".e57"))
  
  if (file.exists(e57_file)) {
    cat("Successfully converted .blk to .e57:", blk_name, "\n")
  }else{
    cat("Failed converting .blk to .e57:", blk_name, "Skipping file")
    next
    }
    
  # Move .e57 to output directory
  final_e57 <- file.path(e57_dir, basename(e57_file))
  file.rename(from = e57_file, to = final_e57)
  cat("Moved:", final_e57, "\n")
    
  # Clean up
  unlink(staging_subfolder, recursive = TRUE)
}


#3. Convert From .e57 To .laz ----------------------------------------

e57_files <- list.files(e57_dir, pattern = "\\.e57$", full.names = TRUE)

for (e57_file in e57_files){
  
  # Extract base name
  e57_name <- tools::file_path_sans_ext(basename(e57_file))

  # Convert .e57 to .laz
  laz_file <- file.path(laz_dir, paste0(e57_name, ".laz"))
  laz_cmd <- paste(
    shQuote(e572las_path),
    "-v",
    "-i", shQuote(e57_file),
    "-o", shQuote(laz_file)
  )

  cat("Start converting .e57 to .laz:", laz_file, "\n")

  system(laz_cmd, wait = TRUE)

  # Check if .laz was created
  if (file.exists(laz_file)) {
    cat("Laz file saved to:", laz_file, "\n")
  } else {
    cat("Laz conversion failed for:", e57_name, "\n")
  }
}

cat("All files processed.\n")
