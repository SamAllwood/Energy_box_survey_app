# Script to convert .HEIC files to .jpg and extract GPS coordinates from EXIF data and join to dataset
# Load libraries
library(magick)
library(exiftoolr)
library(tidyverse)

# Select photo folders - direct this to wherever the Google Takeout folder unzipped to. 
# Photo_folders should have 5 folders, one from each survey run
photo_folders <- list.files(path = "Photos/Takeout/Google Photos", 
                            full.names = TRUE, 
                        )
destination_folder <- "Photos/JPEGs"

# Create the destination folder if it does not exist
if (!dir.exists(destination_folder)) {
  dir.create(destination_folder)
}

# Function to convert .HEIC to .jpg for one folder
convert_folder_heic_to_jpg <- function(source_folder, destination_folder) {
  # List the .HEIC files in the folder
  heic_files <- list.files(source_folder, pattern = "\\.HEIC$", full.names = TRUE)
  folder_code <- substr(basename(source_folder), nchar(basename(source_folder)), nchar(basename(source_folder)))
  # Function to convert a single .HEIC file to .jpg
  convert_heic_to_jpg <- function(heic_file, destination_folder) {
    # Read the .HEIC file
    image <- image_read(heic_file)
    # Get the base name of the file (without extension)
    base_name <- paste0(tools::file_path_sans_ext(basename(heic_file)),"_", folder_code)
    # Define the destination file path
    jpg_file <- file.path(destination_folder, paste0(base_name, ".jpg"))
    # Write the image as .jpg
    image_write(image, path = jpg_file, format = "jpg")
  }
  # Apply the conversion function to all .HEIC files
  lapply(heic_files, convert_heic_to_jpg, destination_folder = destination_folder)
}

# Apply the conversion function to all .HEIC files
lapply(photo_folders, convert_folder_heic_to_jpg, destination_folder = destination_folder)

# Print a message indicating completion
print("Conversion complete. All .HEIC files have been converted to .jpg and saved in the destination folder.\n")


# Resize the jpg files for ease of handling in markdown
photo_paths <- list.files(destination_folder, pattern = "\\.jpg$", full.names = TRUE)

# Function to reduce the size of a JPEG file and overwrite the previous version
reduce_image_size <- function(file_path, scale) {
  # Read the image
  image <- image_read(file_path)
  
  # Resize the image
  resized_image <- image_scale(image, scale)
  
  # Overwrite the previous version
  image_write(resized_image, path = file_path)
}

# Reduce the size of each image to 50% of the original size
lapply(photo_paths, reduce_image_size, scale = "50%")

# Print message indicating completion
print("Photos converted to jpg and resized ready for report. Please run report.md to publish report")
