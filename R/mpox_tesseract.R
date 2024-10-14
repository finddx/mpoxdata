library(tesseract)
library(pdftools)
library(stringr)
library(dplyr)
library(lubridate)

# Function to process a single PDF and save extracted text to a file
process_pdf <- function(file_path, output_folder) {
  # Convert PDF pages to images
  pdf_images <- pdf_convert(file_path, pages = 1:5, dpi = 300, )
  
  # Perform OCR on the images and combine extracted text from all pages
  ocr_text <- lapply(pdf_images, tesseract::ocr)
  text <- paste(unlist(ocr_text), collapse = "\n")
  
  # Create a file name for the output text file
  file_name <- str_replace(basename(file_path), "\\.pdf$", ".txt")
  output_file_path <- file.path(output_folder, file_name)
  
  # Write the extracted text to a text file
  writeLines(text, output_file_path)
  
  # Print message to indicate completion
  cat("Processed:", file_name, "\n")
}

# Main function to process all PDFs in a folder
process_all_pdfs <- function(input_folder, output_folder) {
  # Get a list of all PDF files in the input folder
  pdf_files <- list.files(input_folder, pattern = "\\.pdf$", full.names = TRUE)
  
  # Create the output folder if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  # Loop through each PDF and process it
  for (pdf_file in pdf_files) {
    process_pdf(pdf_file, output_folder)
    file.rename(from = pdf_file, to = paste0(input_folder, "/processed/", basename(pdf_file)))
  }
  
  cat("All PDFs processed.\n")
  
}

# Example usage:
input_folder <- "data/pdf"  # Folder containing PDF files
output_folder <- "data/output/text"  # Folder where text files will be saved

# Process all PDFs in the folder
process_all_pdfs(input_folder, output_folder)



# Function to merge all text files in a folder into one file
merge_txt_files <- function(input_folder, output_file) {
  # Get a list of all .txt files in the folder
  txt_files <- list.files(input_folder, pattern = "\\.txt$", full.names = TRUE)
  
  # Initialize an empty character vector to hold all the text
  merged_text <- c()
  
  # Loop through each text file and append its content to the merged_text vector
  for (txt_file in txt_files) {
    file_text <- readLines(txt_file, warn = FALSE)  # Read the text file
    merged_text <- c(merged_text, file_text, "\n")  # Append the text and add a newline
    file.rename(from = txt_file, to = paste0(input_folder, "/processed/", basename(txt_file)))
  }
  
  # Write the merged text into the output file
  writeLines(merged_text, output_file)
  
  
  # Print message to indicate completion
  cat("All text files merged into:", output_file, "\n")
}

# Example usage:
input_folder <- "data/output/text"  # Folder containing individual .txt files
output_file <- paste0("data/output/text/file_ACDC_report_", Sys.Date(), ".txt")  # The path for the final merged file

# Merge all .txt files into one
merge_txt_files(input_folder, output_file)


png_input_folder <- getwd()
png_output_folder <- "data/png/"

png_files <- list.files(png_input_folder, pattern = "\\.png$", full.names = TRUE)

for (png_file in png_files) {
  file.rename(from = png_file, to = paste0(png_output_folder, basename(png_file)))
}



