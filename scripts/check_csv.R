# Script that reads in file from Rscript call, and renames all the columns
# as the filename_ + column name to avoid conflicts with other files.
# it also checks whether the columns "p_value" and <id> are present. If not, it will
# cause an error as output of the script.

# load packages
library(tidyverse)
library(readr)
library(readxl)

# import the csv file name from the bash Rscript call
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("Usage: Rscript check_csv.R <timestamp> <csv_file>")
}

timestamp <- args[1]
csv_file_path <- args[2]
id <- args[3]

# csv_file <- "./data/MAPS_test.csv"

# Extract the base name (without .csv) for renaming, if ending is csv. else, if it is an .xlsx file, extract using xlsx
name_output <- gsub("\\.csv$|\\.xlsx$", "", basename(csv_file_path))

# Read CSV file, if it ends with .csv, if it ends with .xlsx, read it with read_excel. if neither, throw an error
if (grepl("\\.csv$", csv_file_path)) {
  csv_data <- read_csv(csv_file_path)
} else if (grepl("\\.xlsx$", csv_file_path)) {
  csv_data <- read_excel(csv_file_path)
} else {
  stop(paste0("File format not supported. ", csv_file_path, " is not a .csv or .xlsx file."))
}


# Check if 'p_value' and 'gene_name' columns exist
if (!("p_value" %in% colnames(csv_data)) || !(id %in% colnames(csv_data))) {
  stop(paste0("Columns 'p_value' and ", id, " are not present in: ", csv_file_path))
} else {
  message("Columns 'p_value' and ", id, " are present in: ", csv_file_path)
}


# rename all the columns to avoid conflicts with other files, except for gene_name
for (col in colnames(csv_data)) {
  if (col != id) {
    colnames(csv_data)[colnames(csv_data) == col] <- paste0(name_output, "_", col)
  }
}


# Replace dashes and spaces with underscores in column names
colnames(csv_data) <- gsub("-", "_", colnames(csv_data))
colnames(csv_data) <- gsub(" ", "_", colnames(csv_data))

output_path <- paste0(timestamp, "/data/", name_output, ".tsv")

# write the tibble to a file
write_tsv(csv_data, output_path)

message("Wrote file to: ", output_path)



