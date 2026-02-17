#!/usr/bin/env Rscript

# combine_tables.R
# Script that takes in:
#   [1] a "timestamp" folder from the command line
#   [2..n] up to four file names (CSV, XLSX, etc.)
# This script:
#   - Converts each file name to .tsv (the output of check_csv.R).
#   - Reads them from ".data/<timestamp>/data/<file>.tsv".
#   - Joins the data by '<id>'.
#   - Writes the result to ".data/<timestamp>/data/combined_data.tsv".

# load packages
library(tidyverse)
library(readr)

# import the csv file names from the bash Rscript call
args <- commandArgs(trailingOnly = TRUE)


# We expect at least one argument for the timestamp
if (length(args) < 3) {
  stop("Usage: Rscript combine_tables.R <timestamp> <id> [file1 file2 file3 file4]")
}

# args <- c("./spirit_commandline/data/2025_07_09_T_16_44_22", "gene", "./spirit_commandline/data/default/spi1_pulse_new.csv",
#           "./spirit_commandline/data/default/spi1_maps_new.csv")

# First argument is the timestamp folder
timestamp <- args[1]

id <- args[2]

# The remaining arguments are the potential file names (CSV, XLSX, etc.)
input_files <- args[-c(1, 2)]

# Remove empty placeholders if any
input_files <- input_files[nzchar(input_files)]

# Convert each file name to a .tsv path located in <timestamp>/data/
# 1) Remove .csv / .xlsx suffix
# 2) Append .tsv
# 3) Prefix with timestamp/data path
input_files_tsv <- file.path(
  timestamp, "data",
  paste0(gsub("\\.csv$|\\.xlsx$", "", basename(input_files)), ".tsv")
)

# For debugging / clarity
message("Reading these TSV files:")
print(input_files_tsv)

# Read each file
data_list <- lapply(input_files_tsv, read_tsv)

# If more than one file is provided, combine them via inner_join on id
if (length(data_list) > 1) {
  data_combined <- data_list[[1]]
  # show redundant ids in the first file and print them if they exist
    if (id %in% names(data_combined)) {
        dupl_ids <- unique(data_combined[["id"]][duplicated(data_combined[["id"]])])
        if (length(dupl_ids) > 0) {
        message("The following IDs are duplicates in the first file, and will be combined (keeps first occurence): ",
                paste(dupl_ids, collapse = ", "))

        }
      # remove duplicates in the first file. just keep the first occurrence
        data_combined <- data_combined[!duplicated(data_combined[[id]]), ]
      # also remove nas in the id column, if it exists
        data_combined <- data_combined %>% filter(!is.na(.[[id]]))
      # make a new column called id_lower with lower case ids
        data_combined[["id_lower"]] <- tolower(data_combined[[id]])
    }
  for (i in 2:length(data_list)) {
    # show redundant ids in the current file and print them if they exist
    if (id %in% names(data_list[[i]])) {
      dupl_ids <- unique(data_list[[i]][[id]][duplicated(data_list[[i]][[id]])])
      if (length(dupl_ids) > 0) {
        message("The following IDs are duplicates in file ", i, " and will be combined (keeps first occurence): ",
                paste(dupl_ids, collapse = ", "))

      }
      # remove duplicates in the current file. just keep the first occurrence
      data_list[[i]] <- data_list[[i]][!duplicated(data_list[[i]][[id]]), ]
      # make a new column called id_lower with lower case ids
      data_list[[i]][["id_lower"]] <- tolower(data_list[[i]][[id]])
      }
      data_combined <- full_join(data_combined, data_list[[i]], by = "id_lower")
      # also remove nas in the id column
      data_combined <- data_combined %>% filter(!is.na(.[["id_lower"]]))
    # keep only "id".x
    data_combined[[id]] <- data_combined[[paste0(id, ".x")]]

          # remove the ".x" and ".y" columns
        data_combined <- data_combined %>%
            select(-ends_with(".x"), -ends_with(".y"))
  }
} else {
  # If only one file, just store it as-is
  data_combined <- data_list[[1]]
    dupl_ids <- unique(data_combined[[id]][duplicated(data_combined[[id]])])
  if (length(dupl_ids) > 0) {
  message("The following IDs are duplicates in the first file, and will be combined (keeps first occurence): ",
          paste(dupl_ids, collapse = ", "))

  }
# remove duplicates in the first file. just keep the first occurrence
  data_combined <- data_combined[!duplicated(data_combined[[id]]), ]
# also remove nas in the id column
  data_combined <- data_combined[!is.na(data_combined[[id]]), ]
# make a new column called id_lower with lower case ids
  data_combined[["id_lower"]] <- tolower(data_combined[[id]])

}

# Check for duplicates in id
dupl_genes <- unique(data_combined[[id]][duplicated(data_combined[[id]])])
if (length(dupl_genes) > 0) {
  message("The following genes are duplicates and will be excluded from the analysis: ",
          paste(dupl_genes, collapse = ", "))
} else {
  message("No duplicates found.")
}

# Remove duplicates
data_combined <- data_combined[!duplicated(data_combined[id]), ]

# Write the combined data to "<timestamp>/data/combined_data.tsv"
output_path <- file.path(timestamp, "data", "combined_data.tsv")
write_tsv(data_combined, output_path)

message("Wrote combined tables to: ", output_path)



