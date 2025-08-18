#!/usr/bin/env Rscript

# read_gff_file.R
# This script:
#   1) Takes in a timestamped folder (e.g., "./data/2025_03_10_T_14_05_06") as the first argument.
#   2) Takes in a GFF file path as the second argument.
#   3) Reads "<timestamp>/data/combined_data.tsv".
#   4) Reads/filters the GFF file for relevant features.
#   5) Joins with the combined data on "gene_name".
#   6) Outputs a BED file to "<timestamp>/data/combined_tables.bed".


# load packages
library(rtracklayer)
library(tidyverse)

# Get arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("Usage: Rscript read_gff_file.R <timestamp> <gff_file> <id>")
}
timestamp <- args[1]
gff_file <- args[2]
id <- args[3]

#args <- "./data/jakob/Btheta/B_theta_annotation_210224.gff"

# check whether gff file ends with .gff or .gff3
if (!grepl(".gff$", gff_file) && !grepl(".gff3$", gff_file)) {
  stop("GFF file must end with .gff or .gff3")
}

# We'll read the combined_data.tsv from the timestamped data folder
combined_data_path <- file.path(timestamp, "data", "combined_data.tsv")

# GFF columns and tags to extract
gff_tags <- unique(c("Name", "locus_tag", "ID", "gene", "product", id))
gff_cols <- c("seqid", "type", "start", "end", "strand")

# Filter for these feature types
gff_filter <- list(type = c("gene", "ncRNA", "sRNA"))

# Load GFF
gff <- readGFF(
  filepath = gff_file,
  tags = gff_tags,
  columns = gff_cols,
  filter = gff_filter
) %>%
  as_tibble()

# Read the combined data
data_combined <- read_tsv(combined_data_path)

# Create a 'gene_name' in GFF, falling back to 'locus_tag' if 'gene' is NA
# gff <- gff %>%
#   mutate(id = ifelse(is.na(id), locus_tag, id))
gff[[id]] <- ifelse(is.na(gff[[id]]), gff$locus_tag, gff[[id]])

# create a id_lower column in gff
gff[["id_lower"]] <- tolower(gff[[id]])


# Inner-join on 'gene_name'
gff_upd <- inner_join(gff, data_combined, by = "id_lower")

# now re-name id column to the original id ("id".x)
gff_upd[[id]] <- gff_upd[[paste0(id, ".x")]]

gff_upd <- gff_upd %>%
  # remove columns ending with .x # and .y
    select(-ends_with(".x"), -ends_with(".y"))

# Prepare BED-like table
print(id)
gff_upd_bed <- gff_upd %>%
  select(seqid, start, end, all_of(id), type, strand) %>%
  mutate(
    # If type is CDS/gene and strand is +, adjust start/end
    start = ifelse(type %in% c("CDS", "gene") & strand == "+", start - 50, start),
    end   = ifelse(type %in% c("CDS", "gene") & strand == "+", start + 100, end),
    # If type is CDS/gene and strand is -, adjust start/end
    start = ifelse(type %in% c("CDS", "gene") & strand == "-", end - 50, start),
    end   = ifelse(type %in% c("CDS", "gene") & strand == "-", start + 100, end),
    # Ensure start is ≥ 1
    start = ifelse(start < 1, 1, start)
  )

#Save tibble to file
write_tsv(gff_upd, paste0(timestamp, "/data/combined_tables.tsv"))

# remove combined_data.tsv
file.remove(combined_data_path)

# Write the updated BED to <timestamp>/data/combined_tables.bed (no column headers)
output_bed_path <- file.path(timestamp, "data", "combined_tables.bed")
write_tsv(gff_upd_bed, output_bed_path, col_names = FALSE)


message("Wrote BED file to: ", output_bed_path)


