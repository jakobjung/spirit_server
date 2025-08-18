#!/usr/bin/env Rscript <timestamp>

# get_pvalues_from_intarna.R
# This script:
#   1) Reads the shuffled IntaRNA output (IntaRNA_shuffled_output.csv)
#   2) Reads the original IntaRNA output (IntaRNA_output.csv)
#   3) Fits a Gumbel (GEV) distribution to the shuffled energies
#   4) Calculates p-values for the original energies
#   5) Merges those p-values into combined_data.tsv
#   6) Writes updated combined_data.tsv
#   7) Saves diagnostic plots to <timestamp>/results

# ---------------------------------------------------
# 0) Load required libraries
# ---------------------------------------------------
library(tidyverse)
library(readr)
library(ggplot2)
library(ggpubr)
library(MASS)
library(evd)      # for fgumbel(), etc.
library(cowplot)  # for plot_grid()


# ---------------------------------------------------
# Parse Arguments
# ---------------------------------------------------
args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript get_pvalues_from_intarna.R <timestamp> <id>")
}
timestamp <- args[1]
# timestamp <- "./SPIRIT/data/2025_03_14_T_15_40_26"
id <- args[2]
# id <- "gene"

# Construct relevant paths
shuffled_path    <- file.path(timestamp, "data", "IntaRNA_shuffled_output.csv")
original_path    <- file.path(timestamp, "data", "IntaRNA_output.csv")
combined_in_path <- file.path(timestamp, "data", "combined_tables.tsv")
combined_out_path <- file.path(timestamp, "data", "combined_tables.tsv")

# We'll store plots in <timestamp>/results
dir.create(file.path(timestamp, "results"), showWarnings=FALSE)

plot_shuffled_vs_original_pdf <- file.path(timestamp, "results", "shuffled_vs_original_gumbel.pdf")
plot_pval_dist_pdf            <- file.path(timestamp, "results", "pval_dist_IntaRNA.pdf")


# ---------------------------------------------------
# 1) Read in your data
# ---------------------------------------------------
# Shuffled data
shuffled_data <- read_delim(shuffled_path, delim = ";") %>%
  mutate(gene_name = str_extract(id1, ".*(?=::)"))

# Original data
original_data <- read_delim(original_path, delim = ";") %>%
  mutate(gene_name = str_extract(id1, ".*(?=::)"))


# ---------------------------------------------------
# 2) Negate E so that large positive values
#    correspond to "extreme" negative energies
# ---------------------------------------------------
shuffled_data <- shuffled_data %>% mutate(negE = -E)
original_data <- original_data %>% mutate(negE = -E)

# ---------------------------------------------------
# 3) Fit a Gumbel distribution to the shuffled negE
# ---------------------------------------------------
fit_gumbel <- fgumbel(shuffled_data$negE)

# 95th percentile threshold
threshold <- qgumbel(0.95,
                     loc   = fit_gumbel$param["loc"],
                     scale = fit_gumbel$param["scale"])


# ---------------------------------------------------
# 4) Diagnostic Plots
# ---------------------------------------------------
plot_shuffled_gumbel_threshold <- ggplot(shuffled_data, aes(negE)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5) +
  stat_function(fun = dgumbel,
                args = list(loc = fit_gumbel$param["loc"],
                            scale = fit_gumbel$param["scale"])) +
  geom_vline(xintercept = threshold, color = "red") +
  annotate("text", x = threshold + 3, y = 0.07,
           label = "95th percentile", color = "red") +
  labs(
    title = "Shuffled MFEs & Fitted Gumbel",
    x = "-MFE",
    y = "Density"
  ) +
  theme_minimal()

plot_original_gumbel <- ggplot(original_data, aes(negE)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.4) +
  stat_function(fun = dgumbel,
                args = list(loc = fit_gumbel$param["loc"],
                            scale = fit_gumbel$param["scale"])) +
  geom_vline(xintercept = threshold, color = "red") +
  annotate("text", x = threshold + 3, y = 0.07,
           label = "95th percentile", color = "red") +
  labs(
    title = "Original MFEs & Fitted Gumbel",
    x = "-MFE",
    y = "Density"
  ) +
  theme_minimal()

plot_both <- plot_grid(plot_shuffled_gumbel_threshold,
                       plot_original_gumbel, ncol = 2)

# Save the combined plot
ggsave(plot_shuffled_vs_original_pdf,
       plot_both, width = 10, height = 5)


# ---------------------------------------------------
# 5) Calculate p-values for original_data
# ---------------------------------------------------
original_data <- original_data %>%
  mutate(p_value = pgumbel(negE,
                           loc   = fit_gumbel$param["loc"],
                           scale = fit_gumbel$param["scale"],
                           lower.tail = FALSE)) %>%
  arrange(p_value) %>%
  distinct(gene_name, .keep_all = TRUE)

# remove negE column
original_data$negE <- NULL


# Rename columns (prefix IntaRNA_ except gene_name)
colnames(original_data) <- paste0("IntaRNA_", colnames(original_data))
colnames(original_data)[colnames(original_data) == "IntaRNA_gene_name"] <- id


# ---------------------------------------------------
# 6) Merge p-values into combined_data & write back
# ---------------------------------------------------
data_combined <- read_tsv(combined_in_path)

data_combined <- full_join(data_combined, original_data, by = id) %>%
  mutate(
    IntaRNA_p_value = ifelse(is.na(IntaRNA_p_value), 1, IntaRNA_p_value)
  )

# add for all NA p-values a 1. p-value colums end with p_value
data_combined <- data_combined %>%
  mutate(across(ends_with("p_value"), ~ ifelse(is.na(.), 1, .)))

write_tsv(data_combined, combined_out_path)

message("Updated combined_data with IntaRNA p-values at: ", combined_out_path)
message("Saved plots to: ", file.path(timestamp, "results"))



