#   Date: 2024-11-20
#   Description: This script integrates the p-values from the different
#                analyses to obtain a single p-value for each gene.
#   Input: The p-value tables from the different analyses.
#   Output: A table with the integrated p-values.


# load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(cowplot)
library(ggrepel)
library(writexl)

# ---------------------------------------------------
# Parse Arguments
# ---------------------------------------------------
args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript p_value_integration.R <timestamp> <id> <weights>")
}
timestamp <- args[1]
# timestamp <- "./spirit_commandline/data/2025_07_09_T_10_28_21/"
id <- args[2]
# id <- "gene"
# get weights if provided
if (length(args) > 2) {
  weights <- args[3]
  # splot weights by comma
    weights <- as.numeric(strsplit(weights, ",")[[1]])
} else {
  weights <- ""
}

# weights <- c(1,1,1)

# Construct relevant paths
combined_in_path <- file.path(timestamp, "data", "combined_tables.tsv")
combined_out_path <- file.path(timestamp, "data", "combined_tables.tsv")
combined_out_path_xlsx <- file.path(timestamp, "data", "combined_tables.xlsx")

# We'll store plots in <timestamp>/results
plot_pval_dist_combined_pdf <- file.path(timestamp, "results", "pval_dist_combined.pdf")
plot_all_vs_fisher_pdf <- file.path(timestamp, "results", "all_vs_fisher.pdf")

# ---------------------------------------------------
# 1) Read in your data
# ---------------------------------------------------


# get the table including all p values:
data_full <- read_tsv(combined_in_path)

data_only_pvals <- data_full %>%
  # select only columns that contain end with "p_value", and gene_name
  select(matches(paste0("_p_value$|", id, "$")))

# get nr. of p-values - nr of columns - 1
nr_pvals <- ncol(data_only_pvals) - 1

# if weights are provided, check if they match the number of p-values
if (length(weights) > 1) {
  if (length(weights) != nr_pvals) {
    stop("Number of weights (sep. by comma) must match the number of p-values.")
  }
} else {
  weights <- rep(1, nr_pvals)
}

# ---------------------------------------------------
# 2) Combine p-values using Fisher's method
# ---------------------------------------------------

print(paste0("Number of p-values to be combined by Fisher's test': ", nr_pvals))


# ok. now use the Fisher's test to combine the p-values"
# create function for p-value integration using Fisher method
combine_fishers_pvalue <- function(p_matrix, weights = NULL) {
  # Check if input is a matrix
  if (!is.matrix(p_matrix)) stop("Input must be a matrix of p-values.")

  # Initialize result vector
  combined_pvalues <- numeric(nrow(p_matrix))

  # Loop through rows of the matrix
  for (i in 1:nrow(p_matrix)) {
    p_values <- p_matrix[i, ]

    # Remove NA values
    p_values <- p_values[!is.na(p_values)]

    # Apply weights if provided
    if (!is.null(weights)) {
      if (length(weights) != length(p_values)) {
        stop("Length of weights must match the number of p-values in each row.")
      }
      weighted_stat <- -2 * sum(weights * log(p_values))
    } else {
      weighted_stat <- -2 * sum(log(p_values))
    }

    # Degrees of freedom
    df <- 2 * length(p_values)

    # Calculate combined p-value
    combined_pvalues[i] <- pchisq(weighted_stat, df, lower.tail = FALSE)
  }

  # Return combined p-values
  return(combined_pvalues)
}


# same with stouffer method
# create function for p-value integration using Stouffer's method
combine_stouffers_pvalue <- function(p_matrix, weights = NULL) {
  # Check if input is a matrix
  if (!is.matrix(p_matrix)) stop("Input must be a matrix of p-values.")

  # Initialize result vector
  combined_pvalues <- numeric(nrow(p_matrix))

  # Loop through rows of the matrix
  for (i in 1:nrow(p_matrix)) {
    p_values <- p_matrix[i, ]

    # Remove NA values
    p_values <- p_values[!is.na(p_values)]

    # Apply weights if provided
    if (!is.null(weights)) {
      if (length(weights) != length(p_values)) {
        stop("Length of weights must match the number of p-values in each row.")
      }
      z_scores <- qnorm(1 - p_values)
      weighted_z <- sum(weights * z_scores) / sqrt(sum(weights^2))
      combined_pvalues[i] <- pnorm(weighted_z, lower.tail = FALSE)
    } else {
      z_scores <- qnorm(1 - p_values)
      combined_z <- mean(z_scores)
      combined_pvalues[i] <- pnorm(combined_z, lower.tail = FALSE)
    }
  }

  # Return combined p-values
  return(combined_pvalues)
}


# Combine p-values without weights
raw_pvalues_merged <- data_only_pvals %>%
  select(-all_of(id)) %>%
  as.matrix()

# find row with NAs in raw_pvalues_merged and make 1 of it in raw_pvalues_merged
raw_pvalues_merged[is.na(raw_pvalues_merged)] <- 1

# Combine p-values using Fisher's method
# weights <- c(1,3,5)
data_full$fisher_p_value <- combine_fishers_pvalue(raw_pvalues_merged, weights)
# do fdr correction
data_full$fisher_p_value_fdr <- p.adjust(data_full$fisher_p_value, method = "fdr")

data_only_pvals$fisher_p_value <- data_full$fisher_p_value

# Combine p-values using Stouffer's method
data_full$stouffer_p_value <- combine_stouffers_pvalue(raw_pvalues_merged, weights)

# do fdr
data_full$stouffer_p_value_fdr <- p.adjust(data_full$stouffer_p_value, method = "fdr")


#  save as csv
write_tsv(data_full, combined_out_path)
# save as excel
write_xlsx(data_full, combined_out_path_xlsx)






# p-value distribution of the different analyses. use ggplot.
# forst, get colnames of p-values and extract them without _p_value
pval_cols <- colnames(data_only_pvals)[grepl("p_value", colnames(data_only_pvals))]
exp_names <- gsub("_p_value", "", pval_cols)

# ok now go through the p-values and make a plot for each. save them in a list
p_val_distr_list <- list()
for (i in 1:length(pval_cols)) {
  p_val_distr_list[[i]] <- ggplot(data_only_pvals, aes(.data[[pval_cols[i]]])) +
    geom_histogram( binwidth = 0.01, fill="steelblue") +
    labs(title = paste("Distribution of \np-values for", exp_names[i]),
         x = "p-value",
         y = "Density") +
    theme_bw()
}


# combine all plots in a grid in cowplot
p_val_distr_combined <- plot_grid(plotlist = p_val_distr_list, ncol = 2)

# save them as pdf
ggsave(plot_pval_dist_combined_pdf, p_val_distr_combined)

# OK, now make plots for each p-value against the fisher p-value. use -log10 for the p values.
# again, use a loop to make the plots and save them in a list

# get pvalues wo fishers "fisher_p_value"
pval_cols_wo_fisher <- pval_cols[!pval_cols %in% "fisher_p_value"]

p_val_vs_fisher_list <- list()
for (i in 1:length(pval_cols_wo_fisher)) {
    p_val_vs_fisher_list[[i]] <- ggplot(data_full, aes(-log10(.data[[pval_cols_wo_fisher[i]]]), -log10(fisher_p_value))) +
        geom_point(alpha=0.2, size=0.5) +
        theme_bw() +
        labs(title = paste(exp_names[i], "p-value vs \nFisher combined p-value"),
             x = "-log10(p-value)",
             y = "-log10(Fisher combined p-value)") +
        theme(legend.position = "none") +
        # add label for top 20 genes of fisher p-value
        geom_text_repel(data = data_full %>% top_n(20, - log10(fisher_p_value)),
                        size=3,
                        # increase size of labe
                        aes(label = .data[[id]],
                            fontface="italic"),
                        box.padding = unit(0.35, "lines"),
                        point.padding = unit(0.5, "lines"),
                        # make the segment transparent
                        segment.alpha = 0.2,
                        # make text steelblue
                        color = "steelblue",
                        max.overlaps = 100,
                        min.segment.length = 0)
    }

# combine all plots in a grid in cowplot
p_val_vs_fisher_combined <- plot_grid(plotlist = p_val_vs_fisher_list, ncol = 2)

# save them as pdf
ggsave(plot_all_vs_fisher_pdf, p_val_vs_fisher_combined)

# done
print("Integration of p-values completed.")

