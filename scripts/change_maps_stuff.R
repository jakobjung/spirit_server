# read in maps csv and just change the p-values with negative log2fc to 1.
#
# ---------------------------------------------------
# 0) Load required libraries
# ---------------------------------------------------
library(tidyverse)
library(readr)

# ---------------------------------------------------
# read csv
# ---------------------------------------------------7
csv_maps <- read.csv("./spirit_commandline/data/default/MAPS_test.csv")

# ok now change the p-values with negative log2fc to 1
tail(csv_maps)
#divide p values by 2
csv_maps$p_value <- csv_maps$p_value / 2
csv_maps$p_value <- ifelse(csv_maps$ms2.pint_spi.1_vs_pint_spi.1_log2foldchange < 0, 1, csv_maps$p_value)
head(csv_maps)

# save as csv
write.csv(csv_maps, "./spirit_commandline/data/default/MAPS_test_1.csv", row.names = FALSE)



