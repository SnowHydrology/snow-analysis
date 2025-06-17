# Script for crosswalking the HARBOR and SNOTEL metadata datasets

# Keith Jennings

# Load packages
library(tidyverse)
library(snotelr)

################################################################################
# User input
# Subset parameters (these can change for different subsets)
start_year = 1985
end_year = 2024
ref_gages = "Y"
subset_name = "40y_gagesIIref"
################################################################################


# SNOTEL metadata from snotelr
snotel_info <- snotel_info()

# Manipulate string for the HUC code
snotel_info <- snotel_info %>% 
  mutate(HUC = stringr::str_extract(string = description,
                                    pattern = "(?<=\\().*(?=\\))"))

# HARBOR metadata
harbor_url <- "https://raw.githubusercontent.com/peckhams/nextgen_basin_repo/refs/heads/main/__Collated/collated_basins_all.tsv"
basin_info <- read_tsv(harbor_url)

# Join the two data sources by the HUC column
all_info <- left_join(snotel_info,
                      basin_info,
                      by = "HUC")

# Export the full metadata
write.csv(x = all_info,
          file = "data/snotel_harbor_metadata_crosswalk.csv",
          row.names = F, quote = F)

# The above produces a "many to one" warning
# There are some rows in `snotel_info` that have multiple matches in `basin_info` and vice versa. 
# This occurs when there are multiple SNOTEL stations in a given HUC or 
# when a SNOTEL station finds itself in multiple nested basins

# Subset to our given parameters
subset_info <- all_info %>% 
  filter(year(start) <= start_year & year(end) >= end_year) %>% 
  filter(Is_GAGES2_Ref == ref_gages)

# Filter to just the higher elevation basins 
subset_info <- subset_info %>% 
  group_by(site_id) %>% 
  slice_max(order_by = Elev, with_ties = FALSE) %>% 
  ungroup()

# Export the subset metadata
write.csv(x = all_info,
          file = paste0("data/snotel_harbor_metadata_crosswalk_", subset_name, ".csv"),
          row.names = F, quote = F)