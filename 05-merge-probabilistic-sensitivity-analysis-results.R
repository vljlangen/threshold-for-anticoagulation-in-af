
################################################################################
################################################################################
###                                                                          ###
###          MERGE RESULTS FROM PROBABILISTIC SENSITIVITY ANALYSIS           ###
###                                                                          ###
###            THIS SCRIPT COMBINES MULTIPLE `.RDS` RESULT FILES             ###
###  GENERATED BY 1000 PSA SIMULATION RUNS INTO ONE DATA FRAME (`RES_ALL`).  ###
###                                                                          ###
###    THE FOLDER PATH IS ASSUMED TO CONTAIN ONLY THE RELEVANT .RDS FILES.   ###
###                                                                          ###
################################################################################
################################################################################

library(dplyr)

# Set the folder path containing the .rds outputs
folder_path <- "psa_results/"  # Replace with the path to the folder containing your .rds files

# List all .rds files in the folder
rds_files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)

# Read and combine all .rds files
res_all <- rds_files %>%
  lapply(readRDS) %>%
  bind_rows()

# Basic checks
print(dim(res_all))   # e.g., number of rows and columns
head(res_all)         # preview first rows

# Save the combined data frame (uncomment to save)
# saveRDS(res_all, "res_all_merged.rds")

