# combine across years 
# --------------------------------
# deduplication 1_dedup_tab.R
# The data were deduplicated to retain only the first qualifying encounter 
# within each 30-day window per surgeon-patient pair. 
# however, certain surgeries may appear under multiple surgeons to accurately reflect collaborative care. 

# This means after deduplication, we still have duplicated patients 

# ----------------------------
# Temperature 
# Table 6: Surgery:

# FirstPostProcedureCoreTemp
# HighestCoreTempDuringProcedure
# HighestTempNearAnesthesiaStop
# AnesthesiaTemp

# AnesthesiaTemp, being coded using the same method as Kyle Bata uses

# ----------------------------


vars_to_keep <- c()

# Remove all objects except those in vars_to_keep
rm(list = setdiff(ls(), vars_to_keep))

library(openxlsx)
library(lubridate)
library(kableExtra)
library(openxlsx)
library(tidyverse)
library(here) 
library(gtsummary)

source("./global_vars.R")

# ---------------------------------
# one time combine deduplicated arb_person_id
fp_base_dir <- paste0("../processed_data/")
years_date_str <- "20250203"

list_Epic_data <- list()

for(year_val in years){
  current_year_folder <- paste0(years_date_str, "_", year_val)
  current_fp_processed <- file.path(fp_base_dir, current_year_folder)
  
  cat("Processing year:", year_val, "\n")
  
  epic_file_path <- file.path(current_fp_processed, "ICD10_uni_surg_id_spec.rds")
  cat("  Looking for ICD10_uni_surg_id_spec file:", epic_file_path, "\n")
  if (file.exists(epic_file_path)) {
    datepic_year <- read_csv(epic_file_path, show_col_types = FALSE)
    list_Epic_data[[as.character(year_val)]] <- datepic_year
    cat("    Loaded ICD10_uni_surg_id_spec data for", year_val, ". Rows:", nrow(datepic_year), "\n")
  } else {
    warning(paste("ICD10_uni_surg_id_spec file not found for year", 
                  year_val, ":", epic_file_path))
  }
}

# --- Combine all years' data ---
if (length(list_Epic_data) > 0) {
  datpost <- dplyr::bind_rows(list_Epic_data)
  cat("Total rows in combined datpost:", nrow(datpost), "\n")
} else {
  warning("No postop data loaded. Creating an empty datpost dataframe.")
  datpost <- data.frame() 
}

# ------------
# read table6 and filter 
table6 <- read_csv(fp_tab6)

# tbl_summary of , by year, using table6 
# FirstPostProcedureCoreTemp
# HighestCoreTempDuringProcedure
# HighestTempNearAnesthesiaStop
# AnesthesiaTemp




