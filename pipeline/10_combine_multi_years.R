# TODO: combine data from 2021 to 2024, all complications 
# TODO: may need to merge MRN  

vars_to_keep <- c()

# Remove all objects except those in vars_to_keep
rm(list = setdiff(ls(), vars_to_keep))

library(openxlsx)
library(lubridate)
library(kableExtra)
library(openxlsx)
library(tidyverse)
library(here) 

source("./global_vars.R")

# -----------------------------------------

# # Function to create a directory if it doesn't exist
# create_dir_if_not_exists <- function(dir_path) {
#   if (!dir.exists(dir_path)) {
#     dir.create(dir_path, recursive = TRUE)
#     cat(sprintf("Created directory: %s\n", dir_path))
#   } else {
#     cat(sprintf("Directory already exists: %s\n", dir_path))
#   }
# }
# 
# # save multiple years data 
pipeline_dir       <- here::here()
parent_dir         <- dirname(pipeline_dir)
processed_data_dir_comb <- file.path(parent_dir, "processed_data", date_str)
# 
# create_dir_if_not_exists(processed_data_dir_comb)

# -----------------------------------------

fp_base_dir <- paste0("../processed_data/")
years_date_str <- "20250203"
# --- Initialize lists to store data from each year ---
list_datpost <- list()
list_datpre <- list()
sizes_year <- numeric() # Using a named numeric vector


# --- Loop through years to read and collect data ---
for (year_val in years) {
  current_year_folder <- paste0(years_date_str, "_", year_val)
  current_fp_processed <- file.path(fp_base_dir, current_year_folder)
  
  cat("Processing year:", year_val, "\n")
  
  postop_file_path <- file.path(current_fp_processed, "postop_observed_rate.csv")
  preop_file_path <- file.path(current_fp_processed, "preop_expected_rate.csv")
  
  cat("  Looking for postop file:", postop_file_path, "\n")
  if (file.exists(postop_file_path)) {
    datpost_year <- read_csv(postop_file_path, show_col_types = FALSE)
    list_datpost[[as.character(year_val)]] <- datpost_year
    cat("    Loaded postop data for", year_val, ". Rows:", nrow(datpost_year), "\n")
  } else {
    warning(paste("Postop file not found for year", year_val, ":", postop_file_path))
  }
  
  cat("  Looking for preop file:", preop_file_path, "\n")
  if (file.exists(preop_file_path)) {
    datpre_year <- read_csv(preop_file_path, show_col_types = FALSE)
    list_datpre[[as.character(year_val)]] <- datpre_year
    cat("    Loaded preop data for", year_val, ". Rows:", nrow(datpre_year), "\n")
  } else {
    warning(paste("Preop file not found for year", year_val, ":", preop_file_path))
  }
  
  if (nrow(datpost_year) != nrow(datpre_year)) {
    # Construct a detailed error message
    error_message <- paste0(
      "FATAL ERROR: Row count mismatch for year ", year_val, ".\n",
      "  Pre-op data file ('", preop_file_path, "') has ", nrow(datpre_year), " rows.\n",
      "  Post-op data file ('", postop_file_path, "') has ", nrow(datpost_year), " rows.\n",
      "  The number of rows must be identical for pre-op and post-op data for each year. Script halted."
    )
    stop(error_message)
  } else {
    cat("    Row counts match for year", year_val, ":", nrow(datpre_year), "rows.\n")
    # Store the sample size (using nrow from datpre_year as per your request)
    sizes_year[as.character(year_val)] <- nrow(datpre_year)
  }
}

# --- Combine all years' data ---
if (length(list_datpost) > 0) {
  datpost <- dplyr::bind_rows(list_datpost)
  cat("Total rows in combined datpost:", nrow(datpost), "\n")
} else {
  warning("No postop data loaded. Creating an empty datpost dataframe.")
  datpost <- data.frame() 
}

if (length(list_datpre) > 0) {
  datpre <- dplyr::bind_rows(list_datpre)
  cat("Total rows in combined datpre:", nrow(datpre), "\n")
} else {
  warning("No preop data loaded. Creating an empty datpre dataframe.")
  datpre <- data.frame()
}


# --- Merge to get data ---
if (nrow(datpre) > 0 || nrow(datpost) > 0) {
  # Ensure merge key columns exist if dataframes are not empty
  required_merge_cols <- c("SurgeryID", "arb_person_id")
  
  pre_cols_ok <- all(required_merge_cols %in% names(datpre))
  post_cols_ok <- all(required_merge_cols %in% names(datpost))
  # TODO: check, this merge step increases total nrow
  if(pre_cols_ok && post_cols_ok){
    dat5 <- merge(datpre, datpost,
                  by = required_merge_cols, all = TRUE)
    cat("Merge complete. Dimensions of dat5:", paste(dim(dat5), collapse="x"), "\n")
  } else {
    warning("Merge skipped: Required merge columns ('SurgeryID', 'arb_person_id') missing in datpre or datpost.")
  }
  
} else {
  warning("Both preop and postop data are empty. Creating an empty dat5 dataframe.")
}


# --- Define complication columns ---
complication_cols <- list(
  "SSI" = c("pred_prob_SSI_postop", "pred_prob_SSI_preop"),
  "Sepsis" = c("pred_prob_SYSEP_postop", "pred_prob_SYSEP_preop"),
  "Pneumonia" = c("pred_prob_PNEU_postop", "pred_prob_PNEU_preop"),
  "UTI" = c("pred_prob_UTI_postop", "pred_prob_UTI_preop"),
  "cardiac" = c("pred_prob_cardiac_postop", "pred_prob_cardiac_preop"),
  "morb" = c("pred_prob_morb_postop", "pred_prob_morb_preop"),
  "nothome" = c("pred_prob_nothome_postop", "pred_prob_nothome_preop"),
  "pulmonary" = c("pred_prob_pulmonary_postop", "pred_prob_pulmonary_preop"),
  "renal" = c("pred_prob_renal_postop", "pred_prob_renal_preop"),
  "upradmin" = c("pred_prob_upradmin_postop", "pred_prob_upradmin_preop"),
  "VTE" = c("pred_prob_VTE_postop", "pred_prob_VTE_preop"),
  "bleed" = c("pred_prob_bleed_postop", "pred_prob_bleed_preop")
)

pred_prob_cols_postop <- c(
  "pred_prob_SSI_postop", "pred_prob_UTI_postop", "pred_prob_SYSEP_postop", "pred_prob_PNEU_postop",
  "pred_prob_cardiac_postop", "pred_prob_morb_postop", "pred_prob_nothome_postop", "pred_prob_pulmonary_postop",
  "pred_prob_renal_postop", "pred_prob_upradmin_postop", "pred_prob_VTE_postop", "pred_prob_bleed_postop"
)

pred_prob_cols_preop <- gsub("_postop$", "_preop", pred_prob_cols_postop)
pred_prob_cols <- c(pred_prob_cols_postop, pred_prob_cols_preop)

# --- Date manipulation ---
if (nrow(dat5) > 0) {
  if ("SurgeryDate_asDate" %in% colnames(dat5)) {
    # Attempt to parse SurgeryDate, handling potential mixed formats or NAs
    # First, try standard ISO format, then common US format if NAs remain
    dat5$DOS <- as.Date(dat5$SurgeryDate_asDate) # Primary attempt
    
    
    dat5$year <- year(dat5$DOS) 
    dat5$quar <- quarter(dat5$DOS)
    dat5$quar <- factor(dat5$quar, levels = c(1, 2, 3, 4))
    cat("Date columns (DOS, year_col, quar) created.\n")
  } 
} else {

}

# table(dat5$year)
# -----------------------------------------------------------------
# quick summary 
# ----------------------------------------------------------------
# Calculate mean predicted probability for each column 
yearly_prevalence_df <- dat5 %>%
  group_by(year) %>% 
  dplyr::summarise(
    # Calculate mean for all prediction probability columns
    dplyr::across(all_of(pred_prob_cols), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop" # Drop grouping for subsequent operations
  ) %>%
  # Pivot longer to process column names and extract complication/timing
  pivot_longer(
    cols = all_of(pred_prob_cols),
    names_to = "column",
    values_to = "prevalence"
  ) %>%
  mutate(
    timing = if_else(str_detect(column, "_preop$"), "preop", "postop"),
    complication = str_remove(str_remove(column, "^pred_prob_"), "_preop$|_postop$")
  ) %>%
  # Select relevant columns (including the dynamic year column) and pivot wider for pre-op/post-op comparison
  dplyr::select(year, complication, timing, prevalence) %>%
  pivot_wider(names_from = timing, values_from = prevalence) %>%
  arrange(year) %>% dplyr::select(year, complication, preop, postop)


yearly_prevalence_table <- yearly_prevalence_df %>%
  kable(
    col.names = c("Year", "Complication", "Pre-op Mean Prob.", "Post-op Mean Prob."), 
    digits = 3,
    caption = "Yearly Mean Predicted Complication Probability (Pre-op vs. Post-op)",
    booktabs = TRUE 
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
  # Collapse rows for the 'Year' column (which is the first column) to group complications by year visually
  collapse_rows(columns = 1, valign = "top")

print(yearly_prevalence_table)

sizes_year_comb <-c(Complication = "Sample Size", sizes_year) 

#  --------------------------------------------------------------

# 1. Select only necessary columns (year, complication, postop)
postop_data_for_table <- yearly_prevalence_df %>%
  dplyr::select(year, complication, postop)

# 2. Pivot wider: complications as rows, years as columns, postop values in cells
#    Using names_sort = TRUE to ensure year columns are in chronological order.
postop_wide_table_df <- postop_data_for_table %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = postop,
    names_sort = TRUE # Ensures columns like 2021, 2022, etc., are sorted
  ) 

kable_col_names <- c("Complication", colnames(postop_wide_table_df)[-1])

postop_wide_table_df <- rbind(sizes_year_comb, postop_wide_table_df) %>%
  dplyr::mutate(
    dplyr::across(all_of(years), as.numeric) # Apply as.numeric to each column in 'years'
  )

postop_kable_table <- postop_wide_table_df %>%
  kableExtra::kbl(
    col.names = kable_col_names,
    digits = 3, # Adjust digits as needed
    caption = "Post-operative Complication Prevalence by Year",
    booktabs = TRUE,
    align = c("l", rep("c", ncol(postop_wide_table_df) - 1)) # Align first column left, others center
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE
  ) %>%
  kableExtra::add_header_above(c(" " = 1, "Year" = (ncol(postop_wide_table_df) - 1))) # Group year columns under "Year"

print(postop_kable_table)

# --------------------------

preop_data_for_table <- yearly_prevalence_df %>%
  dplyr::select(year, complication, preop)

# 2. Pivot wider: complications as rows, years as columns, preop values in cells
#    Using names_sort = TRUE to ensure year columns are in chronological order.
preop_wide_table_df <- preop_data_for_table %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = preop,
    names_sort = TRUE # Ensures columns like 2021, 2022, etc., are sorted
  ) 

kable_col_names <- c("Complication", colnames(preop_wide_table_df)[-1])

preop_wide_table_df <- rbind(sizes_year_comb, preop_wide_table_df) %>%
  dplyr::mutate(
    dplyr::across(all_of(years), as.numeric) # Apply as.numeric to each column in 'years'
  )

preop_kable_table <- preop_wide_table_df %>%
  kableExtra::kbl(
    col.names = kable_col_names,
    digits = 3, # Adjust digits as needed
    caption = "Pre-operative Complication Prevalence by Year",
    booktabs = TRUE,
    align = c("l", rep("c", ncol(preop_wide_table_df) - 1)) # Align first column left, others center
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE
  ) %>%
  kableExtra::add_header_above(c(" " = 1, "Year" = (ncol(preop_wide_table_df) - 1))) # Group year columns under "Year"

print(preop_kable_table)

# ------------------------------------------ #
# save 

full_path_dat5 <- file.path(processed_data_dir_comb[1], 
          paste0("Aspin_all_complications_pred_prob_",years[1], years[length(years)], ".csv"))
write.csv(dat5, file = full_path_dat5, row.names = FALSE)

full_path_postop_wide <- file.path(processed_data_dir_comb[1], 
                            paste0("Aspin_all_complications_postop_yearly_",years[1], years[length(years)], ".csv"))
write.csv(postop_wide_table_df, file = full_path_postop_wide, row.names = FALSE)

full_path_preop_wide <- file.path(processed_data_dir_comb[1], 
                            paste0("Aspin_all_complications_preop_yearly_",years[1], years[length(years)], ".csv"))
write.csv(preop_wide_table_df, file = full_path_preop_wide, row.names = FALSE)

