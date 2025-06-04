# 0_run_pipeline.R

# ====================================================================
# Pipeline Execution Script
# 
# This script orchestrates the execution of the entire data processing
# and report generation pipeline. It performs the following steps:
# 
# TODO: may need 00_mount_data first

# sudo mount -t cifs -o credentials=~/credentials/smbcredentials //data.ucdenver.pvt/dept/SOM/ACCORDS/Data/ACCORDSGPUHDCDATA ~/AccordsAspin_HDC_data/
# 3 user input parameters: year_w, date_str, save_date_str
# 
# Ensure that all required scripts and files are present in the 
# pipeline directory before executing this script.
# DATE=20250305
# 
# sudo mkdir -p ~/AspinReport/data/"$DATE" /shared/Aspin/"$DATE"
# ====================================================================


# Load necessary libraries
rm(list = ls())

suppressPackageStartupMessages({
  library(here)        # For handling file paths
  library(dplyr)       # For data manipulation
  # library(stringr)     # For string operations
})

source_if_exists <- function(script_path, is_critical = TRUE) {
  if (file.exists(script_path)) {
    cat(sprintf("Running: %s\n", basename(script_path)))
    source(script_path)
    cat(sprintf("Completed: %s\n\n", basename(script_path)))
  } else {
    msg <- sprintf("Script %s not found.", basename(script_path))
    if (is_critical) {
      cat(sprintf("ERROR: %s\n", msg))
      stop(sprintf("Pipeline terminated due to missing script: %s", basename(script_path)))
    } else {
      cat(sprintf("WARNING: %s Skipping...\n\n", msg))
    }
  }
}


# Function to create a directory if it doesn't exist
create_dir_if_not_exists <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat(sprintf("Created directory: %s\n", dir_path))
  } else {
    cat(sprintf("Directory already exists: %s\n", dir_path))
  }
}

# -----------------------------
# Step 1: Load Global Variables
# -----------------------------

# TODO: 3 user input parameters: year_w, date_str, save_date_str

pipeline_dir          <- here::here()  # Root directory of the pipeline
parent_dir <- dirname(pipeline_dir)
global_vars_file      <- file.path(pipeline_dir, "global_vars.R")
cat("Step 1: Loading global variables...\n")
source_if_exists(global_vars_file)

# Log the start of the pipeline
cat("=====================================================\n")
cat("Pipeline Execution Started\n")
cat("Parameters:\n")
cat("  Year of data to process:", year_w, "\n")
cat("  Date of data to use:", date_str, "\n")
cat("  Prcoessed data and Reports will be saved to:", save_date_str, "\n\n")
cat("=====================================================\n\n")


cat("Step 1 completed.\n\n")


###########################

cat("Step 2: Creating necessary directories based on save_date_str...\n")
# Define the directories to create

processed_data_dir <- file.path(parent_dir, "processed_data", save_date_str)
reports_dir        <- file.path(parent_dir, "reports", save_date_str)
pilot_reports_dir  <- file.path(parent_dir, "reports",  paste0("pilot_", save_date_str))

# dest_dir_all <- file.path("~/OneDrive/Documents/Support_proj/Aspin/reports", 
#                           paste0("all_", save_date_str))
# dest_dir_pilot <- file.path("~/OneDrive/Documents/Support_proj/Aspin/reports",
#                             paste0("pilot_", save_date_str))

# Create the directories if they do not exist

create_dir_if_not_exists(processed_data_dir)
create_dir_if_not_exists(reports_dir)
create_dir_if_not_exists(pilot_reports_dir)
# create_dir_if_not_exists(dest_dir_all )
# create_dir_if_not_exists(dest_dir_pilot)
cat("Step 2 completed.\n\n")

# Define file paths

dedup_file            <- file.path(pipeline_dir, "1_dedup_tab6.R")
scripts_to_run        <- c(
  "2_get_ICD10_uni_surg_id_spec.R",
  "3_load_data_coefs.R",
  "4_process_data.R",
  "5_combine_data_for_pred.R",
  "6_pred_prob.R"
)
batch_report_script   <- file.path(pipeline_dir, "8_batch_reports_email.R")
move_reports_script   <- file.path(pipeline_dir, "9_send_reports_onedrive.R")

cat("Step 3: Deduplicating table6 data (if required)...\n")

if (file.exists(fp_tab6_Surgeon_nullcpt)) {
  cat(sprintf("Processed data file '%s' already exists. Skipping deduplication step.\n\n", 
              basename(fp_tab6_Surgeon_nullcpt)))
} else {
  cat(sprintf("Processed data file '%s' not found. Running deduplication script...\n", 
              basename(fp_tab6_Surgeon_nullcpt)))
  source_if_exists(dedup_file)
  cat(sprintf("Deduplication completed. Processed data file '%s' should now exist.\n\n", 
              basename(fp_tab6_Surgeon_nullcpt)))
}
cat("Step 3 completed.\n\n")


# -----------------------------
# Step 4: Run Data Processing Scripts
# -----------------------------
cat("Step 4: Running data processing scripts...\n")
for (script in scripts_to_run) {
  script_path <- file.path(pipeline_dir, script)
  source_if_exists(script_path)
}
cat("All data processing scripts have been executed.\n\n")
# ------------------------
# check step 4
# ------------------------
if(file.exists(paste0(fp_processed, "postop_observed_rate.csv")) && 
   file.exists(paste0(fp_processed, "preop_expected_rate.csv")) ){
  cat("Step 4 completed: Good.\n\n")
}else{
  cat("Step 4 went wrong!.\n\n")
}




################### dot not run #########################
# -----------------------------
# Step 9: Generate Reports in Batch
# -----------------------------
cat("Step 5: Generating reports in batch using the template...\n")
source_if_exists(batch_report_script)
cat("Report generation completed.\n\n")
cat("Step 5 completed.\n\n")
