# TODO: may update the list as new HDC data delivered   
# 06/04/2025, lists for 2021 - 2024 HDC data

# Define variables to preserve
vars_to_keep <- c("pipeline_dir", "source_if_exists")

# Remove all objects except those in vars_to_keep
rm(list = setdiff(ls(), vars_to_keep))

# Rest of your script...
library(openxlsx)
library(lubridate)
library(kableExtra)
library(openxlsx)
library(tidyverse)
library(here) 
library(gtsummary)

source("./global_vars.R")

# years should be 
# load 2021 - 2024 data, predictive probability and all predictors 
fp_base_dir <- paste0("../processed_data/")
years_date_str <- "20250203"
df_all_var_prob <- read_csv(file.path(fp_base_dir, years_date_str, "Aspin_all_complications_pred_prob_20212024.csv"))
# dim(df_all_var_prob)

df_all_var_prob %>% dplyr::select(arb_person_id) %>% dplyr::distinct(arb_person_id) %>% dplyr::pull(arb_person_id) %>% length()
# 
pre_op_missing_cols <- c(
  "OS CT BODY COMPARE-NO READ",
  "ANTIBODY PATIENT INTERPS 1-5",
  "CLOS LARGE BOWEL BIOPSY",
  "UPDATE PATIENT CLASS I.E. INPATIENT / OBSERVATION"
)

post_op_missing_cols <- c(
  "CPAP OVERNIGHT",
  "INSERT ARTERIAL LINE",
  "XR CHEST 2 VIEW (PA,LAT)",
  "POINT OF CARE TESTS",
  "INCISION AND DRAINAGE HEAD/NECK",
  "LAST TYPE & SCREEN (CLOT) STATUS",
  "POCT CRITICAL PANEL",
  "ADMIT PATIENT",
  "FEMORAL NERVE BLOCK",
  "RETURN PATIENT",
  "NSAIDS, CYCLOOXYGENASE INHIBITOR - TYPE ANALGESICS",
  "POCT GLUCOSE CPT 82962 INTERFACED RESULT/DOCKED DEVICE",
  "RESTRAINTS NON-BEHAVIORAL",
  "TRANSFER PATIENT",
  "D DIMER QUANTITATIVE",
  "IP CONSULT TO PHARMACY - TPN",
  "IP CONSULT TO WOUND / OSTOMY / SKIN TEAM",
  "US RENAL (KIDNEYS/BLADDER ONLY)",
  "US UPPER EXTREMITY VENOUS BIL",
  "WEIGH PATIENT",
  "PREADMISSION ADMIT ORDER - RN TO RELEASE",
  "UPDATE PATIENT CLASS I.E. INPATIENT / OBSERVATION",
  ".XR CHEST SINGLE (PA)",
  "CTA CHEST FOR PE",
  "OPN RT HEMICOLECTOMY NEC",
  "US UPPER EXTREMITY VENOUS UNILATERAL"
)

