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

years_date_str2 <- "20250305"

# list_Epic_data <- list()
# 
# for(year_val in years){
#   current_year_folder <- paste0(years_date_str, "_", year_val)
#   current_fp_processed <- file.path(fp_base_dir, current_year_folder)
#   
#   cat("Processing year:", year_val, "\n")
#   
#   epic_file_path <- file.path(current_fp_processed, "ICD10_uni_surg_id_spec.rds")
#   cat("  Looking for ICD10_uni_surg_id_spec file:", epic_file_path, "\n")
#   if (file.exists(epic_file_path)) {
#     datepic_year <- read_rds(epic_file_path)
#     list_Epic_data[[as.character(year_val)]] <- datepic_year
#     cat("    Loaded ICD10_uni_surg_id_spec data for", year_val, ". Rows:", nrow(datepic_year), "\n")
#   } else {
#     warning(paste("ICD10_uni_surg_id_spec file not found for year", 
#                   year_val, ":", epic_file_path))
#   }
# }
# 
# # --- Combine all years' data ---
# if (length(list_Epic_data) > 0) {
#   Epic_data_all <- dplyr::bind_rows(list_Epic_data)
#   cat("Total rows in combined Epic_data_all:", nrow( Epic_data_all), "\n")
# } else {
#   warning("No Epic data loaded. Creating an empty Epic_data_all dataframe.")
#   Epic_data_all <- data.frame() 
# }
# 
# Epic_data_all %<>% dplyr::select(arb_person_id , SurgeryDate_asDate, SurgeryID) 
# 
# # dim(Epic_data_all)
# # 06/04/2025 read deduplicated list of 2025
# Epic_data <- read_rds(paste0(fp_processed,
#                             "ICD10_uni_surg_id_spec.rds")) %>%
#   dplyr::select(arb_person_id, SurgeryDate_asDate, SurgeryID)
# 
# # dedup list of patients by year 
# dedup_list_year <- dplyr::bind_rows(Epic_data_all, Epic_data) %>% 
#               mutate(DOS = as.Date(SurgeryDate_asDate ),
#                      year = year(DOS))
# 
# write.csv(dedup_list_year, 
#           file = paste0(fp_processed,
#                         "dedup_pts_list_years.csv"), 
#           row.names = FALSE)
# table(dedup_list_year$year, useNA = "ifany")

dedup_list_year <- read_csv(paste0(fp_processed,
                         "dedup_pts_list_years.csv"))
# dim(dedup_list_year)

# -----------------------------------------
# load old table 6 surgery table which does not have temperature variables 

table6_notemp1 <- read_csv(paste0("../data/", years_date_str, "/", 
                fp_prefix, "Table6_Surgery_", years_date_str, ".csv"))

table6_notemp1 %<>% mutate(DOS = as.Date(SurgeryDate),
                          year = year(DOS))

table6_notemp2 <- read_csv(paste0("../data/", years_date_str2, "/", 
                                  fp_prefix, "Table6_Surgery_", years_date_str2, ".csv"))

table6_notemp2 %<>% mutate(DOS = as.Date(SurgeryDate),
                           year = year(DOS))

# ------------
# read table6 and filter 
# tbl_summary of , by year, using table6 
# FirstPostProcedureCoreTemp
# HighestCoreTempDuringProcedure
# HighestTempNearAnesthesiaStop
# AnesthesiaTemp


table6 <- read_csv(fp_tab6)
# problems(table6)

table6 %<>% mutate(DOS = as.Date(SurgeryDate),
                   year = year(DOS))

# table(table6$year, useNA = "ifany")

table6_notemp1 %>% group_by(year) %>% dplyr::summarise(n_surgeries = n()) %>%  
            kableExtra::kbl(caption = paste("Number of Surgeries in table 6", years_date_str),
                            col.names = c("Year", "Num_surg"))

table6_notemp2 %>% group_by(year) %>% dplyr::summarise(n_surgeries = n()) %>%  
  kableExtra::kbl(caption = paste("Number of Surgeries in table 6", years_date_str2),
                  col.names = c("Year", "Num_surg"))

table6 %>% group_by(year) %>% dplyr::summarise(n_surgeries = n()) %>%  
  kableExtra::kbl(caption = paste("Number of Surgeries in table 6", date_str),
                  col.names = c("Year", "Num_surg"))

# -----------------------------------------------
# temp vars 

temp_vars <- c("firstpostprocedurecoretemperature",
               "highestcoretemperatureduringprocedure",
               "highesttempnearanesthesiastop",
               "AnesthesiaTemp")
table6 %<>% dplyr::rename(AnesthesiaTemp = AnesthesisaTemp)
# temp vars in each table 
assign(paste0("temp_var_", years_date_str),
colnames(table6_notemp1)[grepl("temp", colnames(table6_notemp1), ignore.case = TRUE)])

assign(paste0("temp_var_", years_date_str2),
colnames(table6_notemp2)[grepl("temp", colnames(table6_notemp2), ignore.case = TRUE)])

assign(paste0("temp_var_", date_str),
colnames(table6)[grepl("temp", colnames(table6), ignore.case = TRUE)] )

# temp_var_20250203
# temp_var_20250305
# temp_var_20250520
# ------------------------------------
# summary 
table6 %>% tbl_summary(by = year, 
     include = all_of(temp_vars),
     statistic = list(all_categorical() ~ "{n} ({p}%)",
                      all_continuous() ~ c(
                        "{N_nonmiss}",
                        "{median} ({p25}, {p75})",
                        "{min}, {mean} ({sd}), {max}"
                      )),
     digits = all_continuous() ~ 3,
     missing_text = "(Missing)",
     type = list(all_continuous() ~ "continuous2",
                 all_dichotomous() ~ "categorical")) %>% 
  bold_labels()

