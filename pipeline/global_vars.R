# folder structure 
# AspinReport
# data pipeline processed_data reports
# data in shared drive 
# \\data.ucdenver.pvt\dept\SOM\ACCORDS\Data\ACCORDSGPUHDCDATA

# TODO: a parameter
years <- c("2021", "2022", "2023", "2024")
# year, date and time related variables
# TODO: a parameter
year_w <- 2025
 # date_str <- "20240911"
# date_str <- "20241121"
date_str <- "20250520"
# date_str <- "20241219"
# date_str <- "20241031"

# save_date_str <- "20241031_2023"
# save_date_str <- "20241031"
# save_date_str <- "20241219"
# TODO: a parameter
save_date_str <- "20250520"
# file path related variables 
fp_processed <- paste0("../processed_data/", save_date_str, "/")


fp_prefix <- "C2730_"
fp_tab2 <- paste0("../data/", date_str, "/", fp_prefix, "Table2_Encounters_", date_str, ".csv")
fp_tab6 <- paste0("../data/", date_str, "/", fp_prefix, "Table6_Surgery_", date_str, ".csv")
fp_tab6_Surgeon_nullcpt <- paste0("../processed_data/", save_date_str, "/",
                                  "table6_", year_w, "_Surgeon_nullcpt.rds")

fp_reports <- paste0("../reports/", save_date_str, "/")
fp_reports_pilot <- paste0("../reports/pilot_", save_date_str, "/")

