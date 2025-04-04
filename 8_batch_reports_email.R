# setwd('C:/Users/suy2/OneDrive - The University of Colorado Denver/Documents/R Files/ASPIN/Report_0409')
# surgeon_list = data.frame(name = c("MEGUID, ROBERT ALEXANDER"),
#                     specialty = c("Thoracic Surgery"))
# replace surgeon list

# move reports to OneDrive 
# cp ~/AspinReport/reports/20240911/*.docx ~/OneDrive/Documents/Support_proj/Aspin/reports/all_202409/
# cp ~/AspinReport/reports/pilot_20240911/*.docx ~/OneDrive/Documents/Support_proj/Aspin/reports/pilot_202409/
rm(list = ls())

library(tidyverse)
library(readxl)
library(here)

source("./global_vars.R")

# here() "/home/sheng/AspinReport/pipeline"


# 862 surgeons
# read_excel("../data/Surgeon_2023_with_Dep_MZ_addition_11_01_24.xlsx", sheet = 6)
# 325 surgeons
# read_excel("../data/2024_10_28_UCH_surgeons_and_pivot_tables_YS.xlsx", sheet = 6)

surgeon_list <- read_excel("../data/Surgeon_2023_with_Dep_MZ_addition_11_01_24.xlsx", sheet = 6) %>%
  dplyr::rename("specialty" = "ServiceLine",
                "name" = "PrimarySurgeonName")

# surgeon_list %>% filter(specialty == "Surgery: Thoracic")

date = Sys.Date()
library(mailR)
sender <- "noreply_aspin_report_test@ucdenver.edu"


# pilot_list <- read_excel("../data/Surgeon_2023_with_Dep_MZ_addition_11_01_24.xlsx", 
#            sheet = 7) %>%
#   dplyr::rename("specialty" = "ServiceLine",
#                 "name" = "PrimarySurgeonName")
# 
# # surgeon_list %>% filter(name == "SCHROEPPEL, THOMAS JOHN")
# 
# 
# new_rows <- tibble(
#   name = c("MATHES, DAVID W", "COOPWOOD, THOMAS BENTON JR.", 
#            "CRIBARI, CHRIS", "DORLAC, WARREN CHARLES",
#            "ERICKSON, CRYSTAL JOY", "LEWIS, HEATHER LYNN",
#            "PETRUN, BRANDEN", "REPPERT, AMY ELIZABETH",
#            "SCHROEPPEL, THOMAS JOHN"),
#   
#   specialty = c("Surgery: Plastic", "Surgery: General", "Surgery: General",
#                 "Surgery: General", "Surgery: Thoracic", "Surgical Oncology",
#                 "Surgery: General", "Surgery: Trauma", "Surgery: Trauma")
# )
# 
# pilot_list_updated <- bind_rows(pilot_list, new_rows)
# 
# saveRDS(pilot_list_updated, paste0(fp_processed,
#                                    "pilot_list_updated.rds"))

pilot_list_updated <-  readRDS(paste0("../processed_data/20241031/",
                                   "pilot_list_updated.rds"))

# pilot_list_updated %>% filter(specialty == "Surgery: Thoracic")
# 
# table(pilot_list_updated$specialty )
# TODO:change MUST change year and date_str inside this template 
# save_date_str
input_template <- "7b_ASPINreport_word.Rmd"

# TODO: run following lines to get full list of reports
n <- nrow(surgeon_list)
m <- 0
for (i in c(1:n)){
  
  newSpecialty = surgeon_list$specialty[i]
  newSurgeonName = surgeon_list$name[i]
  
  write.csv(newSpecialty, paste0(fp_processed,"nSpec.csv") )
  write.csv(newSurgeonName, paste0(fp_processed,"nName.csv") )
  
  tryCatch(
    {
  
  report_name <- paste0(fp_reports, gsub(" ", "_", gsub(", ", "_", newSurgeonName)), "_", 
                        gsub(" ", "_", gsub(" / ","_", gsub(": ", "_", newSpecialty))), "_",
                        date, ".docx")
  rmarkdown::render(input = input_template,
                    output_file = report_name)
  
  # TODO: save twice, directly to OneDrive
  
  # destination_file <- file.path( paste0("~/OneDrive/Documents/Support_proj/Aspin/reports/pilot_",
  #                                       save_date_str),
  #                                basename(report_name))
  # 
  # 
  # # Move (rename) the file from source to destination
  # file.rename(report_name, destination_file )
  
  message(paste("Report generated successfully:", report_name))
  m <- m + 1
    }, error = function(e) {
      # Handle the error, log it, and continue with the next iteration
      message(paste("Failed to generate report for:", report_name))
      message("Error:", e$message)
    }
  )
  # recipient <- c("kathryn.colborn@cuanschutz.edu")
  # send.mail(from = sender,
  #           to = recipient,
  #           cc = c("yi.2.su@cuanschutz.edu"),
  #           subject = "Automate email test",
  #           body = "Hi Katie,\n\nThis is a test.\nPlease see the report attached above.\n\nBest,\nYi",
  #           smtp = list(host.name = "mail.ucdenver.pvt", port = 25, 
  #                       ssl = FALSE),
  #           attach.files = c(report_name),
  #           authenticate = FALSE,
  #           send = TRUE)
  message(paste0("Report number ", i, " out of total ", n, " has been processed"))
}
message(paste0("Reports ", m, " out of total ", n, " have been generated successfully."))

# TODO: run run following lines only for the pilot list 
n <- nrow(pilot_list_updated)
m <- 0

for (i in c(1:n)){
  
  newSpecialty = pilot_list_updated$specialty[i]
  newSurgeonName = pilot_list_updated$name[i]
  
  write.csv(newSpecialty, paste0(fp_processed,"nSpec.csv") )
  write.csv(newSurgeonName, paste0(fp_processed,"nName.csv") )
  
  tryCatch(
    {
      
      report_name <- paste0(fp_reports_pilot, gsub(" ", "_", gsub(", ", "_", newSurgeonName)), "_", 
                            gsub(" ", "_", gsub(" / ","_", gsub(": ", "_", newSpecialty))), "_",
                            date, ".docx")
      rmarkdown::render(input = input_template,
                        output_file = report_name)
      
      # TODO: save twice, directly to OneDrive
      
      destination_file <- file.path( paste0("~/OneDrive/Documents/Support_proj/Aspin/reports/pilot_",
                                            save_date_str),
                                     basename(report_name))

      # 
      # # Move (rename) the file from source to destination
      file.rename(report_name, destination_file )
      
      message(paste("Report generated successfully:", report_name))
      m <- m + 1
    }, error = function(e) {
      # Handle the error, log it, and continue with the next iteration
      message(paste("Failed to generate report for:", report_name))
      message("Error:", e$message)
    }
  )
  # recipient <- c("kathryn.colborn@cuanschutz.edu")
  # send.mail(from = sender,
  #           to = recipient,
  #           cc = c("yi.2.su@cuanschutz.edu"),
  #           subject = "Automate email test",
  #           body = "Hi Katie,\n\nThis is a test.\nPlease see the report attached above.\n\nBest,\nYi",
  #           smtp = list(host.name = "mail.ucdenver.pvt", port = 25, 
  #                       ssl = FALSE),
  #           attach.files = c(report_name),
  #           authenticate = FALSE,
  #           send = TRUE)
  message(paste0("Report number ", i, " out of total ", n, " has been processed"))
}


message(paste0("Reports ", m, " out of total ", n, " have been generated successfully."))


################ Test MRN #############################################
# TODO: run run following lines only for the pilot list, with MRN table 
input_template <- "7c_ASPINreport_word_MRN.Rmd"
n <- nrow(pilot_list_updated)
m <- 0

for (i in c(1:n)){
  
  newSpecialty = pilot_list_updated$specialty[i]
  newSurgeonName = pilot_list_updated$name[i]
  
  write.csv(newSpecialty, paste0(fp_processed,"nSpec.csv") )
  write.csv(newSurgeonName, paste0(fp_processed,"nName.csv") )
  
  tryCatch(
    {
      
      report_name <- paste0(fp_reports_pilot, gsub(" ", "_", gsub(", ", "_", newSurgeonName)), "_", 
                            gsub(" ", "_", gsub(" / ","_", gsub(": ", "_", newSpecialty))), "_",
                            date, "_withMRN.docx")
      rmarkdown::render(input = input_template,
                        output_file = report_name)
      
      # TODO: save twice, directly to OneDrive
      
      destination_file <- file.path( paste0("~/OneDrive/Documents/Support_proj/Aspin/reports/pilot_",
                                            save_date_str),
                                     basename(report_name))
      
      # 
      # # Move (rename) the file from source to destination
      file.rename(report_name, destination_file )
      
      message(paste("Report generated successfully:", report_name))
      m <- m + 1
    }, error = function(e) {
      # Handle the error, log it, and continue with the next iteration
      message(paste("Failed to generate report for:", report_name))
      message("Error:", e$message)
    }
  )

  message(paste0("Report number ", i, " out of total ", n, " has been processed"))
}


message(paste0("Reports ", m, " out of total ", n, " have been generated successfully."))


#################### Only UCH Thoracic ######################################
uch_list <- read_excel("../data/2024_10_28_UCH_surgeons_and_pivot_tables_YS.xlsx", 
                       sheet = 6) %>%
  dplyr::rename("specialty" = "ServiceLine",
                "name" = "PrimarySurgeonName")

thoracic_list <-  uch_list %>% filter(specialty == "Surgery: Thoracic")

create_dir_if_not_exists <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat(sprintf("Created directory: %s\n", dir_path))
  } else {
    cat(sprintf("Directory already exists: %s\n", dir_path))
  }
}

pipeline_dir          <- here::here()  # Root directory of the pipeline
parent_dir <- dirname(pipeline_dir)

# TODO: change this date and date_str in template
# 
save_date_str <- "20241031_2023"
# save_date_str <- "20250203_2024" # which is date_str1, date_str2 in 7_uch_ASPINreport_word.Rmd
# change save_date_str in global_vars.R
 
# TODO: create save reports folder for both local and onedrive 
uch_thoracic_reports_dir  <- file.path(parent_dir, "reports",  
                                       paste0("uch_thoracic_", save_date_str))

dest_dir_uch_thoracic <- file.path("~/OneDrive/Documents/Support_proj/Aspin/reports",
                            paste0("uch_thoracic_", save_date_str))

create_dir_if_not_exists(uch_thoracic_reports_dir)
create_dir_if_not_exists(dest_dir_uch_thoracic)

## TODO: generate reports for only 4 UCH thoracic surgeons, year 2023, 2024
# using updated 7b_ASPINreport_word.Rmd, which is 7_uch_ASPINreport_word.Rmd
# 7_uch means filtered predicted probability data by uch_list
# TODO: setup year and date_str1, date_str2,

input_template <- "7_uch_ASPINreport_word.Rmd"

n <- nrow(thoracic_list)
m <- 0

for (i in c(1:n)){
  
  newSpecialty = thoracic_list$specialty[i]
  newSurgeonName = thoracic_list$name[i]
  
  write.csv(newSpecialty, paste0(fp_processed,"nSpec.csv") )
  write.csv(newSurgeonName, paste0(fp_processed,"nName.csv") )
  
  tryCatch(
    {
      
      report_name <- paste0(uch_thoracic_reports_dir , "/",
                            gsub(" ", "_", gsub(", ", "_", newSurgeonName)), "_", 
                            gsub(" ", "_", gsub(" / ","_", gsub(": ", "_", newSpecialty))), "_",
                            date, ".docx")
      rmarkdown::render(input = input_template,
                        output_file = report_name)
      
      # TODO: save twice, directly to OneDrive
      
      destination_file <- file.path( dest_dir_uch_thoracic,
                                     basename(report_name))
      
      # 
      # # Move (rename) the file from source to destination
      file.rename(report_name, destination_file )
      
      message(paste("Report generated successfully:", report_name))
      m <- m + 1
    }, error = function(e) {
      # Handle the error, log it, and continue with the next iteration
      message(paste("Failed to generate report for:", report_name))
      message("Error:", e$message)
    }
  )

  message(paste0("Report number ", i, " out of total ", n, " has been processed"))
}


message(paste0("Reports ", m, " out of total ", n, " have been generated successfully."))



