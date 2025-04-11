# Define variables to preserve
vars_to_keep <- c("pipeline_dir", "source_if_exists")

# Remove all objects except those in vars_to_keep
rm(list = setdiff(ls(), vars_to_keep))

# Rest of your script...


library(tidyverse)
library(data.table)#fread

source("./global_vars.R")
# diag
# TODO: load ICD10
# colnames(Epic_data)
# colnames(Epic_data)[grepl("phecode", colnames(Epic_data), ignore.case = TRUE)]
Epic_data <- readRDS(paste0(fp_processed,
                    "ICD10_uni_surg_id_spec.rds"))
#_r represents reduced version, speeds up run time and reduce RAM usage
Epic_data_r <- Epic_data%>%
  dplyr::select(arb_person_id, SurgeryDate_asDate, SurgeryID)
# sum(is.na(Epic_data$SurgeryID))

# table(Epic_data$PrimarySurgeonSpecialty)

# which(Epic_data$PrimarySurgeonName == "CRIBARI, CHRIS")
# dim(Epic_data) [1] 22484   271
# > dim(Epic_data) [1] 22633   271

# Diag table4
# read_csv
# TODO: fread
Diag <- fread(paste0("../data/", date_str, "/", fp_prefix, 
                        "Table4_Diagnosis_", date_str, ".csv"))%>%as.data.frame()

phe_code <- read.table(file = "../data/ICDMAP.txt",header = TRUE)
# lab table11
Lab <- fread(paste0("../data/", date_str, "/", fp_prefix, 
                       "Table11_Lab_", date_str, ".csv"))%>%as.data.frame()
Lab_r <- Lab%>%
  dplyr::select(arb_person_id, LabPanelName, LabCollectedDate)

# medi table7
Medi <- fread(paste0("../data/", date_str, "/", fp_prefix, 
                        "Table7_Medications_", date_str, ".csv"))%>%as.data.frame()
Medi_r <- Medi%>%
  dplyr::select(arb_person_id, TherapeuticClass, PharmaceuticalClass, OrderedDate)

# proc table5
Proc <- fread(paste0("../data/", date_str, "/", fp_prefix, 
                        "Table5_Procedures_", date_str, ".csv"))%>%as.data.frame()
Proc_r <- Proc%>%
  dplyr::select(arb_person_id, ProcedureName, ProcedureEventDate)

# gender table1 
patient <- fread(paste0("../data/", date_str, "/", fp_prefix, 
                           "Table1_Person_", date_str, ".csv"))%>%as.data.frame()
# need to check if this is in accordance with your folder
mrn <- fread(paste0("../data/", date_str, "/", fp_prefix, 
                    "MRN_", date_str, ".csv"))%>%as.data.frame()

# # preop rate
# SSI_coef_preop <- as.data.frame(read.csv(file = "../data/model/SSI_coef_20220906.csv",check.names=FALSE))
# UTI_coef_preop <- as.data.frame(read.csv(file = "../data/model/UTI_coef_2022_0906.csv",check.names=FALSE))
# SYSEP_coef_preop <- as.data.frame(read.csv(file = "../data/model/SYSEP_coef_20220906_est.csv",check.names=FALSE))
# PNEU_coef_preop <- as.data.frame(read.csv(file = "../data/model/PNEU_coef_20220906.csv",check.names=FALSE))
# load("../data/model/glmSSI.rdata")
# # View(glmSSI)
# # glimpse(glmSSI)
# load("../data/model/glmUTI.rdata")
# load("../data/model/glmSYSEP.rdata")
# load("../data/model/glmPNEU.rdata")
# # postop rate
# SSI_coef_postop <- as.data.frame(read.csv(file = "../data/model/SSI_coefficients_1220.csv",check.names=FALSE))
# UTI_coef_postop <- as.data.frame(read.csv(file = "../data/model/UTI_coefficients_1220.csv",check.names=FALSE))
# SYSEP_coef_postop <- as.data.frame(read.csv(file = "../data/model/SYSEP_coefficients_0127.csv",check.names=FALSE))
# PNEU_coef_postop <- as.data.frame(read.csv(file = "../data/model/PNEU_coefficients_1220.csv",check.names=FALSE))


