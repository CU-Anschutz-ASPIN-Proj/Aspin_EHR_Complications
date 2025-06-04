library(readr)
library(stringr)
library(magrittr)
library(tidyverse)
# Rest of your script...
source("./global_vars.R")
df_lab <- read_csv(paste0("../data/", date_str, "/", fp_prefix, 
                       "Table11_Lab_", date_str, ".csv"))
df_proc <- read_csv(paste0("../data/", date_str, "/", fp_prefix, 
                        "Table5_Procedures_", date_str, ".csv"))


df_medi <- read_csv(paste0("../data/", date_str, "/", fp_prefix, 
                        "Table7_Medications_", date_str, ".csv"))

# Filter by this year 
Epic_data <- readRDS(paste0(fp_processed,
                            "ICD10_uni_surg_id_spec.rds"))
#_r represents reduced version, speeds up run time and reduce RAM usage
Epic_data_r <- Epic_data%>%
  dplyr::select(arb_person_id, SurgeryDate_asDate, SurgeryID)

# TODO: ANTIHYPERTENSIVES
# name1 <- "ANTIHYPERTENSIVES"
# name2 <- "ANTIHYPERTENSIVES, ACE INHIBITORS"

# name1 <- "FOLIC ACID PREPARATIONS"
# name2 <- "FOLIC ACID PREPARATIONS"

# name1 <- "NOSE PREPARATIONS, VASOCONSTRICTORS(OTC)"
# name1 <- "NOSE PREPARATIONS, VASOCONSTRICTORS"
# name2 <- "NOSE PREPARATIONS"

name1 <- "\\bCT\\b.*\\b(chest|body)\\b"
name2 <- "\\bCT\\b.*\\b(abd(omen)?|pelv(is)?)\\b"

medi_search <- df_medi %>% dplyr::select(arb_person_id, PharmaceuticalClass) %>%
  # TODO: try to find rbc unit and rbc 
  filter(arb_person_id %in% Epic_data_r$arb_person_id) %>% 
  filter(str_detect(PharmaceuticalClass, regex(name1, ignore_case = TRUE)) 
         # |str_detect(PharmaceuticalClass, regex(name2, ignore_case = TRUE)) 
         )

table(medi_search)

medi_search <- df_medi %>% dplyr::select(PharmaceuticalClass) %>%
  filter(PharmaceuticalClass == name1 )

table(medi_search)

lab_search <- df_lab %>% dplyr::select(arb_person_id, LabPanelName) %>% filter(arb_person_id %in% Epic_data_r$arb_person_id) %>% 
  # TODO: try to find rbc unit and rbc 
  # TODO: find ANTI-OBESITY - ANOREXIC AGENTS
  # TODO: 
  filter(str_detect(LabPanelName, regex(name1, ignore_case = TRUE)) 
         | str_detect(LabPanelName, regex(name2, ignore_case = TRUE)) 
         )

table(lab_search)

lab_search <- df_lab %>% dplyr::select(LabPanelName) %>%
  filter(LabPanelName == name1 )

table(lab_search)

# TODO: only need to search "OS CT BODY", no matter upper or lower case 
# in column ProcedureName, and we only need to search "culture", 
# no matter upper or lower case from column LabPanelName

# TODO: search for "RBC Unit"
# Search for "OS CT BODY" in df_proc and extract relevant columns
proc_search <- df_proc %>%
  dplyr::select(
    arb_person_id, 
    arb_encounter_id, 
    ProcedureOrderDate, 
    ProcedureEventDate, 
    ProcedureName
  ) %>% filter(arb_person_id %in% Epic_data_r$arb_person_id) %>% 
  # TODO: no result for rbc unit, too many for "rbc"
  filter(str_detect(ProcedureName, regex(name1, 
                                         ignore_case = TRUE))
         | str_detect(ProcedureName, regex(name2, 
                                           ignore_case = TRUE))
         ) 

table(proc_search$ProcedureName)


proc_search <- df_proc %>% dplyr::select(ProcedureName) %>%
  filter(ProcedureName == name1 )

table(proc_search)

######################################################################
# colnames(df_lab)
# colnames(df_proc)
# 
# # TODO: 
# # string contains “OS CT BODY” from df_proc, columns: TYPE Category ProcedureName
# # string contains “culture” from df_lab, columns: LabComponentName LabPanelName
# 
# # glimpse(df_proc)
# result_proc <- df_proc %>%
#   filter(
#     str_detect(TYPE, regex("OS CT BODY", ignore_case = TRUE)) |
#       str_detect(Category, regex("OS CT BODY", ignore_case = TRUE)) |
#       str_detect(ProcedureName, regex("OS CT BODY", ignore_case = TRUE)) |
#       str_detect(PatientFriendlyName, regex("OS CT BODY", ignore_case = TRUE))
#   )
# 
# # result_proc
# table(result_proc$ProcedureName)
# 
# glimpse(result_proc)
# 
# # glimpse(df_lab)
# 
# # search CDIFFICILE
# 
# # cultures <- read.csv(file = "../data/cultureAllInfections.csv")
# # Only LabPanelName
# 
# result_lab <- df_lab %>%
#   filter(
#     str_detect(LabComponentName, regex("culture", ignore_case = TRUE)) |
#       str_detect(LabPanelName, regex("culture", ignore_case = TRUE))
#   )
# 
# glimpse(result_lab)
# 
# # colnames(Proc)
# 
# table(result_lab$LabComponentName)
# table(result_lab$LabPanelName )
# 
# # table(df_proc$ProcedureName)
# 
# # Search for "OS CT BODY" in df_proc and extract relevant columns
# df_os_ct_search <- df_proc %>%
#   filter(str_detect(ProcedureName, regex("OS CT BODY", ignore_case = TRUE))) %>%
#   select(
#     arb_person_id, 
#     arb_encounter_id, 
#     ProcedureOrderDate, 
#     ProcedureEventDate, 
#     ProcedureName
#   )
# 
# table(df_os_ct_search$ProcedureName)
# 
# # TODO: search CDIFFICILE
# 
# # Search for "culture" in df_lab and extract relevant columns
# # CDIFF
# # CDIFFICILE TOXIN PCR
# # LAB USE ONLY - C.DIFFICILE TOXIN REFLEX
# # C DIFF GDH/TOXIN
# # C DIFFICILE TOXIN/GDH
# # LAB USE ONLY - C.DIFFICILE TOXIN REFLEX (GI PCR)
# # glimpse(df_lab)
# 
# df_CDIFF_search <- df_lab %>%
#   filter(str_detect(LabPanelName, regex("C[^/]DIFF", ignore_case = TRUE)) |
#            str_detect(LabPanelName, regex("CDIFF", ignore_case = TRUE)) ) %>%
#   select(
#     arb_person_id, 
#     arb_encounter_id, 
#     LabCollectedDate, 
#     LabResultedDate, 
#     LabPanelName
#   )
# 
# table(df_CDIFF_search$LabPanelName) %>% names()
# 
# # Search for "culture" in df_lab and extract relevant columns
# df_culture_search <- df_lab %>%
#   filter(str_detect(LabPanelName, regex("culture", ignore_case = TRUE))) %>%
#   select(
#     arb_person_id, 
#     arb_encounter_id, 
#     LabCollectedDate, 
#     LabResultedDate, 
#     LabPanelName
#   )
# typeof(df_culture_search$LabCollectedDate)
# typeof(df_culture_search$LabResultedDate)
# 
# table(df_culture_search$LabCollectedDate)
# table(df_culture_search$LabResultedDate)
# table(df_culture_search$LabPanelName)
# 
# # glimpse(df_CDIFF_search)
# 
# # TODO: for culture search Only search data back to 2023, not using too old data
# 
# df_culture_2023 <- df_culture_search %>%
#   mutate(
#     LabCollectedDate = as.Date(LabCollectedDate, origin = "1970-01-01"),
#     LabResultedDate = as.Date(LabResultedDate, origin = "1970-01-01")
#   ) %>%
#   filter(
#     LabCollectedDate >= as.Date("2023-01-01") | 
#       LabResultedDate >= as.Date("2023-01-01")
#   )
# 
# 
# table(df_culture_2023$LabCollectedDate)
# table(df_culture_2023$LabResultedDate)
# 
# # Check 2024 "CDIFFICILE"
# 
# df_CDIFF_2023 <- df_CDIFF_search %>%
#   mutate(
#     LabCollectedDate = as.Date(LabCollectedDate, origin = "1970-01-01"),
#     LabResultedDate = as.Date(LabResultedDate, origin = "1970-01-01")
#   ) %>%
#   filter(
#     LabCollectedDate >= as.Date("2023-01-01") | 
#       LabResultedDate >= as.Date("2023-01-01")
#   )
# 
# glimpse(df_CDIFF_2023)
# 
# table(df_CDIFF_2023$LabPanelName)
# 
# 
# table(df_culture_2023$LabPanelName) %>% as.data.frame() %>%
#   write.csv(., paste0(fp_processed, "lab_culture_freq_", date_str, ".csv"), 
#             row.names = FALSE)
# 
# table(df_os_ct_search$ProcedureName) %>% as.data.frame() %>%
#   write.csv(., paste0(fp_processed, "procedure_OSCTBody_freq_", date_str, ".csv"), 
#             row.names = FALSE)
# 
# 
# 
# 
