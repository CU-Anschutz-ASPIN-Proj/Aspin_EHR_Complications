# source("./3_load_data_coefs.R")

library(lubridate)
library(tidyr)
library(tidyverse)
library(plyr)
library(dplyr)
library(fastDummies)
library(bit64)
library(magrittr)

#############################################################################
# regualr expression match ProcedureName or LabPanelName
# the vector of matched strings should not be removed by rm

# 1. first, generate string vectors here, 
# 2. then, filter lab or Proc_r using those vectors, as |(lab_cohort_r$LabPanelName %in% CDIFFs)

# 3. then in 5_combine_data_, check message(CDIFFs_to_combine), avoid using grep again
# 4. may be using CDIFFs using CDIFFs[CDIFFs %in% colnames(analytical_postopv2)]

# TODO: 5. check 2024 Aspin infectious only pipeline of message(cultures_to_combine), 
# TODO: to resolve conflicts message(blood_cultures_to_combine), message(urine_cultures_to_combine), message(CDIFFs_to_combine)

# finally, using rowSums ifelse(rowSums(analytical_preopv2[, cultures_to_combine])>=1,1,0)


df_lab <- Lab_r[Lab_r$arb_person_id %in% Epic_data_r$arb_person_id,] %>% dplyr::select(LabPanelName)

df_proc <- Proc_r[Proc_r$arb_person_id %in% Epic_data_r$arb_person_id,] %>% dplyr::select(ProcedureName)

######### temp check #############
# length(df_proc$ProcedureName)
# name1 <- "OS CT BODY COMPARE-NO READ"
# 
# temp_proc_list <- df_proc %>% 
#   filter(
#     str_detect(ProcedureName, regex(name1, ignore_case = TRUE)) 
#   )
# table(temp_proc_list$ProcedureName, useNA = "ifany")
################# pre-op
# POCT CRITICAL PANEL
name1 <- "\\bPOCT\\s+CRITICAL\\s+PANEL\\b"
# table(df_lab$LabPanelName)
# Create a list of matching LabPanelNames
POCT_CRITICAL_PANEL_list <- df_lab %>%
  dplyr::filter(str_detect(LabPanelName, regex(name1, ignore_case = TRUE))) %>%
  dplyr::distinct(LabPanelName) %>%
  dplyr::pull(LabPanelName)

# sum(df_lab$LabPanelName == "POCT CRITICAL PANEL")

# ANTIBODY PATIENT INTERPS 1-5
# Define the regex match both "ANTIBODY PATIENT INTERPS" and "ANTIBODY INTERPS"
regex_antibody_interps <- "\\bANTIBODY\\s+(?:PATIENT\\s+)?INTERPS\\b"
# table(df_lab$LabPanelName)
# Create a list of matching LabPanelNames
ANTIBODY_PATIENT_INTERPS_list <- df_lab %>%
  dplyr::filter(str_detect(LabPanelName, regex(regex_antibody_interps, ignore_case = TRUE))) %>%
  dplyr::distinct(LabPanelName) %>%
  dplyr::pull(LabPanelName)

# CLOS LARGE BOWEL BIOPSY
name1 <- "\\b(?:CLOS|COLONOSCOPY)\\b.*\\bBIOPSY\\b"
name2 <- "\\b(?:LARGE\\s*BOWEL|COLON)\\b.*\\bBIOPSY\\b"

CLOS_LARGE_BOWEL_BIOPSY_list <- df_proc %>% 
  filter(
    ( str_detect(ProcedureName, regex(name1, ignore_case = TRUE)) |
        str_detect(ProcedureName, regex(name2, ignore_case = TRUE)) )
    # &
    #   !str_detect(ProcedureName, regex(exclude_pattern, ignore_case = TRUE))
  ) %>%
  dplyr::distinct(ProcedureName) %>% 
  dplyr::pull(ProcedureName)

# TODO: match: UPDATE PATIENT CLASS I.E. INPATIENT / OBSERVATION
name1 <- "\\bUPDATE\\s+PATIENT\\s+CLASS\\b"

UPDATE_PATIENT_CLASS_list <- df_proc %>% 
  filter(
     str_detect(ProcedureName, regex(name1, ignore_case = TRUE)) 
  ) %>%
  dplyr::distinct(ProcedureName) %>% 
  dplyr::pull(ProcedureName)
# sum(df_proc$ProcedureName == "UPDATE PATIENT CLASS I.E. INPATIENT / OBSERVATION")
# CPAP OVERNIGHT
name1 <- "\\bCPAP\\s+OVERNIGHT\\b"

CPAP_OVERNIGHT_list <- df_proc %>% 
  filter(
    str_detect(ProcedureName, regex(name1, ignore_case = TRUE)) 
  ) %>%
  dplyr::distinct(ProcedureName) %>% 
  dplyr::pull(ProcedureName)
# sum(df_proc$ProcedureName == "CPAP OVERNIGHT")
# sum(df_proc$ProcedureName == "RETURN PATIENT")
# sum(df_proc$ProcedureName == "ADMIT PATIENT")
# sum(df_proc$ProcedureName == "TRANSFER PATIENT")
# sum(df_proc$ProcedureName == "UPDATE PATIENT CLASS I.E. INPATIENT / OBSERVATION")
# CT.BODY.CHEST.PELV.W.ABD

name1 <- "\\bCT\\b.*\\b(chest|body)\\b"
name2 <- "\\bCT\\b.*\\b(abd(omen)?|pelv(is)?)\\b"

# : no "biopsy", no "abscess drain/drn", no "drn w tube", no "tube placement"
# : no "cryo ablation", no "fine needle aspiration", no "anes chg"

# exclude these unwanted procedures
exclude_pattern <- "\\b(?:biopsy|abscess|drn\\s*w\\s*tube|tube\\s*placement|cryo\\s*ablation|fine\\s*needle\\s*aspiration|anes\\s*chg)\\b"

CT_BODY_CHEST_PELV_W_ABD_list <- df_proc %>% 
  filter(
    ( str_detect(ProcedureName, regex(name1, ignore_case = TRUE)) |
        str_detect(ProcedureName, regex(name2, ignore_case = TRUE)) )
    &
      !str_detect(ProcedureName, regex(exclude_pattern, ignore_case = TRUE))
  ) %>%
  dplyr::distinct(ProcedureName) %>% 
  dplyr::pull(ProcedureName)


# STRESS.NUCLEAR
name1 <- "STRESS NUCLEAR"
name2 <- "NUCLEAR STRESS"

STRESS_NUCLEAR_list <- df_proc %>% 
  # TODO: no result for rbc unit, too many for "rbc"
  filter(str_detect(ProcedureName, regex(name1, 
                                         ignore_case = TRUE))
         | str_detect(ProcedureName, regex(name2, 
                                           ignore_case = TRUE))
  ) %>%
  dplyr::distinct(ProcedureName) %>% 
  dplyr::pull(ProcedureName)

# POTASSIUM

name1 <- "POTASSIUM"
# 2) any iSTAT, POCT or generic assay of potassium
name2 <- "\\b(?:I[- ]?STAT|POCT|ASSAY)\\b.*\\bPOTASSIUM\\b"

# exclude unwanted panels
exclude_pattern <- "urine|or\\s*potassium\\s*wb|channel\\s*antibody"

POTASSIUM_list <- df_lab %>% 
  filter( ( str_detect(LabPanelName, regex(name1, ignore_case = TRUE)) 
            | str_detect(LabPanelName, regex(name2, ignore_case = TRUE)) ) &
            !str_detect(LabPanelName, regex(exclude_pattern, ignore_case = TRUE))
  ) %>%
    dplyr::distinct(LabPanelName) %>% 
    dplyr::pull(LabPanelName)


# CENTRAL line

# generic “central line”
name1 <- "\\bCENTRAL\\s+LINE\\b"

# the insertion procedure explicitly
name2 <- "\\bCENTRAL\\s+LINE\\s+INSERTION\\b"

# exclude these unwanted keywords
exclude_pattern <- "assess\\s+need|care|discontinue"

CENTRAL_line_list <- df_proc %>%
  filter(
    ( str_detect(ProcedureName, regex(name1, ignore_case = TRUE)) |
        str_detect(ProcedureName, regex(name2, ignore_case = TRUE)) )
    &
      !str_detect(ProcedureName, regex(exclude_pattern, ignore_case = TRUE))
  ) %>%
  dplyr::distinct(ProcedureName) %>% 
  dplyr::pull(ProcedureName)

################# post-op

# Post CDIFFs lab

# Streamlined dplyr pipeline to get unique LabPanelName values
CDIFFs <-  df_lab %>% 
  dplyr::filter(str_detect(LabPanelName, regex("C[^/]DIFF", ignore_case = TRUE)) |
                    str_detect(LabPanelName, regex("CDIFF", ignore_case = TRUE)) ) %>%
  dplyr::distinct(LabPanelName) %>% # Get unique rows based on LabPanelName
  dplyr::pull(LabPanelName)         # Extract LabPanelName as a character vector

# Post respiratory_comb lab

name1 <- "RESPIRATORY CULTURE"

name2 <- "RESPIRATORY CULTURE"

RESPIRATORY_CULTURE_list <- df_lab %>% 
  filter(str_detect(LabPanelName, regex(name1, ignore_case = TRUE)) 
         | str_detect(LabPanelName, regex(name2, ignore_case = TRUE)) 
  ) %>%
  dplyr::distinct(LabPanelName) %>%
  dplyr::pull(LabPanelName) 

# post BLOOD_GASES_comb lab

# 1) Any “blood gas” or “blood gases”
name1 <- "\\bBLOOD\\s+GAS(?:ES)?\\b"

# 2) The GEM platform variant explicitly
name2 <- "\\(GEM\\)\\s*BLOOD\\s+GASES?"

exclude_term <- "\\bCORD\\b"

BLOOD_GASES_comb_list <- df_lab %>%
  filter(
    # Original conditions for detecting blood gas panels
    (
      str_detect(LabPanelName, regex(name1, ignore_case = TRUE)) |
        str_detect(LabPanelName, regex(name2, ignore_case = TRUE))
    ) & # AND operator: both the above OR condition AND the below NOT condition must be true
      # New condition: LabPanelName should NOT contain the exclude_term_cord
      !str_detect(LabPanelName, regex(exclude_term, ignore_case = TRUE))
  ) %>%
  dplyr::distinct(LabPanelName) %>%
  dplyr::pull(LabPanelName)

# red_blood_cell_or_whole_blood_transfusion

# catches “BLOOD TRANSFUSION” OR “PACKED CELL” OR “RED BLOOD CELLS”
name1 <- "\\b(?:BLOOD\\s+TRANSFUSION|PACKED\\s+CELL|RED\\s+BLOOD\\s+CELLS?)\\b"

# catches the standalone “RBC UNIT”
name2 <- "\\bRBC\\s*UNIT\\b"

# TODO: no "products of conception"
exclude_pattern <- "\\bPRODUCT(?:S)?\\s+OF\\s+CONCEPTION\\b"

red_blood_cell_or_whole_blood_transfusion_list <- df_proc %>%
  filter(
    # Original conditions for detecting relevant procedures
    (
      str_detect(ProcedureName, regex(name1, ignore_case = TRUE)) |
        str_detect(ProcedureName, regex(name2, ignore_case = TRUE))
    ) & # AND operator: both the above OR condition AND the below NOT condition must be true
      # New condition: ProcedureName should NOT contain the exclude_term_poc
      !str_detect(ProcedureName, regex(exclude_pattern, ignore_case = TRUE))
  ) %>%
  dplyr::distinct(ProcedureName) %>% 
  dplyr::pull(ProcedureName)

# intubation

# 1) any INSERT/INSERTION of an endotracheal tube or airway
name1 <- "\\bINSERT(?:ION)?\\b.*\\bENDOTRACHEAL\\b"

# name1 <- "Endotracheal"

# 2) any CHANGE of an endotracheal airway
name2 <- "\\bCHANGE\\b.*\\bENDOTRACHEAL\\b"


intubation_list <- df_proc %>%
  filter(
    str_detect(ProcedureName, regex(name1, ignore_case = TRUE)) |
      str_detect(ProcedureName, regex(name2, ignore_case = TRUE))
  ) %>%
  dplyr::distinct(ProcedureName) %>% 
  dplyr::pull(ProcedureName)


# Blood.products

# 1) Matches "PLATELET UNIT" or "PLATELETS UNIT"
name1 <- "\\bPLATELET(?:S)?\\s+UNIT\\b"

# 2) Matches "PREPARE PLATELET FOR TRANSFUSION" or "PREPARE PLATELETS FOR TRANSFUSION"
name2 <- "\\bPREPARE\\s+PLATELET(?:S)?\\s+FOR\\s+TRANSFUSION\\b"

# TODO: only keep "PLATELETS UNIT" and "PREPARE PLATELETS FOR TRANSFUSION" 

Blood_products_list <- df_lab %>% 
  filter(str_detect(LabPanelName, regex(name1, ignore_case = TRUE)) 
         | str_detect(LabPanelName, regex(name2, ignore_case = TRUE)) 
  ) %>%
  dplyr::distinct(LabPanelName) %>%
  dplyr::pull(LabPanelName)

# echo

# 1) Any echo exam (TTE, TEE, Doppler, contrast, etc.)
name1 <- "\\bECHO\\b"

# 2) Stress echo protocols (exercise or dobutamine)
name2 <- "\\b(?:STRESS\\s+ECHO|DOBUTAMINE\\s+STRESS\\s+ECHO)\\b"

# TODO, no " PERICARDIOCENTESIS", no "PEDIATRIC", no "intracardiac"
# TODO, no " fetal ", no " anes " (Anesthesia), no " amb "
# TODO, no "TEE GUID", no "pelvic", no "TRANSRECTAL"
# TODO, no "scrotum"
exclusion_regex_parts <- c(
  "\\bPERICARDIOCENTESIS\\b",
  "\\bPEDIATRIC\\b",
  "\\BINTRACARDIAC\\b",
  "\\bFETAL\\b",           # Handles " fetal " by matching the whole word
  "\\bANES\\b",            # Handles " anes "; using \b for whole word/abbreviation
  "\\bAMB\\b",             # Handles " amb "; using \b for whole word/abbreviation
  "\\bTEE\\s+GUID\\b",     # Specific phrase "TEE GUID"
  "\\bPELVIC\\b",
  "\\bTRANSRECTAL\\b",
  "\\bSCROTUM\\b"
)

# Combine exclusion terms into a single regex pattern
exclude_pattern <- paste(exclusion_regex_parts, collapse = "|")

ECHO_list <- df_proc %>%
  filter(
    # Initial condition: ProcedureName must match name1 OR name2
    (
      str_detect(ProcedureName, regex(name1, ignore_case = TRUE)) |
        str_detect(ProcedureName, regex(name2, ignore_case = TRUE))
    ) & # AND operator
      # Exclusion condition: ProcedureName must NOT match any of the exclude_pattern terms
      !str_detect(ProcedureName, regex(exclude_pattern, ignore_case = TRUE))
  ) %>%
  dplyr::distinct(ProcedureName) %>% 
  dplyr::pull(ProcedureName)

# PR PARTIAL HIP REPLACEMENT, PARTIAL HIP REPLACEMENT

# troponin

# 1) Generic troponin measurement
name1 <- "\\bTROPONIN\\b"

# 2) Platform or assay qualifiers before “troponin”
name2 <- "\\b(?:ISTAT|POCT|ASSAY)\\b.*\\bTROPONIN\\b"

# TODO: this is good 

Troponin_list <- df_lab %>% 
  filter(str_detect(LabPanelName, regex(name1, ignore_case = TRUE)) 
         | str_detect(LabPanelName, regex(name2, ignore_case = TRUE)) 
  ) %>%
  dplyr::distinct(LabPanelName) %>%
  dplyr::pull(LabPanelName)

# Social.planning

# 1) any consult to discharge planning
name1 <- "\\bCONSULT\\b.*\\bDISCHARGE\\s+PLANNING\\b"

# 2) any consult to social work
name2 <- "\\bCONSULT\\b.*\\bSOCIAL\\s+WORK\\b"

# Define the regex for the term to be excluded
exclude_term <- "\\bDISCHARGE\\b"

Social_planning_list <- df_proc %>%
  filter(
    # Initial condition: ProcedureName must match name1 OR name2
    (
      str_detect(ProcedureName, regex(name1, ignore_case = TRUE)) |
        str_detect(ProcedureName, regex(name2, ignore_case = TRUE))
    ) & # AND operator
      # Exclusion condition: ProcedureName must NOT match the exclude_term_discharge
      !str_detect(ProcedureName, regex(exclude_term, ignore_case = TRUE))
  )  %>%
  dplyr::distinct(ProcedureName) %>% 
  dplyr::pull(ProcedureName)

# Discharge.planning

# 1) any OT or PT eval & treatment
name1 <- "\\b(?:OT|PT)\\s+EVAL(?:\\s+AND\\s+TREAT)?\\b"

# 2) any oxygen evaluation
name2 <- "\\bOXYGEN\\s+EVALUATION\\b"

# TODO: add, "DISCHARGE PLANNING"
# 3) any discharge planning
name3 <- "\\bDISCHARGE\\s+PLANNING\\b"

# TODO: no "wound"
# Define the regex for the term to be excluded
exclude_term <- "\\bWOUND\\b"

Discharge_planning_list <- df_proc %>%
  filter(
    # Inclusion criteria: (name1 OR name2 OR name3_discharge_planning)
    (
      str_detect(ProcedureName, regex(name1, ignore_case = TRUE)) |
        str_detect(ProcedureName, regex(name2, ignore_case = TRUE)) |
        str_detect(ProcedureName, regex(name3, ignore_case = TRUE))
    ) & # AND operator for combining inclusion and exclusion
      # Exclusion criterion: ProcedureName must NOT match the exclude_term_wound
      !str_detect(ProcedureName, regex(exclude_term, ignore_case = TRUE))
  ) %>%
  dplyr::distinct(ProcedureName) %>% 
  dplyr::pull(ProcedureName)

# XRAY

# 1) generic blood transfusion
name1 <- "XR(?:ay) CHEST SINGLE"

# 2) red‐cell or packed‐cell transfusion
name2 <- "CHEST SINGLE"

# TODO: this is good 

XRAY_list <- df_proc %>%
  filter(
    str_detect(ProcedureName, regex(name1, ignore_case = TRUE)) |
      str_detect(ProcedureName, regex(name2, ignore_case = TRUE))
  )  %>%
  dplyr::distinct(ProcedureName) %>% 
  dplyr::pull(ProcedureName)

# POINT.OF.CARE

# 1) any POC blood gas
name1 <- "\\bPOC\\b"

# 2) any “point of care” test
name2 <- "\\bPOINT\\s+OF\\s+CARE(?:\\s+TESTS?)?\\b"

# TODO, no "extraction", no "guidance", no "AMB STREP", no "VENOUS ACCESS", no "ARTERIAL ACCESS"

exclusion_regex_list <- c(
  "\\bEXTRACTION\\b",
  "\\bGUIDANCE\\b",
  "\\bAMB\\s+STREP\\b",
  "\\bVENOUS\\s+ACCESS\\b",
  "\\bARTERIAL\\s+ACCESS\\b"
)

# Combine exclusion terms into a single regex pattern using OR (|)
exclude_pattern <- paste(exclusion_regex_list, collapse = "|")

POINT_OF_CARE_list <- df_proc %>%
  filter(
    # Initial inclusion criteria: (name1 OR name2)
    (
      str_detect(ProcedureName, regex(name1, ignore_case = TRUE)) |
        str_detect(ProcedureName, regex(name2, ignore_case = TRUE))
    ) & # AND operator: the following condition must also be true
      # Exclusion criteria: ProcedureName must NOT contain any of the excluded terms
      !str_detect(ProcedureName, regex(exclude_pattern, ignore_case = TRUE))
  ) %>%
  dplyr::distinct(ProcedureName) %>% 
  dplyr::pull(ProcedureName)

# URINE

# 1) any random‐urine test
name1 <- "\\bRANDOM\\s+URINE\\b"

# 2) any of the specific analyte panels in urine
name2 <- "\\b(?:ELECTROLYTES|SODIUM|UREA)\\s+RANDOM\\s+URINE\\b"

URINE_list <- df_lab %>% 
  filter(str_detect(LabPanelName, regex(name1, ignore_case = TRUE)) 
         | str_detect(LabPanelName, regex(name2, ignore_case = TRUE)) 
  ) %>%
  dplyr::distinct(LabPanelName) %>%
  dplyr::pull(LabPanelName)

# dialysis

# 1) generic “dialysis” (catches DIALYSIS, DIALYSIS SOLUTIONS, PR DIALYSIS PROCEDURE, etc.)
name1 <- "\\bDIALYSIS\\b"

# 2) modality‑specific (hemodialysis or peritoneal dialysis)
name2 <- "\\b(?:HEMO|PERITONEAL).*?DIALYSIS\\b"

# TODO: must include, "PERITONEAL" or "insertion" or "acute" or "phys" or "PR DIALYSIS PROCEDURE" or "HEMOSTATS" or "REPEATED" or "PR UNSCHED DIALYSIS ESRD PT HOS "

# must also include one of these keywords
must_include <- "\\b(?:PERITONEAL|INSERTION|ACUTE|PHYS|PR\\s+DIALYSIS\\s+PROCEDURE|HEMOSTATS|REPEATED|PR\\s+UNSCHED\\s+DIALYSIS\\s+ESRD\\s+PT\\s+HOS)\\b"

Dialysis_list <- df_proc %>%
  filter(
    ( str_detect(ProcedureName, regex(name1, ignore_case = TRUE)) |
        str_detect(ProcedureName, regex(name2, ignore_case = TRUE)) )
    &
      str_detect(ProcedureName, regex(must_include, ignore_case = TRUE))
    &
      !str_detect(ProcedureName, regex("removal", ignore_case = TRUE))
  ) %>%
  dplyr::distinct(ProcedureName) %>% 
  dplyr::pull(ProcedureName)

# bloodtrans

# catches “BLOOD TRANSFUSION” OR “PACKED CELL” OR “RED BLOOD CELLS”
name1 <- "\\b(?:BLOOD\\s+TRANSFUSION|PACKED\\s+CELL|RED\\s+BLOOD\\s+CELLS?)\\b"

# catches the standalone “RBC UNIT”
name2 <- "\\bRBC\\s*UNIT\\b"

exclude_pattern <- "\\bPRODUCT(?:S)?\\s+OF\\s+CONCEPTION\\b"

Bloodtrans_list <- df_proc %>%
  filter(
    # Original conditions for detecting relevant procedures
    (
      str_detect(ProcedureName, regex(name1, ignore_case = TRUE)) |
        str_detect(ProcedureName, regex(name2, ignore_case = TRUE))
    ) & # AND operator: both the above OR condition AND the below NOT condition must be true
      # New condition: ProcedureName should NOT contain the exclude_term_poc
      !str_detect(ProcedureName, regex(exclude_pattern, ignore_case = TRUE))
  ) %>%
  dplyr::distinct(ProcedureName) %>% 
  dplyr::pull(ProcedureName)

matched_lists_names <- c("CT_BODY_CHEST_PELV_W_ABD_list", "STRESS_NUCLEAR_list", "POTASSIUM_list", "CENTRAL_line_list",
                           "CDIFFs", "RESPIRATORY_CULTURE_list", "BLOOD_GASES_comb_list",
                           "red_blood_cell_or_whole_blood_transfusion_list", "intubation_list",
                           "Blood_products_list", "ECHO_list", "Troponin_list", "Social_planning_list",
                           "Discharge_planning_list", "XRAY_list", "POINT_OF_CARE_list", "URINE_list",
                           "Dialysis_list", "Bloodtrans_list")

matched_lists <- mget(matched_lists_names) 
saveRDS(matched_lists, file = paste0(fp_processed, "reg_matched_lists.rds"))

###########################################################################
# TODO: the following code is to get Post-op: final_data_store_diag_postop.csv

# Diag <- read.csv(file = "data/C2730_Table4_Diagnosis_20240223.csv")
# #Diag$DiagnosisDate_ad <- as.Date(Diag$DiagnosisDate)
# Epic_data <- ICD10_uni_surg_id_spec

# Epic_data <- Epic_data %>%
#   mutate(arb_person_id = as.integer64(arb_person_id))

Epic_data_r <- Epic_data_r %>%
  mutate(arb_person_id = as.integer64(arb_person_id))

diag_cohort <- Diag[Diag$arb_person_id %in% Epic_data_r$arb_person_id,]

# phe_code <- read.table(file = "data/ICDMAP.txt",header = TRUE)

diag_cohort$ICD <- diag_cohort$DiagnosisCode
phe_code$flag <- as.factor(phe_code$flag)
phe_code[is.na(phe_code$flag)] -> "10"
comp_merge <- left_join(diag_cohort,phe_code,by="ICD")
comp_merge$phecode[is.na(comp_merge$phecode)] -> "Other"
make_dummyvf <- comp_merge[!is.na(comp_merge$phecode),]
need_merge <- make_dummyvf[(make_dummyvf$phecode %in% c(80,1011,501,590,591,1013,
                                                        342,394.7,411.2,411.8,427.12,427.5,429,797,994.21,285,740.1,743.11,800,
                                                        38,480,501,276.13,442,559,585.1,415.11,452,452.2,80,850,851,591,599.3) | 
                              substring(make_dummyvf$phecode,1,3) %in% c(599,592,540,994,480)),]

# inter_Epic <- intersect(unique(Epic_data$arb_person_id),unique(need_merge$arb_person_id))
nsqip_diag_merge <-  full_join(need_merge, Epic_data_r, by = c("arb_person_id"))

nsqip_diag_merge$DiagnosisDate[nsqip_diag_merge$DiagnosisDate==""] <- NA
nsqip_diag_merge$as_date_diag <- as.Date(nsqip_diag_merge$DiagnosisDate)
nsqip_diag_merge$SurgeryDate_asDate <- as.Date(nsqip_diag_merge$SurgeryDate_asDate)
nsqip_diag_merge$length_diag_operation <- nsqip_diag_merge$SurgeryDate_asDate-nsqip_diag_merge$as_date_diag

nsqip_diag_merge$ind_operative_rec[nsqip_diag_merge$length_diag_operation < -30 ] <- 0
nsqip_diag_merge$ind_operative_rec[nsqip_diag_merge$length_diag_operation <= 0 & nsqip_diag_merge$length_diag_operation >= -30 ] <- 1
nsqip_diag_merge$ind_operative_rec[nsqip_diag_merge$length_diag_operation > 0 ] <- 2
nsqip_diag_merge$ind_operative_rec[is.na(nsqip_diag_merge$length_diag_operation)] <- NA

diag_cohort_icd10 <- nsqip_diag_merge[!is.na(nsqip_diag_merge$DiagnosisCodeType),]

make_dummy <- diag_cohort_icd10[diag_cohort_icd10$ind_operative_rec==1 & !is.na(diag_cohort_icd10$ind_operative_rec),]

transpose_icd10 <- make_dummy%>%
  select(SurgeryID, phecode)%>%
  unique()%>%
  mutate(yesno = 1)%>%
  distinct%>%
  spread(phecode, yesno, fill = 0)

store_patient_names_96 <- setdiff(unique(Epic_data$SurgeryID),unique(transpose_icd10$SurgeryID))

final_data <- bind_rows(transpose_icd10,data.frame(SurgeryID = store_patient_names_96))

final_data[is.na(final_data)] <- 0 

saveRDS(final_data, paste0(fp_processed,"final_data_store_diag_postop.rds"))

# glimpse(final_data_store_diag_postop)
#########################################################################
# TODO: ## com score
## TODO: this is to get Pre-op: 2022_0711/combid_pre/com_score_preop.rdata
## 2022_0711/gen_data_preop/comscore_preop_1003_useSurgeryID.R

library(comorbidity)


#Epic_data1 <- Epic_data[,c("arb_person_id","SurgeryID","SurgeryDate_asDate")]

diag_cohort <- Diag[Diag$arb_person_id %in% Epic_data_r$arb_person_id,]

need_merge <- diag_cohort%>%select(arb_person_id,DiagnosisDate,DiagnosisCodeType,DiagnosisCode)
#### indicator for diag ####
nsqip_diag_merge <-  full_join(need_merge, Epic_data_r, by = "arb_person_id")

#save(nsqip_diag_merge,file = "/Users/yaxuzhuang/OneDrive - The University of Colorado Denver/Katie Project/2022_0711/combid_pre/combid_merge_data.Rdata")
#nsqip_diag_merge$as_date_diag <- as.Date(as.character(nsqip_diag_merge$DiagnosisDate))
#View(nsqip_diag_merge$DiagnosisDate)
nsqip_diag_merge$DiagnosisDate[nsqip_diag_merge$DiagnosisDate==""] <- NA
nsqip_diag_merge$as_date_diag <- as.Date(nsqip_diag_merge$DiagnosisDate)
nsqip_diag_merge$SurgeryDate_asDate <- as.Date(nsqip_diag_merge$SurgeryDate_asDate)
#table(nsqip_diag_merge$DiagnosisDate)

nsqip_diag_merge$length_diag_operation <- nsqip_diag_merge$SurgeryDate_asDate-nsqip_diag_merge$as_date_diag

### validation session ends ###

nsqip_diag_merge$ind_rec_yesno[is.na(nsqip_diag_merge$length_diag_operation)] <- 0
nsqip_diag_merge$ind_rec_yesno[!is.na(nsqip_diag_merge$length_diag_operation)] <- 1
# TODO: validation
#sum(table(nsqip_diag_merge$ind_rec_yesno))
#table(nsqip_diag_merge$ind_rec_yesno)

nsqip_diag_merge$ind_rec_30days[nsqip_diag_merge$length_diag_operation <= 0 & nsqip_diag_merge$length_diag_operation >= -30] <- 1
nsqip_diag_merge$ind_rec_30days[nsqip_diag_merge$length_diag_operation < -30 | nsqip_diag_merge$length_diag_operation > 0 ] <- 0


nsqip_diag_merge$ind_operative_rec[nsqip_diag_merge$length_diag_operation < -30 ] <- 0
nsqip_diag_merge$ind_operative_rec[nsqip_diag_merge$length_diag_operation <= 0 & nsqip_diag_merge$length_diag_operation >= -30 ] <- 1
nsqip_diag_merge$ind_operative_rec[nsqip_diag_merge$length_diag_operation > 0 ] <- 2
nsqip_diag_merge$ind_operative_rec[is.na(nsqip_diag_merge$length_diag_operation)] <- NA

nsqip_diag_merge$pre_op_diag <- ifelse(nsqip_diag_merge$length_diag_operation > 0 & nsqip_diag_merge$length_diag_operation <= 365,1,0)

diag_cohort_icd10 <- nsqip_diag_merge[!is.na(nsqip_diag_merge$DiagnosisCodeType),]
#min(nsqip_diag_merge$SurgeryDate_asDate)

ICD10_rec_excl <- diag_cohort_icd10
make_dummy <- diag_cohort_icd10[diag_cohort_icd10$pre_op_diag==1 & !is.na(diag_cohort_icd10$pre_op_diag),]



nsqip_diag_merge$pre_op_diag <- ifelse(nsqip_diag_merge$length_diag_operation > 0 & nsqip_diag_merge$length_diag_operation <= 365,1,0)

#nsqip_diag_merge$ind_icd_type <- ifelse(nsqip_diag_merge$as_date_operation -(as.Date("2016-10-01"))>=0,1,0)
nsqip_diag_merge$ind_icd_type[nsqip_diag_merge$SurgeryDate_asDate -(as.Date("2015-10-01"))<0] <- 0
nsqip_diag_merge$ind_icd_type[nsqip_diag_merge$SurgeryDate_asDate -(as.Date("2015-10-01")) >=0 & nsqip_diag_merge$SurgeryDate_asDate -(as.Date("2016-10-01"))<0] <- 1
nsqip_diag_merge$ind_icd_type[nsqip_diag_merge$SurgeryDate_asDate -(as.Date("2016-10-01"))>=0] <- 2

nsqip_diag_merge365 <- nsqip_diag_merge[nsqip_diag_merge$pre_op_diag==1 & !is.na(nsqip_diag_merge$pre_op_diag),]

convert_icd <- nsqip_diag_merge365[nsqip_diag_merge365$ind_icd_type==1,]
convert_icd_9 <- convert_icd[convert_icd$DiagnosisCodeType=='ICD-9-CM',]
convert_icd_10 <- convert_icd[convert_icd$DiagnosisCodeType=='ICD-10-CM',]
#colnames(ninetoten) <- c("DiagnosisCode","ICD-10-CM","unknow")
# colnames(convert_icd_9)

#merge_convert_icd_9 <- merge(convert_icd_9,ninetoten,by="DiagnosisCode",all.x = TRUE)

#merge_convert_icd_10 <- merge(convert_icd_10,tentonine,by="DiagnosisCode",all.x = TRUE)

ICD10 <- nsqip_diag_merge365[nsqip_diag_merge365$ind_icd_type==1,]

nsqip_diag_merge$ind_icd_type <- ifelse(nsqip_diag_merge$SurgeryDate_asDate -(as.Date("2016-10-01"))>=0,1,0)

nsqip_diag_merge365 <- nsqip_diag_merge[nsqip_diag_merge$pre_op_diag==1 & !is.na(nsqip_diag_merge$pre_op_diag),]

ICD9 <- nsqip_diag_merge365[nsqip_diag_merge365$ind_icd_type==0,]
#save(ICD9,file="/Users/yaxuzhuang/OneDrive - The University of Colorado Denver/Katie Project/2022_0711/combid_pre/ICD9.Rdata")

if(nrow(ICD9) == 0){
  message("There is no ICD9 subject. There is no data in ICD9!")
}

# glimpse(nsqip_diag_merge365)
# glimpse(ICD9)

ICD10 <- nsqip_diag_merge365[nsqip_diag_merge365$ind_icd_type==1,]

# glimpse(ICD10)

icd9_diag <- ICD9[ICD9$DiagnosisCodeType=="ICD-9-CM",]
icd10_diag <- ICD10[ICD10$DiagnosisCodeType=="ICD-10-CM",]

if (nrow(icd9_diag) > 0){
  comorbidity_dat_icd9 <- icd9_diag[order(icd9_diag$SurgeryID),c("SurgeryID","DiagnosisCode")]
  charlson9 <- comorbidity(x = comorbidity_dat_icd9, id = "SurgeryID", code = "DiagnosisCode", map = "charlson_icd9_quan", assign0 = FALSE)
  elixhauser9 <- comorbidity(x = comorbidity_dat_icd9, id = "SurgeryID", code = "DiagnosisCode", map = "elixhauser_icd9_quan", assign0 = FALSE)
}

comorbidity_dat <- icd10_diag[order(icd10_diag$SurgeryID),c("SurgeryID","DiagnosisCode")]
charlson10 <- comorbidity(x = comorbidity_dat, id = "SurgeryID", code = "DiagnosisCode", map = "charlson_icd10_quan", assign0 = FALSE)

# glimpse(charlson10)

elixhauser10 <- comorbidity(x = comorbidity_dat, id = "SurgeryID", code = "DiagnosisCode", map = "elixhauser_icd10_quan", assign0 = FALSE)

if(nrow(icd9_diag) == 0){
  message("There is no ICD9 subject. There is no data in icd9_diag!")
}


if (nrow(icd9_diag) > 0){
  store_having_com <- c(charlson9$SurgeryID,charlson10$SurgeryID)
} else {
  store_having_com <- c(charlson10$SurgeryID)
}

nsqip_diag_rest <- nsqip_diag_merge365[!(nsqip_diag_merge365$SurgeryID %in% store_having_com),]

# glimpse(nsqip_diag_rest)

if (nrow(nsqip_diag_rest) > 0){
  nsqip_diag_rest10 <- nsqip_diag_rest[nsqip_diag_rest$DiagnosisCodeType=="ICD-10-CM",]
  comorbidity_dat_rest10  <- nsqip_diag_rest10[order(nsqip_diag_rest10$SurgeryID),c("SurgeryID","DiagnosisCode")]
  charlson10_rest <- comorbidity(x = comorbidity_dat_rest10, id = "SurgeryID", code = "DiagnosisCode", map = "charlson_icd9_quan", assign0 = FALSE)
}

if (nrow(icd9_diag) > 0){
  charlson9$wscore <- score(charlson9, weights = "charlson", assign0=FALSE)
}

charlson10$wscore <- score(charlson10, weights = "charlson", assign0=FALSE)

if (nrow(nsqip_diag_rest) == 0){
  message("There is no ICD9 subject. There is no data in nsqip_diag_rest!")
}

if (nrow(nsqip_diag_rest) > 0){
  charlson10_rest$wscore <- score(charlson10_rest, weights = "charlson", assign0=FALSE)
}

if ((nrow(nsqip_diag_rest) > 0) & (nrow(icd9_diag) > 0)){
  final_combidity <- rbind(charlson9,charlson10,charlson10_rest)
} else if (nrow(nsqip_diag_rest) > 0){
  final_combidity <- rbind(charlson10,charlson10_rest)
} else if (nrow(icd9_diag) > 0){
  final_combidity <- rbind(charlson9,charlson10)
} else {
  final_combidity <- charlson10
}

final_combidity$windex[final_combidity$wscore==0] <- "0"
final_combidity$windex[final_combidity$wscore==1 |final_combidity$wscore==2] <- "1-2"
final_combidity$windex[final_combidity$wscore>=3] <- ">=3"

final_combidity$windex2[final_combidity$wscore==0] <- "0"
final_combidity$windex2[final_combidity$wscore==1 |final_combidity$wscore==2] <- "1-2"
final_combidity$windex2[final_combidity$wscore==3 |final_combidity$wscore==4] <- "3-4"
final_combidity$windex2[final_combidity$wscore>=5] <- ">=5"

final_combidity2 <- final_combidity

rest_ID <- as.data.frame(Epic_data[!(Epic_data$SurgeryID %in% final_combidity2$SurgeryID),"SurgeryID"])
colnames(rest_ID) <- "SurgeryID"
rest_ID$windex <- 0

final_combidityv3 <- rbind.fill(final_combidity2,rest_ID)

com_score_preop <- final_combidityv3
saveRDS(com_score_preop, paste0(fp_processed,
                                             "com_score_preop.rds"))

# glimpse(com_score_preop)
rm(final_combidityv3, rest_ID, final_combidity2,
   final_combidity, nsqip_diag_rest,
   nsqip_diag_rest10, nsqip_diag_merge)


###########################################################################
# TODO: the following code is to get Pre-op: final_data_store_diag_preop

need_merge <- make_dummyvf[(make_dummyvf$phecode %in% c(80,591,567,994,150,480,501,
                                                        276.12,394.7,411.2,427.4,429.9,444.1,458.2,681.6,720.1,
                                                        797,994.21,150,509.1,740,994.2,251.1,290.2,797,585,585.1,585.2,
                                                        276.1,276.11,276.13,276.4,276.42,281.12,452,452.2,
                                                        281.12,286.2,386.9,415,345.1,386.9,577.2,599.8,285,285.1,285.2,
                                                        285.21,285.22,285.3,285.8,198.3,
                                                        198.6,276,290.1,427.2,737) | 
                              substring(make_dummyvf$phecode,1,3) %in% c(560)),]

nsqip_diag_merge <-  full_join(need_merge,Epic_data_r,by = "arb_person_id")

nsqip_diag_merge$DiagnosisDate[nsqip_diag_merge$DiagnosisDate==""] <- NA
nsqip_diag_merge$as_date_diag <- as.Date(nsqip_diag_merge$DiagnosisDate)
nsqip_diag_merge$SurgeryDate_asDate <- as.Date(nsqip_diag_merge$SurgeryDate_asDate)
nsqip_diag_merge$length_diag_operation <- nsqip_diag_merge$SurgeryDate_asDate-nsqip_diag_merge$as_date_diag

nsqip_diag_merge <- nsqip_diag_merge[!is.na(nsqip_diag_merge$DiagnosisCodeType),]

nsqip_diag_merge$pre_op_diag <- ifelse(nsqip_diag_merge$length_diag_operation > 0 & nsqip_diag_merge$length_diag_operation <= 365,1,0)

make_dummy <- nsqip_diag_merge[nsqip_diag_merge$pre_op_diag==1 & !is.na(nsqip_diag_merge$pre_op_diag),]

transpose_icd10 <- make_dummy%>%
  select(SurgeryID, phecode)%>%
  unique()%>%
  mutate(yesno = 1)%>%
  distinct%>%
  spread(phecode, yesno, fill = 0)

store_patient_names_96 <- setdiff(unique(Epic_data$SurgeryID),unique(transpose_icd10$SurgeryID))

final_data <- bind_rows(transpose_icd10,data.frame(SurgeryID = store_patient_names_96))

final_data[is.na(final_data)] <- 0 

saveRDS(final_data, paste0(fp_processed,
                                             "final_data_store_diag_preop.rds"))

# glimpse(final_data_store_diag_preop)

rm(final_data_store, Diag, final_data, make_dummy, make_dummyvf, 
   make_dummyvf_keep, phe_code, unique_comp_patients, transpose_icd10,
   need_merge, nsqip_diag_merge, first_row_dficd3, diag_cohort,
   diag_cohort_icd10, dficd1, dficd2, comp_merge, comp_patients,
   dummydf1)

#########################################################################
# this is to get final_data_store_lab_postop

#  GET THE DATA file cultureAllInfections.csv
cultures <- read.csv(file = "../data/cultureAllInfections.csv")
# View(cultures)
store_cult_names <- as.factor(cultures$LabPanelName)

lab_cohort_r <- Lab_r[Lab_r$arb_person_id %in% Epic_data_r$arb_person_id,]

# TODO: include more urine culture here 
# lab_cdiff <- Lab %>% dplyr::select(LabPanelName) %>%
#   filter(str_detect(LabPanelName, regex("C[^/]DIFF", ignore_case = TRUE)) |
#            str_detect(LabPanelName, regex("CDIFF", ignore_case = TRUE)) )
# 
# CDIFFs <- table(lab_cdiff) %>% names()

# lab_cohort <- lab_cohort[(lab_cohort$LabPanelName %in% c("AEROBIC CULTURE (EG: TISSUE, ABSCESS, WOUND, SINUS, ETC","URINE CULTURE - RWHS","AFB BLOOD CULTURE - SOUTH","SCREEN, YEAST SCREENING CULTURE",
#                                                          "AEROBIC CULTURE - MHS ONLY","ANAEROBIC CULTURE, (EG:TISSUE, ABSCESS, WOUND, SINUS, ETC.)","ANAEROBIC CULTURE","CHLAMYDIA CULTURE - MHS ONLY",
#                                                          " ANAEROBIC CULTURE - MHS ONLY","BLOOD CULTURE","CULTURE AEROBIC, ROUT, COMPOSITE","BLOOD CULTURE SET 2","BLOOD CULTURE POS WORKUP PEDS",
#                                                          "CULTURE ANAEROBIC","BLOOD CULTURE POSITIVE WORKUP - RWHS","BLOOD CULTURE POS WORKUP HIGH#","CULTURE BLOOD, LAB COLL 1","ANAEROBIC CULTURE - RWHS",
#                                                          "UA DIPSTICK W/ REFLEX TO MICROSCOPIC EXAM IF IND (NO CULTURE REFLEX, EXCEPT EPH)","UA COMPLETE URINALYSIS (NO CULTURE REFLEX)",
#                                                          "UA COMPLETE URINALYSIS W/ CULTURE REFLEX IF INDICATED","URINE CULTURE","URINE CULTURE - MHS ONLY","UA MICROSCOPIC ONLY (NO CULTURE REFLEX,EXCEPT EPH)","CBC NO AUTO DIFF",
#                                   "CBC WITH AUTO DIFF","CBC WITH MANUAL DIFF IF AUTO FAILS (PERFORMABLE)",
#                                   "MAGNESIUM SERUM","ZZMORPHOLOGY","VANCOMYCIN TROUGH","RESPIRATORY CULTURE","QUANTITATIVE RESPIRATORY CULTURE - NORTH/SOUTH ONLY",
#                                   "CULTURE BLOOD, LAB COLL 2","(GEM) BLOOD GASES 4000","ARTERIAL BLOOD GAS","BLOOD GAS ARTERIAL WITH ELECTROLYTES","VENOUS BLOOD GAS",
#                                   "BLOOD CULTURE, FUNGAL (MOLD ONLY)", "BLOOD CULTURE, ISOLATOR BACTERIA", "BLOOD CULTURE, ISOLATOR FUNGUS",
#                                   "FUNGUS CULTURE - BLOOD", "LAB USE ONLY - MRSA/SA BLOOD CULTURE PCR",
#                                   "URINE CULTURE,COMPREHENSIVE", "URINE CULTURE,PRENATAL, W/GBS",
#                                   "LAB USE ONLY - URINE CULTURE, ROUTINE", "LAB USE ONLY - URINE CULTURE RT 997870", 
#                                   "LAB USE ONLY - URINE CULTURE, PRENATAL, W/GBS RESULT")) |
#                           (lab_cohort$LabPanelName %in% as.factor(cultures$LabPanelName)) |
#                            (lab_cohort$LabPanelName %in% CDIFFs) ,]
lab_cohort <- lab_cohort_r[(lab_cohort_r$LabPanelName %in% c("(GEM) BLOOD GASES 4000","ACUTE HEPATITIS PANEL","AEROBIC CULTURE (EG: TISSUE, ABSCESS, WOUND, SINUS, ETC)",
                                                             "VANCOMYCIN TROUGH","AHG CROSSMATCH","ANTIBODY PATIENT INTERPS 1-5","ARTERIAL BLOOD GAS","BASIC METABOLIC PANEL",
                                                             "TEG PANEL","CBC NO AUTO DIFF","POCT CRITICAL PANEL","CONGESTIVE HEART FAILURE BNP","D DIMER QUANTITATIVE","RESPIRATORY CULTURE","VANCOMYCIN TROUGH",
                                                             "CBC NO AUTO DIFF","CKMB PANEL","ELECTROLYTES RANDOM URINE","SODIUM RANDOM URINE","UREA RANDOM URINE",
                                                             "HEMATOCRIT","ISTAT TROPONIN","POCT TROPONIN","TROPONIN I",
                                                             "LACTATE WHOLE BLOOD","NT-PROBNP","PLATELETS UNIT",
                                                             "TEG PANEL",
                                                             "AEROBIC CULTURE (EG: TISSUE, ABSCESS, WOUND, SINUS, ETC","URINE CULTURE - RWHS","AFB BLOOD CULTURE - SOUTH","SCREEN, YEAST SCREENING CULTURE",
                                                             "AEROBIC CULTURE - MHS ONLY","ANAEROBIC CULTURE, (EG:TISSUE, ABSCESS, WOUND, SINUS, ETC.)","ANAEROBIC CULTURE","CHLAMYDIA CULTURE - MHS ONLY",
                                                             " ANAEROBIC CULTURE - MHS ONLY","BLOOD CULTURE","CULTURE AEROBIC, ROUT, COMPOSITE","BLOOD CULTURE SET 2","BLOOD CULTURE POS WORKUP PEDS",
                                                             "CULTURE ANAEROBIC","BLOOD CULTURE POSITIVE WORKUP - RWHS","BLOOD CULTURE POS WORKUP HIGH#","CULTURE BLOOD, LAB COLL 1","ANAEROBIC CULTURE - RWHS",
                                                             "UA DIPSTICK W/ REFLEX TO MICROSCOPIC EXAM IF IND (NO CULTURE REFLEX, EXCEPT EPH)","UA COMPLETE URINALYSIS (NO CULTURE REFLEX)",
                                                             "UA COMPLETE URINALYSIS W/ CULTURE REFLEX IF INDICATED","URINE CULTURE","URINE CULTURE - MHS ONLY","UA MICROSCOPIC ONLY (NO CULTURE REFLEX,EXCEPT EPH)","CBC NO AUTO DIFF",
                                                             "CBC WITH AUTO DIFF","CBC WITH MANUAL DIFF IF AUTO FAILS (PERFORMABLE)",
                                                             "CDIFFICILE TOXIN PCR","C DIFFICILE BY PCR","MAGNESIUM SERUM","ZZMORPHOLOGY","VANCOMYCIN TROUGH","RESPIRATORY CULTURE","QUANTITATIVE RESPIRATORY CULTURE - NORTH/SOUTH ONLY",
                                                             "CULTURE BLOOD, LAB COLL 2","(GEM) BLOOD GASES 4000","ARTERIAL BLOOD GAS","BLOOD GAS ARTERIAL WITH ELECTROLYTES","VENOUS BLOOD GAS",
                                                             "POC(EPOC) ABG","POINT OF CARE TESTS",
                                                             "POCT GLUCOSE CPT 82962 INTERFACED RESULT/DOCKED DEVICE",
                                                             "RENAL FUNCTION PANEL","VANCOMYCIN RANDOM",
                                                             # pasted your names here
                                                             "AEROBIC CULTURE (EG: TISSUE, ABSCESS, WOUND, SINUS, ETC","URINE CULTURE - RWHS","AFB BLOOD CULTURE - SOUTH","SCREEN, YEAST SCREENING CULTURE",
                                                             "AEROBIC CULTURE - MHS ONLY","ANAEROBIC CULTURE, (EG:TISSUE, ABSCESS, WOUND, SINUS, ETC.)","ANAEROBIC CULTURE","CHLAMYDIA CULTURE - MHS ONLY",
                                                             " ANAEROBIC CULTURE - MHS ONLY","BLOOD CULTURE","CULTURE AEROBIC, ROUT, COMPOSITE","BLOOD CULTURE SET 2","BLOOD CULTURE POS WORKUP PEDS",
                                                             "CULTURE ANAEROBIC","BLOOD CULTURE POSITIVE WORKUP - RWHS","BLOOD CULTURE POS WORKUP HIGH#","CULTURE BLOOD, LAB COLL 1","ANAEROBIC CULTURE - RWHS",
                                                             "UA DIPSTICK W/ REFLEX TO MICROSCOPIC EXAM IF IND (NO CULTURE REFLEX, EXCEPT EPH)","UA COMPLETE URINALYSIS (NO CULTURE REFLEX)",
                                                             "UA COMPLETE URINALYSIS W/ CULTURE REFLEX IF INDICATED","URINE CULTURE","URINE CULTURE - MHS ONLY","UA MICROSCOPIC ONLY (NO CULTURE REFLEX,EXCEPT EPH)","CBC NO AUTO DIFF",
                                                             "CBC WITH AUTO DIFF","CBC WITH MANUAL DIFF IF AUTO FAILS (PERFORMABLE)",
                                                             "MAGNESIUM SERUM","ZZMORPHOLOGY","VANCOMYCIN TROUGH","RESPIRATORY CULTURE","QUANTITATIVE RESPIRATORY CULTURE - NORTH/SOUTH ONLY",
                                                             "CULTURE BLOOD, LAB COLL 2","(GEM) BLOOD GASES 4000","ARTERIAL BLOOD GAS","BLOOD GAS ARTERIAL WITH ELECTROLYTES","VENOUS BLOOD GAS",
                                                             "BLOOD CULTURE, FUNGAL (MOLD ONLY)", "BLOOD CULTURE, ISOLATOR BACTERIA", "BLOOD CULTURE, ISOLATOR FUNGUS",
                                                             "FUNGUS CULTURE - BLOOD", "LAB USE ONLY - MRSA/SA BLOOD CULTURE PCR",
                                                             "URINE CULTURE,COMPREHENSIVE", "URINE CULTURE,PRENATAL, W/GBS",
                                                             "PREPARE PLATELETS FOR TRANSFUSION", 
                                                             "LAB USE ONLY - URINE CULTURE, ROUTINE", "LAB USE ONLY - URINE CULTURE RT 997870", 
                                                             "LAB USE ONLY - URINE CULTURE, PRENATAL, W/GBS RESULT")) | 
                             (lab_cohort_r$LabPanelName %in% as.factor(cultures$LabPanelName))|
                             (lab_cohort_r$LabPanelName %in% CDIFFs)|
                             (lab_cohort_r$LabPanelName %in% RESPIRATORY_CULTURE_list)|
                             (lab_cohort_r$LabPanelName %in% BLOOD_GASES_comb_list)|
                             (lab_cohort_r$LabPanelName %in% Blood_products_list)|
                             (lab_cohort_r$LabPanelName %in% Troponin_list)|
                             (lab_cohort_r$LabPanelName %in% URINE_list),]



# glimpse(lab_cohort)

# a <- unique(lab_cohort$LabPanelName)

# nsqip_lab_merge <-  merge(lab_cohort, Epic_data, by = c("arb_person_id", "arb_encounter_id"), all = TRUE)
nsqip_lab_merge <-  full_join(lab_cohort, Epic_data_r, by = c("arb_person_id"))

nsqip_lab_merge$LabCollectedDate[nsqip_lab_merge$LabCollectedDate==""] <- NA
nsqip_lab_merge$as_date_lab <- as.Date(nsqip_lab_merge$LabCollectedDate)
nsqip_lab_merge$SurgeryDate_asDate <- as.Date(nsqip_lab_merge$SurgeryDate_asDate)
nsqip_lab_merge$length_lab_operation <- nsqip_lab_merge$SurgeryDate_asDate-nsqip_lab_merge$as_date_lab

nsqip_lab_merge$ind_operative_rec[nsqip_lab_merge$length_lab_operation < -30 ] <- 0
nsqip_lab_merge$ind_operative_rec[nsqip_lab_merge$length_lab_operation <= 0 & nsqip_lab_merge$length_lab_operation >= -30 ] <- 1
nsqip_lab_merge$ind_operative_rec[nsqip_lab_merge$length_lab_operation > 0 ] <- 2
nsqip_lab_merge$ind_operative_rec[is.na(nsqip_lab_merge$length_lab_operation)] <- NA

make_dummy <- nsqip_lab_merge[nsqip_lab_merge$ind_operative_rec==1 & !is.na(nsqip_lab_merge$ind_operative_rec),]

transpose_icd10 <- make_dummy%>%
  select(SurgeryID, LabPanelName)%>%
  unique()%>%
  mutate(yesno = 1)%>%
  distinct%>%
  spread(LabPanelName, yesno, fill = 0)

store_patient_names_96 <- setdiff(unique(Epic_data$SurgeryID),unique(transpose_icd10$SurgeryID))

final_data <- bind_rows(transpose_icd10,data.frame(SurgeryID = store_patient_names_96))

final_data[is.na(final_data)] <- 0 

saveRDS(final_data, paste0(fp_processed,
                                       "final_data_store_lab_postop.rds"))

#########################################################################
# TODO: final_data_store_preop_lab

# TODO: include more Blood culture

lab_cohort <- lab_cohort_r[(lab_cohort_r$LabPanelName %in% c("AEROBIC CULTURE (EG: TISSUE, ABSCESS, WOUND, SINUS, ETC",
                                "AEROBIC CULTURE - MHS ONLY", "AFB BLOOD CULTURE - SOUTH", "ANAEROBIC CULTURE, (EG:TISSUE, ABSCESS, WOUND, SINUS, ETC.)","ANAEROBIC CULTURE",
                    " ANAEROBIC CULTURE - MHS ONLY","BLOOD CULTURE","CULTURE AEROBIC, ROUT, COMPOSITE","BLOOD CULTURE SET 2","BLOOD CULTURE POS WORKUP PEDS",
                          "CULTURE ANAEROBIC","BLOOD CULTURE POSITIVE WORKUP - RWHS","BLOOD CULTURE POS WORKUP HIGH#","CULTURE BLOOD, LAB COLL 1","ANAEROBIC CULTURE - RWHS",
                    "BLOOD CULTURE, FUNGAL (MOLD ONLY)", "BLOOD CULTURE, ISOLATOR BACTERIA", "BLOOD CULTURE, ISOLATOR FUNGUS",
                    "FUNGUS CULTURE - BLOOD", "LAB USE ONLY - MRSA/SA BLOOD CULTURE PCR",
                    # pasted my here
                    "AEROBIC CULTURE (EG: TISSUE, ABSCESS, WOUND, SINUS, ETC",
                    "AEROBIC CULTURE - MHS ONLY","ANAEROBIC CULTURE, (EG:TISSUE, ABSCESS, WOUND, SINUS, ETC.)","ANAEROBIC CULTURE",
                    " ANAEROBIC CULTURE - MHS ONLY","BLOOD CULTURE","CULTURE AEROBIC, ROUT, COMPOSITE","BLOOD CULTURE SET 2","BLOOD CULTURE POS WORKUP PEDS",
                    "CULTURE ANAEROBIC","BLOOD CULTURE POSITIVE WORKUP - RWHS","BLOOD CULTURE POS WORKUP HIGH#","CULTURE BLOOD, LAB COLL 1","ANAEROBIC CULTURE - RWHS",
                    "ANTIBODY PATIENT INTERPS 1-5","POTASSIUM SERUM/PLASMA","PR ASSAY OF SERUM POTASSIUM","I STAT POTASSIUM",
                    "POCT POTASSIUM","POTASSIUM WHOLE BLOOD",
                    "ANTI-OBESITY - ANOREXIC AGENTS", "ANTIHYPERTENSIVES, ACE INHIBITORS", "FOLIC ACID PREPARATIONS", 
                    "NOSE PREPARATIONS, VASOCONSTRICTORS(OTC)"))|
                    (lab_cohort_r$LabPanelName %in% POTASSIUM_list),]


#a <- unique(lab_cohort$LabPanelName)

#### indicator for diag ####
nsqip_lab_merge <-  full_join(lab_cohort, Epic_data_r, by = c("arb_person_id"))


#save(nsqip_lab_merge,file = "/Users/yaxuzhuang/OneDrive - The University of Colorado Denver/Katie Project/2022_0711/preop_lab/nsqip_lab_merge.Rdata")

nsqip_lab_merge$LabCollectedDate[nsqip_lab_merge$LabCollectedDate==""] <- NA
nsqip_lab_merge$as_date_lab <- as.Date(nsqip_lab_merge$LabCollectedDate)

nsqip_lab_merge$SurgeryDate_asDate <- as.Date(nsqip_lab_merge$SurgeryDate_asDate)
nsqip_lab_merge$length_lab_operation <- nsqip_lab_merge$SurgeryDate_asDate-nsqip_lab_merge$as_date_lab

nsqip_lab_merge$pre_op_lab <- ifelse(nsqip_lab_merge$length_lab_operation > 0 & nsqip_lab_merge$length_lab_operation <= 365,1,0)

make_dummy <- nsqip_lab_merge[nsqip_lab_merge$pre_op_lab==1 & !is.na(nsqip_lab_merge$pre_op_lab),]
# saveRDS(make_dummy, "data/make_dummy.rds")
# lab_panel2 <- make_dummy[make_dummy$LabPanelName=="ZZMORPHOLOGY",]

transpose_icd10 <- make_dummy%>%
  select(SurgeryID, LabPanelName)%>%
  unique()%>%
  mutate(yesno = 1)%>%
  distinct%>%
  spread(LabPanelName, yesno, fill = 0)

store_patient_names_96 <- setdiff(unique(Epic_data$SurgeryID),unique(transpose_icd10$SurgeryID))

final_data <- bind_rows(transpose_icd10,data.frame(SurgeryID = store_patient_names_96))

final_data[is.na(final_data)] <- 0 

saveRDS(final_data, paste0(fp_processed,
                                            "final_data_store_preop_lab.rds"))

rm(Lab_p1, Lab_p2, final_data, comp_patients, unique_comp_patients, transpose_icd10,
   first_row_dficd3, dficd1, dficd2,
   make_dummy_keep_lbNames, dummydf1, make_dummy, df3, nsqip_lab_merge,
   include_pat, lab_panel2)

#################################################################################
## TODO: final_data_store_medi_postop
# MEDI post op are seperated into infectious and non-infectious
# Because they use different class TherapeuticClass for inf, PharmaceuticalClass for non-inf

# Medi <- read.csv(file = "data/C2730_Table7_Medications_20240223.csv")

#write.csv(unique(Medi$arb_person_id),file="/Users/yaxuzhuang/OneDrive - The University of Colorado Denver/Katie Project/2022_0711/medi_postop/Medi_all_population.csv")
#write.csv(unique(Epic_data$arb_person_id),file="/Users/yaxuzhuang/OneDrive - The University of Colorado Denver/Katie Project/2022_0711/medi_postop/Surgery_all_population.csv")

#check_diff <- setdiff(unique(Epic_data$arb_person_id),unique(Medi$arb_person_id))


medi_cohort_r <- Medi_r[Medi_r$arb_person_id %in% Epic_data_r$arb_person_id,]

medi_cohort <- medi_cohort_r[medi_cohort_r$TherapeuticClass=="ANTIBIOTICS",]

# dim(medi_cohort)

nsqip_medi_merge <- full_join(medi_cohort,Epic_data_r,by = "arb_person_id")

### Validation ###
# 
# 
# dim_epic <- Epic_data[Epic_data$arb_person_id==1590586, ]
# dim_medi <- medi_cohort[medi_cohort$arb_person_id==1590586, ]
# dim_nsqip_medi_merge <- nsqip_medi_merge[nsqip_medi_merge$arb_person_id==1590586, ]


nsqip_medi_merge$SurgeryDate_asDate <- as.Date(nsqip_medi_merge$SurgeryDate_asDate)
nsqip_medi_merge$as_date_med <- as.Date(nsqip_medi_merge$OrderedDate)

nsqip_medi_merge$length_med_operation <- nsqip_medi_merge$SurgeryDate_asDate-nsqip_medi_merge$as_date_med
#View(nsqip_medi_merge[,c("as_date_operation","as_date_med","length_med_operation")])

# range(nsqip_medi_merge$length_med_operation, na.rm = TRUE)

nsqip_medi_merge$ind_operative_rec[nsqip_medi_merge$length_med_operation < -30 ] <- 0
nsqip_medi_merge$ind_operative_rec[nsqip_medi_merge$length_med_operation <= -2 & nsqip_medi_merge$length_med_operation >= -30 ] <- 1
nsqip_medi_merge$ind_operative_rec[nsqip_medi_merge$length_med_operation > -2 ] <- 2
nsqip_medi_merge$ind_operative_rec[is.na(nsqip_medi_merge$length_med_operation)] <- NA

# table(nsqip_medi_merge$ind_operative_rec, useNA = "ifany")

make_dummy <- nsqip_medi_merge[nsqip_medi_merge$ind_operative_rec==1 & !is.na(nsqip_medi_merge$ind_operative_rec),]
make_dummy$AntiBiotics_YN <- 1

transpose_icd10 <- make_dummy[!duplicated(make_dummy$SurgeryID),]

store_patient_names_96 <- setdiff(unique(Epic_data$SurgeryID),unique(transpose_icd10$SurgeryID))

comp_patients <- as.data.frame(cbind(store_patient_names_96,0))
colnames(comp_patients) <- c("SurgeryID","AntiBiotics_YN")

final_data <- rbind.fill(transpose_icd10,comp_patients)
final_data$AntiBiotics_YN[is.na(final_data$AntiBiotics_YN)] <- 0 

final_data_store_medi_postop <- final_data[,c("SurgeryID","AntiBiotics_YN")]

saveRDS(final_data_store_medi_postop, paste0(fp_processed,
                                           "final_data_store_medi_postop.rds"))

#write.csv(final_data_store,file="/Users/yaxuzhuang/OneDrive - The University of Colorado Denver/Katie Project/2022_0711/medi_postop/final_data_store_medi_postop.csv")
rm(final_data)

###############################################################################
## TODO: final_data_store_medi_postop_other_comp
medi_cohort <- medi_cohort_r[medi_cohort_r$PharmaceuticalClass %in% c("CALCIUM REPLACEMENT","PLASMA EXPANDERS","ADRENERGIC AGENTS,CATECHOLAMINES","ANTICOAGULANTS,COUMARIN TYPE","ANTIDIURETIC AND VASOPRESSOR HORMONES","ANTIEMETIC/ANTIVERTIGO AGENTS","BETA-ADRENERGIC AGENTS","BICARBONATE PRODUCING/CONTAINING AGENTS","ERYTHROPOIESIS-STIMULATING AGENTS",
                                "EXPECTORANTS","FOLIC ACID PREPARATIONS","GENERAL ANESTHETICS,INJECTABLE-BENZODIAZEPINE TYPE","GLUCOCORTICOIDS","HEPARIN AND RELATED PREPARATIONS","IRON REPLACEMENT","LAXATIVES, LOCAL/RECTAL",
                                "LOOP DIURETICS","NSAIDS, CYCLOOXYGENASE INHIBITOR - TYPE ANALGESICS","TOPICAL ANTIFUNGALS","ABSORBABLE SULFONAMIDE ANTIBACTERIAL AGENTS","NITROFURAN DERIVATIVES ANTIBACTERIAL AGENTS","PENICILLIN ANTIBIOTICS","QUINOLONE ANTIBIOTICS","DIALYSIS SOLUTIONS","POC(EPOC) ABG","POINT OF CARE TESTS",
                                "POCT GLUCOSE CPT 82962 INTERFACED RESULT/DOCKED DEVICE",
                                "RENAL FUNCTION PANEL","VANCOMYCIN RANDOM"),]

nsqip_medi_merge <- full_join(medi_cohort, Epic_data_r, by = "arb_person_id")

nsqip_medi_merge$SurgeryDate_asDate <- as.Date(nsqip_medi_merge$SurgeryDate_asDate)
nsqip_medi_merge$as_date_med <- as.Date(nsqip_medi_merge$OrderedDate)

nsqip_medi_merge$length_med_operation <- nsqip_medi_merge$SurgeryDate_asDate-nsqip_medi_merge$as_date_med

# range(nsqip_medi_merge$length_med_operation, na.rm = TRUE)

nsqip_medi_merge$ind_operative_rec[nsqip_medi_merge$length_med_operation < -30 ] <- 0
nsqip_medi_merge$ind_operative_rec[nsqip_medi_merge$length_med_operation <= 0 & nsqip_medi_merge$length_med_operation >= -30 ] <- 1
nsqip_medi_merge$ind_operative_rec[nsqip_medi_merge$length_med_operation > 0 ] <- 2
nsqip_medi_merge$ind_operative_rec[is.na(nsqip_medi_merge$length_med_operation)] <- NA

# table(nsqip_medi_merge$ind_operative_rec, useNA = "ifany")

make_dummy <- nsqip_medi_merge[nsqip_medi_merge$ind_operative_rec==1 & !is.na(nsqip_medi_merge$ind_operative_rec),]
# dim(make_dummy)
# saveRDS(make_dummy, "data/make_dummy_med.rds")
transpose_icd10 <- make_dummy%>%
  select(SurgeryID, PharmaceuticalClass)%>%
  unique()%>%
  mutate(yesno = 1)%>%
  distinct%>%
  spread(PharmaceuticalClass, yesno, fill = 0)

store_patient_names_96 <- setdiff(unique(Epic_data$SurgeryID),unique(transpose_icd10$SurgeryID))

final_data <- bind_rows(transpose_icd10,data.frame(SurgeryID = store_patient_names_96))

final_data[is.na(final_data)] <- 0 

saveRDS(final_data, paste0(fp_processed,
                                             "final_data_store_medi_postop_other_comp.rds"))

###############################################################################
## TODO: final_data_store_medi_preop

#medi_cohort <- Medi[Medi$arb_person_id %in% Epic_data$arb_person_id,]
#medi_cohort <- medi_cohort_r[(medi_cohort_r$PharmaceuticalClass=="URINARY TRACT ANTISPASMODIC/ANTIINCONTINENCE AGENT"),]
# dim(medi_cohort)
medi_cohort <- medi_cohort_r[(medi_cohort_r$PharmaceuticalClass%in%c("URINARY TRACT ANTISPASMODIC/ANTIINCONTINENCE AGENT",
                                                                     "ANTI-OBESITY - ANOREXIC AGENTS",
                                                                     "ANTIHYPERTENSIVES, ACE INHIBITORS",
                                                                     "FOLIC ACID PREPARATIONS",
                                                                     "NOSE PREPARATIONS, VASOCONSTRICTORS(OTC)",
                                                                     "ANTI-OBESITY - ANOREXIC AGENTS", "ANTIHYPERTENSIVES, ACE INHIBITORS", 
                                                                     "FOLIC ACID PREPARATIONS", "NOSE PREPARATIONS, VASOCONSTRICTORS(OTC)")),]

nsqip_medi_merge <- full_join(medi_cohort, Epic_data_r, by = "arb_person_id")

nsqip_medi_merge$as_date_med <- as.Date(nsqip_medi_merge$OrderedDate)
nsqip_medi_merge$SurgeryDate_asDate <- as.Date(nsqip_medi_merge$SurgeryDate_asDate)

nsqip_medi_merge$length_med_operation <- nsqip_medi_merge$SurgeryDate_asDate-nsqip_medi_merge$as_date_med
#View(nsqip_medi_merge[,c("as_date_operation","as_date_med","length_med_operation")])

# range(nsqip_medi_merge$length_med_operation, na.rm = TRUE)

nsqip_medi_merge$pre_op_med <- ifelse(nsqip_medi_merge$length_med_operation > 0 & nsqip_medi_merge$length_med_operation <= 365,1,0)

# sum(nsqip_medi_merge$pre_op_med == 1, na.rm = TRUE)
make_dummy <- nsqip_medi_merge[nsqip_medi_merge$pre_op_med==1 & !is.na(nsqip_medi_merge$pre_op_med),]
# dim(make_dummy)
#make_dummy <- include_pat[include_pat$ind_operative_rec==1 & !is.na(include_pat$ind_operative_rec),]

transpose_icd10 <- make_dummy%>%
  select(SurgeryID, PharmaceuticalClass)%>%
  unique()%>%
  mutate(yesno = 1)%>%
  distinct%>%
  spread(PharmaceuticalClass, yesno, fill = 0)

store_patient_names_96 <- setdiff(unique(Epic_data$SurgeryID),unique(transpose_icd10$SurgeryID))

final_data <- bind_rows(transpose_icd10,data.frame(SurgeryID = store_patient_names_96))

final_data[is.na(final_data)] <- 0 

saveRDS(final_data, paste0(fp_processed,
                                             "final_data_store_medi_preop.rds"))

rm(Medi, final_data)

####################################################################################
## TODO: final_data_store_proc_postop

proc_cohort_r <- Proc_r[Proc_r$arb_person_id %in% Epic_data_r$arb_person_id,]
proc_cohort <- proc_cohort_r[(proc_cohort_r$ProcedureName %in% c("CARD DX ECHO COMPLETE TTE TRANSTHORACIC STANDARD",
                                                                 "CARD DX ECHO LIMITED TTE 2D TRANSTHORACIC FOLLOW UP FOCUSED EXAM","CARD DX ECHO TRANSESOPHAGEAL TEE",
                                                                 "DOBUTAMINE STRESS ECHO PROTOCOL","ECHO ADULT COMPLETE TTE","ECHO ADULT LIMITED TTE","ECHO TRANSESOPHAGEAL TEE",
                                                                 "ECHO WITH DEFINITY CONTRAST PROTOCOL","OS CV ECHO COMPARE-NO READ","PR DOPPLER ECHO HEART,LIMITED,F/U","PR ECHO HEART XTHORACIC,COMPLETE W DOPPLER",
                                                                 "PR ECHO HEART XTHORACIC,LIMITED","PR ECHO TRANSESOPHAG R-T 2D W/PRB IMG ACQUISJ I&R","PR ECHO TTHRC R-T 2D W/WO M-MODE REST&STRS CONT ECG",
                                                                 "STRESS ECHO - UCH","STRESS ECHO DOBUTAMINE","Change Endotracheal Airway in Trachea, External Approach","",
                                                                 "INSERT ENDOTRACHEAL TUBE","Insertion of Endotracheal Airway into Trachea, Via Natural or Artificial Opening",
                                                                 "Insertion of Endotracheal Airway into Trachea, Via Natural or Artificial Opening Endoscopic","PR ASSAY OF TROPONIN, QUANT",
                                                                 ".CT ABD/PELVIS WWO CONTRAST UROGRAM","ABN ONLY CT ABD & PELVIS W/O CONTRAST","ABN ONLY CT ABD&PELV 1+ SECTION/REGNS","CT ABD W/WO, PELVIS WITH CONT",
                                                                 "CT ABD/PELVIS W CONTRAST","CT ABD/PELVIS W/WO CONTRAST","CT ABD/PELVIS WITHOUT CONTRAST","CT ANGIO CHEST/ABD/PELVIS",
                                                                 "CT CHEST/ABD W CONTRAST","CT CHEST/ABD W/WO CONTRAST","CT CHEST/ABD WO CONTRAST","CT CHEST/ABD/PELV W CONTRAST",
                                                                 "CT CHEST/ABD/PELV W/WO CONTRAST","CT CHEST/ABD/PELV WO CONTRAST","CT CHEST/PELV W, ABD W/WO CONTRAST","PR CT ANGIO ABD&PLVIS CNTRST MTRL W/WO CNTRST IMGES",
                                                                 "PR CT ANGIO, ABD, COMBO,INCL IMAGE PROC","CT ABDOMEN PELVIS LIVER MASS W/WO CONTRAST","CT ANGIO ABDOMEN/PELVIS - BODY",
                                                                 "CT ANGIOGRAPHY PELVIS","CT CHEST ABDOMEN W/WO PELVIS W CONTRAST","CT CTA PULMONARY, ABDOMEN W, PELVIS W","CT PELVIS W/O CONTRAST",
                                                                 "CT PELVIS W/O CONTRAST-MUSC","CT PELVIS W/WO CONTRAST","CT PELVIS WITH CONTRAST","CT PELVIS WITH CONTRAST-MUSC","HEMODIALYSIS","INSERTION PERITONEAL DIALYSIS CATHETER",
                                                                 "PERITONEAL DIALYSIS","PR DIALYSIS PROCEDURE","PR HEMODIALYSIS PROCEDURE W/ PHYS/QHP EVALUATION","PR UNSCHED DIALYSIS ESRD PT HOS","CPAP OVERNIGHT",
                                                                 "INSERT ARTERIAL LINE","VTE PLATELET MONITORING FOR IV UFH PATIENTS","XR CHEST 2 VIEW (PA,LAT)","PR ASSAY OF TROPONIN, QUANT","ADMIT PATIENT",
                                                                 "FEMORAL NERVE BLOCK","IP CONSULT TO DISCHARGE PLANNING","IP CONSULT TO SOCIAL WORK","OT EVAL AND TREAT","PT EVAL AND TREAT",
                                                                 "RT HOME OXYGEN EVALUATION (INPATIENT ORDER)","PR INJ, PROPOFOL, 10 MG","RETURN PATIENT",".XR CHEST SINGLE (AP)","XR CHEST SINGLE VIEW",
                                                                 "Change Endotracheal Airway in Trachea, External Approach","INSERT ENDOTRACHEAL TUBE","Insertion of Endotracheal Airway into Trachea, Via Natural or Artificial Opening",
                                                                 "Insertion of Endotracheal Airway into Trachea, Via Natural or Artificial Opening Endoscopic","RESTRAINTS NON-BEHAVIORAL","TRANSFER PATIENT","IP CONSULT TO PHARMACY - TPN",
                                                                 "IP CONSULT TO WOUND / OSTOMY / SKIN TEAM","US RENAL (KIDNEYS/BLADDER ONLY)","US UPPER EXTREMITY VENOUS BIL","WEIGH PATIENT",".XR CHEST SINGLE (PA)",
                                                                 "CTA CHEST FOR PE","OPN RT HEMICOLECTOMY NEC","PR INJ ENOXAPARIN SODIUM 10 MG","US UPPER EXTREMITY VENOUS UNILATERAL","PREADMISSION ADMIT ORDER - RN TO RELEASE",
                                                                 "UPDATE PATIENT CLASS I.E. INPATIENT / OBSERVATION","INCISION AND DRAINAGE HEAD/NECK","LAST TYPE & SCREEN (CLOT) STATUS","PR BACTERIA IDENTIFICATION, AEROBIC ISOLATE",
                                                                 "ED BLOOD TRANSFUSION PROCEDURE","PACKED CELL TRANSFUSION","PLATELET TRANSFUSION","SERUM TRANSFUSION NEC","TRANSFUSION NEC","COAG FACTOR TRANSFUSION","PR BLOOD TRANSFUSION SERVICE",
                                                                 "AMBULATORY REFERRAL OIC BLOOD AND/OR BLOOD PRODUCT TRANSFUSION","NURSE INSTRUCTIONS TRANSFUSION REACTION OIC","BLOOD TRANSFUSION: USE ORDER SET ONLY. IF EMERGENT, CALL BLOOD BANK.",
                                                                 "MASSIVE TRANSFUSION PROTOCOL","PR CHG TRANSFUSION PROCEDURE","PR EXCHANGE TRANSFUSION, OTHR","TRANSFUSE RED BLOOD CELLS","TRANSFUSE CRYOPRECIPITATE",
                                                                 "TRANSFUSE PLATELETS","TRANSFUSE WHOLE BLOOD","TRANSFUSE RED BLOOD CELLS (IN ML)","TRANSFUSE PLASMA (IN ML)","TRANSFUSE CRYOPRECIPITATE (IN ML)",
                                                                 "Transfusion of Nonautologous Red Blood Cells into Peripheral Vein, Percutaneous Approach","PARTIAL HIP REPLACEMENT", "PR PARTIAL HIP REPLACEMENT", 
                                                                 "PREPARE PLATELETS FOR TRANSFUSION", 
                                                                 "RBC UNIT",
                                                                 "POC(EPOC) ABG","POINT OF CARE TESTS",
                                                                 "POCT GLUCOSE CPT 82962 INTERFACED RESULT/DOCKED DEVICE",
                                                                 "RENAL FUNCTION PANEL","VANCOMYCIN RANDOM"))|
                               (proc_cohort_r$ProcedureName %in% red_blood_cell_or_whole_blood_transfusion_list)|
                               (proc_cohort_r$ProcedureName %in% intubation_list)|
                               (proc_cohort_r$ProcedureName %in% ECHO_list)|
                               (proc_cohort_r$ProcedureName %in% Social_planning_list)|
                               (proc_cohort_r$ProcedureName %in% Discharge_planning_list)|
                               (proc_cohort_r$ProcedureName %in% XRAY_list)|
                               (proc_cohort_r$ProcedureName %in% POINT_OF_CARE_list)|
                               (proc_cohort_r$ProcedureName %in% Dialysis_list)|
                               (proc_cohort_r$ProcedureName %in% Bloodtrans_list),] 


#proc_cohort[proc_cohort$ProcedureName == "ED BLOOD TRANSFUSION PROCEDURE",]
nsqip_proc_merge <- full_join(proc_cohort, Epic_data_r, by = "arb_person_id")

nsqip_proc_merge$ProcedureEventDate[nsqip_proc_merge$ProcedureEventDate==""] <- NA
nsqip_proc_merge$as_date_proc <- as.Date(nsqip_proc_merge$ProcedureEventDate)
nsqip_proc_merge$SurgeryDate_asDate <- as.Date(nsqip_proc_merge$SurgeryDate_asDate)


nsqip_proc_merge$length_proc_operation <- nsqip_proc_merge$SurgeryDate_asDate-nsqip_proc_merge$as_date_proc
#View(nsqip_proc_merge[,c("ProcedureDate","as_date_proc","as_date_operation","length_proc_operation")])

nsqip_proc_merge$ind_operative_rec[nsqip_proc_merge$length_proc_operation < -30 ] <- 0
nsqip_proc_merge$ind_operative_rec[nsqip_proc_merge$length_proc_operation <= 0 & nsqip_proc_merge$length_proc_operation >= -30 ] <- 1
nsqip_proc_merge$ind_operative_rec[nsqip_proc_merge$length_proc_operation > 0 ] <- 2
nsqip_proc_merge$ind_operative_rec[is.na(nsqip_proc_merge$length_proc_operation)] <- NA

make_dummy <- nsqip_proc_merge[nsqip_proc_merge$ind_operative_rec==1 & !is.na(nsqip_proc_merge$ind_operative_rec),]

transpose_icd10 <- make_dummy%>%
  select(SurgeryID, ProcedureName)%>%
  unique()%>%
  mutate(yesno = 1)%>%
  distinct%>%
  filter(!(ProcedureName == ""))%>%
  spread(key = ProcedureName, value = yesno, fill = 0)

store_patient_names_96 <- setdiff(unique(Epic_data$SurgeryID),unique(transpose_icd10$SurgeryID))

final_data <- bind_rows(transpose_icd10,data.frame(SurgeryID = store_patient_names_96))

final_data[is.na(final_data)] <- 0 

saveRDS(final_data, paste0(fp_processed,
                                                 "final_data_store_proc_postop.rds"))

####################################################################################
## TODO: final_data_store_proc_preop
# Proc <- read.csv(file = "data/C2730_Table5_Procedures_20240223.csv")

proc_cohort <- proc_cohort_r[(proc_cohort_r$ProcedureName %in% c("OS CT BODY COMPARE-NO READ",
                                                                 "OS CT BODY COMPARE-NO READ","CT CHEST/PELV W, ABD W/WO CONTRAST",".PET/CT WHOLE BODY NO IV CONTRAST",
                                                                 "ABN ONLY CT ABD & PELVIS W/O CONTRAST","ABN ONLY CT ABD&PELV 1+ SECTION/REGNS","Computerized Tomography (CT Scan) of Abdomen and Pelvis using Low Osmolar Contrast",
                                                                 "CT ABD W/WO, PELVIS WITH CONT","CT ABD/PELVIS W CONTRAST","CT ABD/PELVIS W/WO CONTRAST","CT ABD/PELVIS WITHOUT CONTRAST","CT ABDO RETRO FINE NEEDLE ASPIRATION",
                                                                 "CT ABDOMEN W AND WO CONTRAST","CT ABDOMEN WITH CONTRAST","CT ABDOMEN WITHOUT CONTRAST","CT ABDOMEN/RET MASS BIOPSY","CT ANGIO ABDOMEN",
                                                                 "CT CHEST W, ABD W/WO CONTRAST","CT CHEST/ABD W CONTRAST","CT CHEST/ABD WO CONTRAST","CT CHEST/ABD/PELV W CONTRAST",
                                                                 "CT CHEST/ABD/PELV W/WO CONTRAST","CT CHEST/ABD/PELV WO CONTRAST","CT CHEST/PELV W, ABD W/WO CONTRAST",
                                                                 "OS CT ABDOMEN COMPARE-NO READ","OS CT BODY COMPARE-NO READ","OS CT BODY INTERPRETATION","PR CT SCAN OF ABDOMEN COMBO",
                                                                 "PR CT SCAN OF ABDOMEN CONTRAST","PR CT SCAN OF PELVIS COMBO","PR CT SCAN OF PELVIS CONTRAST","PR CT SCAN,ABDOMEN,W/O CONTRAST",
                                                                 "OS CT CERVICAL SPINE COMPARE-NO READ","CT CERVICAL SPINE POST MYELO","CT CERVICAL SPINE RECONSTRUCTION","CT CERVICAL SPINE WITHOUT CONTRAST",
                                                                 "PR CT SCAN CERV SPINE CONTRAST","PR CT SCAN,CERVICAL SPINE,W/O CONTRAST","CT  CERVICAL SPINE WITH CONTRAST","PR DUPLEX EXTREM VENOUS,BILAT",
                                                                 "VASC DX LOWER EXTREMITY VENOUS ULTRASOUND","STRESS NUCLEAR - UCH","DOBUTAMINE STRESS NUCLEAR SCAN PROTOCOL","XR LUMBAR SPINE 2 VIEWS (FLEX,EXT)",
                                                                 "OS XR LUMBAR SPINE","OS XR LUMBAR SPINE COMPARE-NO READ","PR CHG X-RAY LUMBAR SPINE 2/3 VW","PR X-RAY LUMBAR SPINE 4 VW","XR LUMBAR SPINE (AP,LAT,FLEX,EXT,BENDING)",
                                                                 "XR LUMBAR SPINE (AP,OBLS,LAT,SPOT,FLEX,EXT)","XR LUMBAR SPINE 1 VIEW (AP)","XR LUMBAR SPINE 1 VIEW (LAT)","XR LUMBAR SPINE 2 VIEWS (AP,LAT)","XR LUMBAR SPINE 2 VIEWS (FLEX,EXT)",
                                                                 "XR LUMBAR SPINE 2 VIEWS (LAT BENDING ONLY)","XR LUMBAR SPINE 3 VIEWS (AP,LAT,SPOT)","XR LUMBAR SPINE 4 VIEW (AP,LAT,FLEX,EXT)","XR LUMBAR SPINE 5 VIEW (AP,LAT,OBLS,SPOT)",
                                                                 "XR LUMBAR SPINE 6 VIEW (AP,LAT,OBLS,FLEX,EXT)","PR ASSAY OF SERUM POTASSIUM","CENTRAL LINE","CENTRAL LINE INSERTION","PR NONINVASV EXTREM EXAM,MULT,BILAT",
                                                                 "PR SPCL STN 2 I&R EXCPT MICROORG/ENZYME/IMCYT&IMHIS","CLOS LARGE BOWEL BIOPSY","PR LORAZEPAM INJECTION 2 MG","UPDATE PATIENT CLASS I.E. INPATIENT / OBSERVATION",
                                                                 "ANTI-OBESITY - ANOREXIC AGENTS", "ANTIHYPERTENSIVES, ACE INHIBITORS", "FOLIC ACID PREPARATIONS", 
                                                                 "NOSE PREPARATIONS, VASOCONSTRICTORS(OTC)"))|
                               (proc_cohort_r$ProcedureName %in% CT_BODY_CHEST_PELV_W_ABD_list)|
                               (proc_cohort_r$ProcedureName %in% STRESS_NUCLEAR_list)|
                               (proc_cohort_r$ProcedureName %in% CENTRAL_line_list),] 



nsqip_proc_merge <- full_join(proc_cohort, Epic_data_r, by = "arb_person_id")
nsqip_proc_merge$ProcedureEventDate[nsqip_proc_merge$ProcedureEventDate==""] <- NA
nsqip_proc_merge$as_date_proc <- as.Date(nsqip_proc_merge$ProcedureEventDate)
nsqip_proc_merge$SurgeryDate_asDate <- as.Date(nsqip_proc_merge$SurgeryDate_asDate)

nsqip_proc_merge$length_proc_operation <- nsqip_proc_merge$SurgeryDate_asDate-nsqip_proc_merge$as_date_proc

#pre
nsqip_proc_merge$pre_op_proc <- ifelse(nsqip_proc_merge$length_proc_operation > 0 & nsqip_proc_merge$length_proc_operation <= 365,1,0)

make_dummy <- nsqip_proc_merge[nsqip_proc_merge$pre_op_proc==1 & !is.na(nsqip_proc_merge$pre_op_proc),]

transpose_icd10 <- make_dummy%>%
  select(SurgeryID, ProcedureName)%>%
  unique()%>%
  mutate(yesno = 1)%>%
  distinct%>%
  filter(!(ProcedureName == ""))%>%
  spread(ProcedureName, yesno, fill = 0)

store_patient_names_96 <- setdiff(unique(Epic_data$SurgeryID),unique(transpose_icd10$SurgeryID))

final_data <- bind_rows(transpose_icd10,data.frame(SurgeryID = store_patient_names_96))

final_data[is.na(final_data)] <- 0 

saveRDS(final_data, paste0(fp_processed,
                                        "final_data_store_proc_preop_0922.rds"))

# table(final_data_store_proc_preop_0922$`OS CT BODY COMPARE-NO READ`)
rm(final_data, Proc)

################################################################################
### TODO: clean_surg_preop_pred1006, clean epic data to get this 

dummydf1_select <- Epic_data

# sum(is.na(dummydf1_select$SurgeryID))

dummydf1_select$WoundClass <- as.factor(dummydf1_select$WoundClass)
# table(dummydf1_select$WoundClass, useNA = "ifany")
dummydf1_select$WoundClass[dummydf1_select$WoundClass=="Other (see notes)"]<- "*Unspecified"
dummydf1_select$WoundClass[dummydf1_select$WoundClass=="N/A"]<- "*Unspecified"
dummydf1_select$WoundClass[dummydf1_select$WoundClass=="*Not Applicable"]<- "*Unspecified"
dummydf1_select$WoundClass[dummydf1_select$WoundClass=="*Unspecified"]<- "Clean"
# table(dummydf1_select$WoundClass, useNA = "ifany")
dummydf1_select$WoundClass <- droplevels(dummydf1_select$WoundClass)

dummydf1_select$WoundClass[is.na(dummydf1_select$WoundClass)] <- "Clean"

# TODO: check missingness of AsaRatingName missingness
AsaRatingMissing <- sum(is.na(dummydf1_select$AsaRatingName))/length(dummydf1_select$AsaRatingName)

if(AsaRatingMissing > 0.2){
  # table(dummydf1_select$AsaClass)
  # sum(dummydf1_select$AsaClass == "*Unspecified", na.rm = TRUE)
  # sum(dummydf1_select$AsaClass == "1", na.rm = TRUE)
  #TODO: Remove class "6", sometimes, "6" is not there, may introduce missingness
  if("6" %in% levels(dummydf1_select$AsaClass)) {
    dummydf1_select <- dummydf1_select %>% filter(AsaClass != "6")
  }
  
  dummydf1_select$ASA_class_epic_trans <- NA
  
  # Map AsaClass values to ASA_class_epic_trans:
  # If AsaClass is missing, empty, or "Unspecified", assign "1or2"
  dummydf1_select$ASA_class_epic_trans[
    is.na(dummydf1_select$AsaClass) | dummydf1_select$AsaClass == "" | 
      dummydf1_select$AsaClass == "*Unspecified"
  ] <- "1or2"
  
  dummydf1_select$ASA_class_epic_trans[
    dummydf1_select$AsaClass %in% c("1", "2")
  ] <- "1or2"
  
  dummydf1_select$ASA_class_epic_trans[
    dummydf1_select$AsaClass == "3"
  ] <- "3"
  
  dummydf1_select$ASA_class_epic_trans[
    dummydf1_select$AsaClass %in% c("4", "5")
  ] <- "4or5"
  
}else{
  dummydf1_select$ASA_class_epic_trans[dummydf1_select$AsaRatingName=="Healthy"] <- "1or2"
  dummydf1_select$ASA_class_epic_trans[dummydf1_select$AsaRatingName=="Mild Systemic Disease"] <- "1or2"
  dummydf1_select$ASA_class_epic_trans[dummydf1_select$AsaRatingName=="Severe Systemic Disease"] <- "3"
  dummydf1_select$ASA_class_epic_trans[dummydf1_select$AsaRatingName=="Incapacitating Disease"] <- "4or5"
  dummydf1_select$ASA_class_epic_trans[dummydf1_select$AsaRatingName=="Moribund"] <- "4or5"
  dummydf1_select$ASA_class_epic_trans[dummydf1_select$AsaRatingName==""] <- "1or2"
  dummydf1_select$ASA_class_epic_trans[is.na(dummydf1_select$ASA_class_epic_trans)] <- "1or2"
}

# table(dummydf1_select$ASA_class_epic_trans)

dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Female Pelvic Medicine And Reconstructive Surgery"] <- "Obstetrics and Gynecology"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Gynecologic Oncology"] <- "Obstetrics and Gynecology"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Hand Surgery"] <- "Orthopedic Surgery"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Orthopedics"] <- "Orthopedic Surgery"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Plastic and Reconstructive Surgery"] <- "Plastic Surgery"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Reproductive Endo/Infertility"] <- "Obstetrics and Gynecology"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Sports Medicine"] <- "Orthopedic Surgery"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Surgery"] <- "General Surgery"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="GI, tumor, endocrine surgery"] <- "General Surgery"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Oral and maxillofacial surgery"] <- "Plastic Surgery"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="GI, Tumor, Endocrine Surgery"] <- "General Surgery"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Gynecology"] <- "Obstetrics and Gynecology"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Oral and Maxillofacial Surgery"] <- "Plastic Surgery"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Certified Nurse Midwife"] <- "Obstetrics and Gynecology"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Maternal Fetal Medicine"] <- "Obstetrics and Gynecology"


dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Radiation Oncology"] <- "Non-surgical"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Pulmonary Medicine"] <- "Non-surgical"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Podiatry"] <- "Non-surgical"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Interventional Radiology"] <- "Non-surgical"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Anesthesiology"] <- "Non-surgical"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Cardiology"] <- "Non-surgical"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty==" Cardiovascular Disease"] <- "Non-surgical"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Family Medicine"] <- "Non-surgical"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Gastroenterology"] <- "Non-surgical"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Internal Medicine"] <- "Non-surgical"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Cardiovascular Disease"] <- "Non-surgical"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Ophthalmology"] <- "Non-surgical"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Pain Medicine"] <- "Non-surgical"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Pediatric Surgery"] <- "Non-surgical"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Pediatric Neurosurgery"] <- "Non-surgical"

dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Sleep Medicine"] <- "Non-surgical"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Surgical Critical Care"] <- "Non-surgical"

dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Transplant Surgery"] <- "Non-surgical"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Surgical Critical Care"] <- "Non-surgical"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Transplant Surgery"] <- "Non-surgical"
dummydf1_select$PrimarySurgeonSpecialty[dummydf1_select$PrimarySurgeonSpecialty=="Obstetrics and Gynecology"] <- "Gynecology"


dummydf1_select$PrimarySurgeonSpecialty[is.na(dummydf1_select$PrimarySurgeonSpecialty)] <- "*Unspecified"

dummydf1_select <- dummy_cols(dummydf1_select, select_columns = "PrimarySurgeonSpecialty",remove_selected_columns=FALSE)

# table(dummydf1_select$SurgeryInpatientOrOutpatient, useNA = "ifany")
# TODO: also make . not available  
# sum(is.na(dummydf1_select$SurgeryInpatientOrOutpatient))
dummydf1_select$SurgeryInpatientOrOutpatient[dummydf1_select$SurgeryInpatientOrOutpatient==""
                    | is.na(dummydf1_select$SurgeryInpatientOrOutpatient)]<- "Not Available"

# sum(is.na(dummydf1_select$SurgeryID))

dummydf1_selectv2 <- dummy_cols(dummydf1_select, select_columns = "SurgeryInpatientOrOutpatient",remove_selected_columns=FALSE)

dummydf1_selectv2$AsaRatingName <- NULL
dummydf1_selectv2$`SurgeryInpatientOrOutpatient_Not Available` <- NULL
dummydf1_selectv2$`PrimarySurgeonSpecialty_*Unspecified` <- NULL

clean_surg_preop_pred1006 <- dummydf1_selectv2

saveRDS(clean_surg_preop_pred1006, paste0(fp_processed,
                                  "clean_surg_preop_pred1006.rds"))

# table(clean_surg_preop_pred1006$WoundClass, useNA = "ifany")

#write.csv(dummydf1_selectv2,"/Users/yaxuzhuang/OneDrive - The University of Colorado Denver/Katie Project/2022_0711/clean_surg_preop_pred1006.csv")

rm(dummydf1_selectv2, dummydf1_select)

######################################################################
## TODO: pre_op_gender
# patient <- read.csv(file = "data/C2730_Table1_Person_20240223.csv")
# TODO: named as patient_mrn (SURPAS)
mrn%<>% dplyr::rename(patient_mrn = Mrn)
patient %<>% left_join(mrn, by = "arb_person_id")
patient$Epic_DOD <- ifelse(patient$Epic_DOD == "", NA, patient$Epic_DOD)
patient$CDPHE_DOD <- ifelse(patient$CDPHE_DOD == "", NA, patient$CDPHE_DOD)
patient$Date_of_Death <- coalesce(as.Date(patient$Epic_DOD, format = "%Y-%m-%d"), as.Date(patient$CDPHE_DOD, format = "%Y-%m-%d"))
patient$Sex <- ifelse(patient$Sex %in% c("Unknown", "X"), NA, patient$Sex)

patient_cohort <- patient[patient$arb_person_id %in% Epic_data$arb_person_id,c("arb_person_id","Sex", "Race", "Ethnicity", "Dob", "Date_of_Death","patient_mrn")]
patient_cohort_uni <- patient_cohort[!duplicated(patient_cohort[ , c("arb_person_id", "patient_mrn")]),]

df_merge_gender <- left_join(Epic_data_r[,c("arb_person_id","SurgeryID")],
                             patient_cohort_uni,by=c("arb_person_id"))

df_storee <- dummy_cols(df_merge_gender, select_columns ="Sex")

saveRDS(df_storee, paste0(fp_processed,
                                          "pre_op_gender.rds"))
#write.csv(df_storee,file="/Users/yaxuzhuang/OneDrive - The University of Colorado Denver/Katie Project/2022_0711/preop_lab/pre_op_gender.csv")
rm(patient, patient_cohort, patient_cohort_uni)



