# Define variables to preserve
vars_to_keep <- c("pipeline_dir", "source_if_exists")

# Remove all objects except those in vars_to_keep
rm(list = setdiff(ls(), vars_to_keep))

# Rest of your script...


library(lubridate)
library(tidyr)
library(plyr)
library(dplyr)
library(tidyverse)
library(fastDummies)


source("./global_vars.R")

Epic_data <- readRDS(paste0(fp_processed,
                            "ICD10_uni_surg_id_spec.rds"))

# TODO: more readRDS
###############################################################################
# preop
final_data_store_diag_preop <- readRDS( paste0(fp_processed,
                                            "final_data_store_diag_preop.rds"))

com_score_preop <- readRDS(paste0(fp_processed,
                                "com_score_preop.rds"))
final_data_store_preop_lab <-  readRDS( paste0(fp_processed,
                                           "final_data_store_preop_lab.rds"))

final_data_store_proc_preop_0922 <- readRDS(paste0(fp_processed,
                                                 "final_data_store_proc_preop_0922.rds"))

clean_surg_preop_pred1006 <- readRDS(paste0(fp_processed,
                                          "clean_surg_preop_pred1006.rds"))

pre_op_gender <- readRDS(paste0(fp_processed,
                              "pre_op_gender.rds"))

# postop
final_data_store_diag_postop <- readRDS( paste0(fp_processed,
                                        "final_data_store_diag_postop.rds"))
final_data_store_lab_postop <- readRDS(paste0(fp_processed,
                                            "final_data_store_lab_postop.rds"))
final_data_store_medi_postop <- readRDS(paste0(fp_processed,
                                             "final_data_store_medi_postop.rds"))

Epic_data <- readRDS(paste0(fp_processed,
                            "ICD10_uni_surg_id_spec.rds"))

##############################################################################
## TODO: prepare analytical_preopv2
## TODO: preop_expected_rate_20221005
diag_pred <- final_data_store_diag_preop

lab_pred <- final_data_store_preop_lab

proc_pred_preop <- final_data_store_proc_preop_0922
# table(proc_pred_preop$`OS CT BODY COMPARE-NO READ`)

# colnames(proc_pred_preop)

# clean_surg_preop_pred1006

surg_pred_preop <- clean_surg_preop_pred1006

# sum(is.na(clean_surg_preop_pred1006$SurgeryID))

# table(surg_pred_preop$`OS CT BODY COMPARE-NO READ`)

gender_preop <- pre_op_gender

# colnames(diag_pred)[284:288]
n <- which(colnames(diag_pred) == "80")

diag_pred_clean <- diag_pred[,c(which(colnames(diag_pred)=="SurgeryID"), 
                                n:ncol(diag_pred))]

# table(diag_pred_clean$`OS CT BODY COMPARE-NO READ`)

# %>%rename(`80` = phecode_80) # problem with 80

analytical_preop <- merge(surg_pred_preop, diag_pred_clean,by="SurgeryID")

# sum(is.na(surg_pred_preop$SurgeryID))


# colnames(lab_pred)[287:ncol(lab_pred)]

n <- grep("^ANAEROBIC CULTURE,", colnames(lab_pred))[1]

# TODO: ncol(lab_pred)-4 to remove ] "arb_encounter_id"   "EpicCptCode"                                                
# [9] "OmopCptCode"     "OmopHcpcsCode"

lab_pred_clean <- lab_pred[,c(which(colnames(lab_pred)=="SurgeryID"),
                              n:(ncol(lab_pred)-4))]

analytical_preopdf2 <- merge(analytical_preop,lab_pred_clean,by="SurgeryID")


#all.equal(analytical_preopdf2$arb_person_id.x,analytical_preopdf2$arb_person_id)

# colnames(proc_pred_preop)[294: 299]

proc_pred_preop_clean <- proc_pred_preop[,
                                 c(which(colnames(proc_pred_preop)=="SurgeryID"),
                                 which(colnames(proc_pred_preop)=="OS CT BODY COMPARE-NO READ"))]

# glimpse(proc_pred_preop_clean)
# colnames(proc_pred_preop)[290:300]



#analytical_preopdf3 <- merge(analytical_preopdf2,medi_pred,by="arb_person_id")
analytical_preopdf4 <- merge(analytical_preopdf2,proc_pred_preop_clean,by="SurgeryID")

# colnames(gender_preop)[3:8]

gender_preop_clean <- gender_preop[,c(which(colnames(gender_preop)=="SurgeryID"),
                                      which(colnames(gender_preop)=="Sex_Male"))]
#analytical_preopdf5 <- merge(analytical_preopdf4,surg_pred_preop,by="arb_person_id")
analytical_preopdf6 <- merge(analytical_preopdf4,gender_preop_clean,by="SurgeryID")

com_score_preop_clean <- com_score_preop[,c(which(colnames(com_score_preop)=="SurgeryID"),ncol(com_score_preop))]

analytical_preopdf7 <- merge(analytical_preopdf6,com_score_preop_clean,by="SurgeryID")

analytical_preopv2 <- analytical_preopdf7

analytical_preopv2$comb_560 <- ifelse(rowSums(
  analytical_preopv2[,which(substring(colnames(analytical_preopv2),1,3)==560)])>=1,1,0)

# nrow(analytical_preopv2)
# table(analytical_preopv2$ASA_class_epic_trans)
# colnames(analytical_preopv2)[295:297]

# colnames(analytical_preopv2)[which(colnames(analytical_preopv2) 
# == "80"): ncol(analytical_preopv2)]

cultures_to_combine <- grep("CULTURE", 
                            colnames(analytical_preopv2)[which(colnames(analytical_preopv2) 
                                                               == "80"): ncol(analytical_preopv2)],
                            value = TRUE)
# TODO: print out cultures to combine 
message(cultures_to_combine)

# analytical_preopv2$blood_cult_comb <- ifelse(rowSums(analytical_preopv2[,c(296:306)])>=1,1,0)
analytical_preopv2$blood_cult_comb <- ifelse(rowSums(analytical_preopv2[,
                                                                        cultures_to_combine])>=1,1,0)
analytical_preopv2$WoundClass <- ifelse(
  is.na(analytical_preopv2$WoundClass), NA,
  paste0(analytical_preopv2$WoundClass, " ")
)



# typeof(analytical_preopv2)
# glimpse(analytical_preopv2)
# table(analytical_preopv2$WoundClass, useNA = "ifany")
# table(analytical_preopv2$SurgeryInpatientOrOutpatient, useNA = "ifany")

# table(analytical_preopv2$SurgeryInpatientOrOutpatient_Inpatient , useNA = "ifany")
# table(analytical_preopv2$SurgeryInpatientOrOutpatient_Outpatient , useNA = "ifany")

saveRDS(analytical_preopv2, paste0(fp_processed,
                                   "analytical_preopv2.rds"))


###############################################################################
# TODO: # Postop rate

diag_pred <-final_data_store_diag_postop
lab_pred <- final_data_store_lab_postop
medi_pred <- final_data_store_medi_postop

Epic_data_sub <- Epic_data[,c("SurgeryID","arb_person_id","PrimarySurgeonSpecialty")]

analytical_postop <- merge(Epic_data_sub,diag_pred)
analytical_postopv <- merge(analytical_postop,medi_pred)

analytical_postopv2 <- merge(analytical_postopv,lab_pred,
                             by = c("SurgeryID","arb_person_id", "arb_encounter_id"))

# analytical_postopv2$`80` <- analytical_postopv2$phecode_80
# colnames(analytical_postopv2)
analytical_postopv2$comb_599 <- ifelse(rowSums(analytical_postopv2[,which(substring(colnames(analytical_postopv2),1,3)==599)])>=1,1,0)
analytical_postopv2$comb_592 <- ifelse(rowSums(analytical_postopv2[,which(substring(colnames(analytical_postopv2),1,3)==592)])>=1,1,0)
analytical_postopv2$comb_540 <- ifelse(rowSums(analytical_postopv2[,which(substring(colnames(analytical_postopv2),1,3)==540)])>=1,1,0)
analytical_postopv2$comb_994 <- ifelse(rowSums(analytical_postopv2[,which(substring(colnames(analytical_postopv2),1,3)==994)])>=1,1,0)
analytical_postopv2$comb_480 <- ifelse(rowSums(analytical_postopv2[,which(substring(colnames(analytical_postopv2),1,3)==480)])>=1,1,0)

# ANAEROBIC CULTURE
# TODO: combine more cultures as 2024 

blood_cultures_to_combine <- grep("BLOOD", 
                                  colnames(analytical_postopv2)[which(colnames(analytical_postopv2) 
                                                                      == "80"): ncol(analytical_postopv2)],
                                  value = TRUE)
# can't subset columns not exist
blood_cultures_to_combine <- c(blood_cultures_to_combine, 
                               # "CULTURE ANAEROBIC", "ANAEROBIC CULTURE",
                               "ANAEROBIC CULTURE, (EG:TISSUE, ABSCESS, WOUND, SINUS, ETC.)")
blood_cultures_to_combine <- blood_cultures_to_combine[!blood_cultures_to_combine
                                                       %in% c("ARTERIAL BLOOD GAS", "VENOUS BLOOD GAS")]

message(blood_cultures_to_combine)

# cultures_to_combine

analytical_postopv2$blood_cult_comb <- ifelse(rowSums(analytical_postopv2[,
                                                                          blood_cultures_to_combine])>=1,1,0)


urine_cultures_to_combine <- grep("URINE CULTURE", 
                                  colnames(analytical_postopv2)[which(colnames(analytical_postopv2) 
                                                                      == "80"): ncol(analytical_postopv2)],
                                  value = TRUE)

urine_cultures_to_combine <- c(urine_cultures_to_combine,
                               "UA DIPSTICK W/ REFLEX TO MICROSCOPIC EXAM IF IND (NO CULTURE REFLEX, EXCEPT EPH)",
                               "UA COMPLETE URINALYSIS (NO CULTURE REFLEX)")
message(urine_cultures_to_combine)

analytical_postopv2$Urine_cult_comb <- ifelse(rowSums(analytical_postopv2[,
                                                                          urine_cultures_to_combine])>=1,1,0)

# TODO: double check if CDIFFICILE TOXIN PCR exists 
# TODO: CDIFFICILE
# TODO: string search LabPanelName analytical_postopv2$c_diff_comb*UTI_coef_postop$x[7]+
# There is one patient since 20240101, has labpanelname  CDIFFICILE TOXIN PCR
# analytical_postopv2$c_diff_comb <- ifelse(analytical_postopv2$`CDIFFICILE TOXIN PCR`>=1,1,0)

# table(df_CDIFF_search$LabPanelName) %>% names()
# [1] "C DIFF GDH/TOXIN"                                 "C DIFFICILE BY PCR"                              
# [3] "C DIFFICILE TOXIN/GDH"                            "CDIFFICILE TOXIN PCR"                            
# [5] "LAB USE ONLY - C.DIFFICILE TOXIN REFLEX"          "LAB USE ONLY - C.DIFFICILE TOXIN REFLEX (GI PCR)"

CDIFFs_to_combine <- grep("DIFF", colnames(analytical_postopv2)[which(colnames(analytical_postopv2) 
                                                                      == "80"): ncol(analytical_postopv2)],
                          value = TRUE) %>%
  grep("CBC", ., invert = TRUE, value = TRUE)

message(CDIFFs_to_combine)

# sum(analytical_postopv2[, CDIFFs_to_combine])

analytical_postopv2$c_diff_comb <- ifelse(rowSums(analytical_postopv2[,
                                                                      CDIFFs_to_combine, drop = FALSE])>=1,1,0)

# sum(analytical_postopv2$c_diff_comb)

analytical_postopv2$CBC_auto_diff_comb <- ifelse(analytical_postopv2$`CBC NO AUTO DIFF`+ 
                                                   analytical_postopv2$`CBC WITH AUTO DIFF`+analytical_postopv2$`CBC WITH MANUAL DIFF IF AUTO FAILS (PERFORMABLE)`>=1,1,0)

analytical_postopv2$respiratory_comb <- ifelse(analytical_postopv2$`RESPIRATORY CULTURE`>=1,1,0)

# Prob
analytical_postopv2$BLOOD_GASES_comb <- ifelse(analytical_postopv2$`ARTERIAL BLOOD GAS`+
                                                 analytical_postopv2$`VENOUS BLOOD GAS`>=1,1,0)

saveRDS(analytical_postopv2, paste0(fp_processed,
                                    "analytical_postopv2.rds"))




