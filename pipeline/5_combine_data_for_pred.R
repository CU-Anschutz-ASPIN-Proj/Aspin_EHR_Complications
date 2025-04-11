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


final_data_store_medi_preop <- readRDS(paste0(fp_processed,
                                                 "final_data_store_medi_preop.rds"))

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
final_data_store_medi_postop_other <- readRDS(paste0(fp_processed,
                                               "final_data_store_medi_postop_other_comp.rds"))
final_data_store_proc_postop <- readRDS(paste0(fp_processed,
                                               "final_data_store_proc_postop.rds"))

Epic_data <- readRDS(paste0(fp_processed,
                            "ICD10_uni_surg_id_spec.rds"))# why we have two load of epic data?

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

medi_pred_preop <- final_data_store_medi_preop

# sum(is.na(clean_surg_preop_pred1006$SurgeryID))

# table(surg_pred_preop$`OS CT BODY COMPARE-NO READ`)

gender_preop <- pre_op_gender

analytical_preop <- inner_join(surg_pred_preop,diag_pred_preop,by="SurgeryID")

# TODO: FIX: add the following line to use predictor 
analytical_preop <- inner_join(analytical_preop, medi_pred_preop, by="SurgeryID")

analytical_preopdf2 <- inner_join(analytical_preop,lab_pred_preop,by="SurgeryID")

analytical_preopdf4 <- inner_join(analytical_preopdf2,proc_pred_preop,by="SurgeryID")

gender_preop_clean <- gender_preop%>%
  select(SurgeryID, Sex, Race, Ethnicity, Dob, Date_of_Death, Sex_Male, patient_mrn)

analytical_preopdf6 <- inner_join(analytical_preopdf4,gender_preop_clean,by="SurgeryID")

com_score_preop_clean <- com_score_preop%>%
  rowwise()%>%
  mutate(num_com = sum(mi,chf,pvd,cevd,dementia,cpd,rheumd,pud,mld,diab,diabwc,hp,rend,canc,msld,metacanc,aids))%>%
  select(SurgeryID, wscore, windex, windex2, num_com)

analytical_preopdf7 <- inner_join(analytical_preopdf6,com_score_preop_clean,by="SurgeryID")

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
# TODO: need to check if your code does not include other cultures in our new code
# anlytical_preopv2$blood_cult_comb <- ifelse(analytical_preopv2$`ANAEROBIC CULTURE, (EG:TISSUE, ABSCESS, WOUND, SINUS, ETC.)`+
#                                                analytical_preopv2$`BLOOD CULTURE`+
#                                                analytical_preopv2$`CULTURE ANAEROBIC` >=1,1,0)

analytical_preopv2$WoundClass <- ifelse(
  is.na(analytical_preopv2$WoundClass), NA,
  paste0(analytical_preopv2$WoundClass, " ")
)

# TODO: variables for non-infectious comoplications
analytical_preopv2$anemia <- ifelse(analytical_preopv2$`285`==1|analytical_preopv2$`285.1`==1|analytical_preopv2$`285.2`==1|
                                      analytical_preopv2$`285.21`==1|analytical_preopv2$`285.22`==1|analytical_preopv2$`285.3`==1|analytical_preopv2$`285.8`==1,1,0)
analytical_preopv2$renal_failure <- ifelse(analytical_preopv2$`585`==1|analytical_preopv2$`585.1`==1|analytical_preopv2$`585.2`==1,1,0)
analytical_preopv2$fluid_elec_acid_disorders <- ifelse(analytical_preopv2$`276.1`==1|analytical_preopv2$`276.11`==1|
                                                         analytical_preopv2$`276.13`==1|analytical_preopv2$`276.4`==1|
                                                         analytical_preopv2$`276.42`==1,1,0)

analytical_preopv2$VT <- ifelse(analytical_preopv2$`452`==1|analytical_preopv2$`452.2`==1,1,0)

analytical_preopv2$CT.BODY.CHEST.PELV.W.ABD <- ifelse(analytical_preopv2$`OS CT BODY COMPARE-NO READ`==1|
                                                        analytical_preopv2$`CT CHEST/PELV W, ABD W/WO CONTRAST`==1|
                                                        analytical_preopv2$`.PET/CT WHOLE BODY NO IV CONTRAST`==1|
                                                        analytical_preopv2$`ABN ONLY CT ABD & PELVIS W/O CONTRAST`==1|
                                                        analytical_preopv2$`ABN ONLY CT ABD&PELV 1+ SECTION/REGNS` == 1|
                                                        analytical_preopv2$`Computerized Tomography (CT Scan) of Abdomen and Pelvis using Low Osmolar Contrast`==1|
                                                        analytical_preopv2$`CT ABD W/WO, PELVIS WITH CONT`==1|
                                                        analytical_preopv2$`CT ABD/PELVIS W CONTRAST`==1|
                                                        analytical_preopv2$`CT ABD/PELVIS W/WO CONTRAST`==1|
                                                        analytical_preopv2$`CT ABD/PELVIS WITHOUT CONTRAST`==1|
                                                        analytical_preopv2$`CT ABDO RETRO FINE NEEDLE ASPIRATION`==1|
                                                        analytical_preopv2$`CT ABDOMEN W AND WO CONTRAST`==1|
                                                        analytical_preopv2$`CT ABDOMEN WITH CONTRAST`==1|
                                                        analytical_preopv2$`CT ABDOMEN WITHOUT CONTRAST`==1|
                                                        analytical_preopv2$`CT ABDOMEN/RET MASS BIOPSY`==1|
                                                        analytical_preopv2$`CT ANGIO ABDOMEN`==1|
                                                        analytical_preopv2$`CT CHEST W, ABD W/WO CONTRAST`==1|
                                                        analytical_preopv2$`CT CHEST/ABD W CONTRAST`==1|
                                                        analytical_preopv2$`CT CHEST/ABD WO CONTRAST`==1|
                                                        analytical_preopv2$`CT CHEST/ABD/PELV W CONTRAST`==1|
                                                        analytical_preopv2$`CT CHEST/ABD/PELV W/WO CONTRAST`==1|
                                                        analytical_preopv2$`CT CHEST/ABD/PELV WO CONTRAST`==1|
                                                        analytical_preopv2$`CT CHEST/PELV W, ABD W/WO CONTRAST`==1|
                                                        analytical_preopv2$`OS CT ABDOMEN COMPARE-NO READ`==1|
                                                        analytical_preopv2$`OS CT BODY COMPARE-NO READ`==1|
                                                        analytical_preopv2$`OS CT BODY INTERPRETATION`==1|
                                                        analytical_preopv2$`PR CT SCAN OF ABDOMEN COMBO`==1|
                                                        analytical_preopv2$`PR CT SCAN OF ABDOMEN CONTRAST`==1|
                                                        analytical_preopv2$`PR CT SCAN OF PELVIS COMBO`==1|
                                                        analytical_preopv2$`PR CT SCAN OF PELVIS CONTRAST`==1|
                                                        analytical_preopv2$`PR CT SCAN,ABDOMEN,W/O CONTRAST`==1,1,0)

analytical_preopv2$OS.CT.CERVICAL.SPINE <- ifelse(analytical_preopv2$`OS CT CERVICAL SPINE COMPARE-NO READ`==1|
                                                    analytical_preopv2$`CT CERVICAL SPINE POST MYELO`==1|
                                                    analytical_preopv2$`CT CERVICAL SPINE RECONSTRUCTION`==1|
                                                    analytical_preopv2$`CT CERVICAL SPINE WITHOUT CONTRAST`==1|
                                                    analytical_preopv2$`PR CT SCAN CERV SPINE CONTRAST`==1|
                                                    analytical_preopv2$`PR CT SCAN,CERVICAL SPINE,W/O CONTRAST`==1|
                                                    analytical_preopv2$`CT  CERVICAL SPINE WITH CONTRAST`==1,1,0)

analytical_preopv2$VENOUS.ULTRASOUND <- ifelse(analytical_preopv2$`PR DUPLEX EXTREM VENOUS,BILAT`==1|
                                                 analytical_preopv2$`VASC DX LOWER EXTREMITY VENOUS ULTRASOUND`==1,1,0)

analytical_preopv2$STRESS.NUCLEAR <- ifelse(analytical_preopv2$`STRESS NUCLEAR - UCH`==1|
                                              analytical_preopv2$`DOBUTAMINE STRESS NUCLEAR SCAN PROTOCOL`==1,1,0)

analytical_preopv2$LUMBAR.SPINE <- ifelse(analytical_preopv2$`XR LUMBAR SPINE 2 VIEWS (FLEX,EXT)`==1|
                                            analytical_preopv2$`OS XR LUMBAR SPINE`==1|
                                            analytical_preopv2$`OS XR LUMBAR SPINE COMPARE-NO READ`==1|
                                            analytical_preopv2$`PR CHG X-RAY LUMBAR SPINE 2/3 VW`==1|
                                            analytical_preopv2$`PR X-RAY LUMBAR SPINE 4 VW`==1|
                                            analytical_preopv2$`XR LUMBAR SPINE (AP,LAT,FLEX,EXT,BENDING)`==1|
                                            analytical_preopv2$`XR LUMBAR SPINE (AP,OBLS,LAT,SPOT,FLEX,EXT)`==1|
                                            analytical_preopv2$`XR LUMBAR SPINE 1 VIEW (AP)`==1|
                                            analytical_preopv2$`XR LUMBAR SPINE 1 VIEW (LAT)`==1|
                                            analytical_preopv2$`XR LUMBAR SPINE 2 VIEWS (AP,LAT)`==1|
                                            analytical_preopv2$`XR LUMBAR SPINE 2 VIEWS (FLEX,EXT)`==1|
                                            analytical_preopv2$`XR LUMBAR SPINE 2 VIEWS (LAT BENDING ONLY)`==1|
                                            analytical_preopv2$`XR LUMBAR SPINE 3 VIEWS (AP,LAT,SPOT)`==1|
                                            analytical_preopv2$`XR LUMBAR SPINE 4 VIEW (AP,LAT,FLEX,EXT)`==1|
                                            analytical_preopv2$`XR LUMBAR SPINE 5 VIEW (AP,LAT,OBLS,SPOT)`==1|
                                            analytical_preopv2$`XR LUMBAR SPINE 6 VIEW (AP,LAT,OBLS,FLEX,EXT)`==1,1,0)

analytical_preopv2$POTASSIUM <- ifelse(analytical_preopv2$`POTASSIUM SERUM/PLASMA`==1|
                                         analytical_preopv2$`PR ASSAY OF SERUM POTASSIUM`==1|
                                         analytical_preopv2$`I STAT POTASSIUM`==1|
                                         analytical_preopv2$`POCT POTASSIUM`==1|
                                         analytical_preopv2$`POTASSIUM WHOLE BLOOD`==1,1,0)

analytical_preopv2$CENTRAL.LINE <- ifelse(analytical_preopv2$`CENTRAL LINE`==1|
                                            analytical_preopv2$`CENTRAL LINE INSERTION`==1,1,0)

analytical_preopv2$age_when_surgery <- age_calc(na.omit(as.Date(analytical_preopv2$Dob)),units = "years",enddate=as.Date(analytical_preopv2$SurgeryDate_asDate),precise = TRUE)
analytical_preopv2$interger_age <- floor(analytical_preopv2$age_when_surgery)

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
medi_pred2 <- final_data_store_medi_postop_other
proc_pred <- final_data_store_proc_postop

Epic_data_sub <- Epic_data[,c("SurgeryID","arb_person_id","PrimarySurgeonSpecialty")]

analytical_postop <- inner_join(Epic_data_sub,diag_pred, by="SurgeryID")
analytical_postopv <- inner_join(analytical_postop,medi_pred, by="SurgeryID")
analytical_postopv1 <- inner_join(analytical_postopv,medi_pred2, by = "SurgeryID")
analytical_postopv2 <- inner_join(analytical_postopv1,lab_pred, by = c("SurgeryID"))
analytical_postopv2 <- inner_join(analytical_postopv2,proc_pred, by = c("SurgeryID"))
analytical_postopv2%<>%as.data.frame()

# analytical_postopv2$`80` <- analytical_postopv2$phecode_80
# colnames(analytical_postopv2)
analytical_postopv2$comb_599 <- ifelse(rowSums(analytical_postopv2[,which(substring(colnames(analytical_postopv2),1,3)==599)])>=1,1,0)
analytical_postopv2$comb_592 <- ifelse(rowSums(analytical_postopv2[,which(substring(colnames(analytical_postopv2),1,3)==592)])>=1,1,0)
analytical_postopv2$comb_540 <- ifelse(rowSums(analytical_postopv2[,which(substring(colnames(analytical_postopv2),1,3)==540)])>=1,1,0)
analytical_postopv2$comb_994 <- ifelse(rowSums(analytical_postopv2[,which(substring(colnames(analytical_postopv2),1,3)==994)])>=1,1,0)
analytical_postopv2$comb_480 <- ifelse(rowSums(analytical_postopv2[,which(substring(colnames(analytical_postopv2),1,3)==480)])>=1,1,0)

# ANAEROBIC CULTURE
# TODO: combine more cultures as 2024 
# CHECK this in our new code
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
# analytical_postopv2$blood_cult_comb <- ifelse(analytical_postopv2$`ANAEROBIC CULTURE, (EG:TISSUE, ABSCESS, WOUND, SINUS, ETC.)`+
#                                                 analytical_postopv2$`BLOOD CULTURE`+
#                                                 analytical_postopv2$`CULTURE ANAEROBIC`>=1,1,0)
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
# analytical_postopv2$Urine_cult_comb <- ifelse(analytical_postopv2$`UA DIPSTICK W/ REFLEX TO MICROSCOPIC EXAM IF IND (NO CULTURE REFLEX, EXCEPT EPH)`+
#                                                 analytical_postopv2$`UA COMPLETE URINALYSIS (NO CULTURE REFLEX)`+
#                                                 analytical_postopv2$`UA COMPLETE URINALYSIS W/ CULTURE REFLEX IF INDICATED` +
#                                                 analytical_postopv2$`URINE CULTURE`+analytical_postopv2$`URINE CULTURE - MHS ONLY`+
#                                                 analytical_postopv2$`UA MICROSCOPIC ONLY (NO CULTURE REFLEX,EXCEPT EPH)`>=1,1,0)

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

# analytical_postopv2$c_diff_comb <- ifelse(analytical_postopv2$`CDIFFICILE TOXIN PCR`+
#                                             analytical_postopv2$`C DIFFICILE BY PCR`>=1,1,0)
analytical_postopv2$c_diff_comb <- ifelse(rowSums(analytical_postopv2[,
                                                                      CDIFFs_to_combine, drop = FALSE])>=1,1,0)

# sum(analytical_postopv2$c_diff_comb)
# analytical_postopv2$CBC_auto_diff_comb <- ifelse(analytical_postopv2$`CBC NO AUTO DIFF`+
#                                                    analytical_postopv2$`CBC WITH AUTO DIFF`+
#                                                    analytical_postopv2$`CBC WITH MANUAL DIFF IF AUTO FAILS (PERFORMABLE)`>=1,1,0)

analytical_postopv2$CBC_auto_diff_comb <- ifelse(analytical_postopv2$`CBC NO AUTO DIFF`+ 
                                                   analytical_postopv2$`CBC WITH AUTO DIFF`+
                                                   analytical_postopv2$`CBC WITH MANUAL DIFF IF AUTO FAILS (PERFORMABLE)`>=1,1,0)

# analytical_postopv2$respiratory_comb <- ifelse(analytical_postopv2$`RESPIRATORY CULTURE`>=1,1,0)
analytical_postopv2$respiratory_comb <- ifelse(analytical_postopv2$`RESPIRATORY CULTURE`+
                                                 analytical_postopv2$`QUANTITATIVE RESPIRATORY CULTURE - NORTH/SOUTH ONLY`>=1,1,0)
# Prob
# analytical_postopv2$BLOOD_GASES_comb <- ifelse(analytical_postopv2$`ARTERIAL BLOOD GAS`+
#                                                  analytical_postopv2$`VENOUS BLOOD GAS`>=1,1,0)
analytical_postopv2$BLOOD_GASES_comb <- ifelse(analytical_postopv2$`(GEM) BLOOD GASES 4000`+
                                                 analytical_postopv2$`ARTERIAL BLOOD GAS`+
                                                 analytical_postopv2$`BLOOD GAS ARTERIAL WITH ELECTROLYTES`+
                                                 analytical_postopv2$`VENOUS BLOOD GAS`>=1,1,0)
# other comp
analytical_postopv2$Coronary.artery.disease <- ifelse(analytical_postopv2$`411.2` == 1|analytical_postopv2$`411.8` == 1 , 1, 0)
analytical_postopv2$red_blood_cell_or_whole_blood_transfusion <- ifelse(analytical_postopv2$`ED BLOOD TRANSFUSION PROCEDURE`+
                                                                          analytical_postopv2$`PACKED CELL TRANSFUSION`+
                                                                          analytical_postopv2$`BLOOD TRANSFUSION: USE ORDER SET ONLY. IF EMERGENT, CALL BLOOD BANK.` +
                                                                          analytical_postopv2$`TRANSFUSE RED BLOOD CELLS`+
                                                                          analytical_postopv2$`Transfusion of Nonautologous Red Blood Cells into Peripheral Vein, Percutaneous Approach`>=1,1,0)
analytical_postopv2$arrythmias <- ifelse(analytical_postopv2$`427.12` == 1|analytical_postopv2$`427.5` == 1 , 1, 0)
analytical_postopv2$sepshock <- ifelse(analytical_postopv2$`797` == 1|analytical_postopv2$`994.21` == 1 , 1, 0)
analytical_postopv2$intubation <- ifelse(analytical_postopv2$`Change Endotracheal Airway in Trachea, External Approach` == 1|
                                           analytical_postopv2$`INSERT ENDOTRACHEAL TUBE` == 1|
                                           analytical_postopv2$`Insertion of Endotracheal Airway into Trachea, Via Natural or Artificial Opening` == 1|
                                           analytical_postopv2$`Insertion of Endotracheal Airway into Trachea, Via Natural or Artificial Opening Endoscopic` == 1, 1, 0)
analytical_postopv2$Blood.products <- ifelse(analytical_postopv2$`VTE PLATELET MONITORING FOR IV UFH PATIENTS` == 1|
                                               analytical_postopv2$`AHG CROSSMATCH` == 1|
                                               analytical_postopv2$`ANTIBODY PATIENT INTERPS 1-5` == 1|
                                               analytical_postopv2$`PLATELETS UNIT` == 1|
                                               analytical_postopv2$`PREPARE PLATELETS FOR TRANSFUSION` == 1, 1, 0)
analytical_postopv2$echo <- ifelse(analytical_postopv2$`CARD DX ECHO COMPLETE TTE TRANSTHORACIC STANDARD` == 1|
                                     analytical_postopv2$`CARD DX ECHO LIMITED TTE 2D TRANSTHORACIC FOLLOW UP FOCUSED EXAM` == 1|
                                     analytical_postopv2$`CARD DX ECHO TRANSESOPHAGEAL TEE` == 1|
                                     analytical_postopv2$`DOBUTAMINE STRESS ECHO PROTOCOL` == 1|
                                     analytical_postopv2$`ECHO ADULT COMPLETE TTE` == 1|
                                     analytical_postopv2$`ECHO TRANSESOPHAGEAL TEE` == 1|
                                     analytical_postopv2$`ECHO WITH DEFINITY CONTRAST PROTOCOL` == 1|
                                     analytical_postopv2$`OS CV ECHO COMPARE-NO READ` == 1|
                                     analytical_postopv2$`PR DOPPLER ECHO HEART,LIMITED,F/U` == 1|
                                     analytical_postopv2$`PR ECHO HEART XTHORACIC,COMPLETE W DOPPLER` == 1| 
                                     analytical_postopv2$`PR ECHO HEART XTHORACIC,LIMITED`== 1|
                                     analytical_postopv2$`PR ECHO TRANSESOPHAG R-T 2D W/PRB IMG ACQUISJ I&R` == 1|
                                     analytical_postopv2$`PR ECHO TTHRC R-T 2D W/WO M-MODE REST&STRS CONT ECG` == 1|
                                     analytical_postopv2$`STRESS ECHO - UCH` == 1| analytical_postopv2$`STRESS ECHO DOBUTAMINE` == 1|
                                     analytical_postopv2$`ECHO ADULT COMPLETE TTE` == 1|
                                     analytical_postopv2$`ECHO ADULT LIMITED TTE` == 1, 1, 0)
analytical_postopv2$troponin <- ifelse(analytical_postopv2$`PR ASSAY OF TROPONIN, QUANT` == 1|
                                         analytical_postopv2$`ISTAT TROPONIN` == 1|
                                         analytical_postopv2$`POCT TROPONIN` == 1|
                                         analytical_postopv2$`TROPONIN I` == 1, 1, 0)

analytical_postopv2$Orthopedic.problems <- ifelse(analytical_postopv2$`740.1` == 1|
                                                    analytical_postopv2$`743.11` == 1|
                                                    analytical_postopv2$`800` == 1|
                                                    analytical_postopv2$`PARTIAL HIP REPLACEMENT` == 1, 1, 0)
analytical_postopv2$Social.planning <- ifelse(analytical_postopv2$`IP CONSULT TO DISCHARGE PLANNING` == 1|
                                                analytical_postopv2$`IP CONSULT TO SOCIAL WORK` == 1, 1, 0)
analytical_postopv2$Discharge.planning <- ifelse(analytical_postopv2$`OT EVAL AND TREAT`== 1|
                                                   analytical_postopv2$`PT EVAL AND TREAT` == 1|
                                                   analytical_postopv2$`RT HOME OXYGEN EVALUATION (INPATIENT ORDER)` == 1, 1, 0)
#analytical_postopv2$Ortho.surgery <- ifelse(analytical_postopv2$TOTAL.HIP.ARTHROPLASTY == 1|analytical_postopv2$TOTAL.KNEE.ARTHROPLASTY == 1|analytical_postopv2$TOTAL.KNEE.ARTHROPLASTY.BILATERAL == 1, 1, 0)
analytical_postopv2$XRAY <- ifelse(analytical_postopv2$`.XR CHEST SINGLE (AP)` == 1|
                                     analytical_postopv2$`XR CHEST SINGLE VIEW` == 1, 1, 0)
analytical_postopv2$BLOOD.GAS <- ifelse(analytical_postopv2$`(GEM) BLOOD GASES 4000` == 1|
                                          analytical_postopv2$`ARTERIAL BLOOD GAS` == 1, 1, 0)
analytical_postopv2$POINT.OF.CARE <- ifelse(analytical_postopv2$`POC(EPOC) ABG` == 1|#
                                              analytical_postopv2$`POINT OF CARE TESTS` == 1, 1, 0)#
analytical_postopv2$IRON.REPLACE <- ifelse(analytical_postopv2$`ERYTHROPOIESIS-STIMULATING AGENTS` == 1|
                                             analytical_postopv2$`IRON REPLACEMENT` == 1, 1, 0)
analytical_postopv2$URINE <- ifelse(analytical_postopv2$`ELECTROLYTES RANDOM URINE` == 1|
                                      analytical_postopv2$`SODIUM RANDOM URINE` == 1|#
                                      analytical_postopv2$`UREA RANDOM URINE` == 1, 1, 0)#
analytical_postopv2$CT_AP <- ifelse(analytical_postopv2$`.CT ABD/PELVIS WWO CONTRAST UROGRAM` == 1|
                                      analytical_postopv2$`ABN ONLY CT ABD & PELVIS W/O CONTRAST` == 1|
                                      analytical_postopv2$`ABN ONLY CT ABD&PELV 1+ SECTION/REGNS`== 1|
                                      analytical_postopv2$`CT ABD W/WO, PELVIS WITH CONT` == 1|
                                      analytical_postopv2$`CT ABD/PELVIS W CONTRAST` == 1|
                                      analytical_postopv2$`CT ABD/PELVIS W/WO CONTRAST` == 1|
                                      analytical_postopv2$`CT ABD/PELVIS WITHOUT CONTRAST` == 1|
                                      analytical_postopv2$`CT ANGIO CHEST/ABD/PELVIS` == 1|
                                      analytical_postopv2$`CT CHEST/ABD W CONTRAST` == 1|
                                      analytical_postopv2$`CT CHEST/ABD W/WO CONTRAST` == 1|
                                      analytical_postopv2$`CT CHEST/ABD WO CONTRAST` == 1|
                                      analytical_postopv2$`CT CHEST/ABD/PELV W CONTRAST` == 1|
                                      analytical_postopv2$`CT CHEST/ABD/PELV W/WO CONTRAST` == 1|
                                      analytical_postopv2$`CT CHEST/ABD/PELV WO CONTRAST` == 1|
                                      analytical_postopv2$`CT CHEST/PELV W, ABD W/WO CONTRAST` == 1|
                                      analytical_postopv2$`PR CT ANGIO ABD&PLVIS CNTRST MTRL W/WO CNTRST IMGES` == 1|
                                      analytical_postopv2$`PR CT ANGIO, ABD, COMBO,INCL IMAGE PROC` == 1|
                                      analytical_postopv2$`CT ABDOMEN PELVIS LIVER MASS W/WO CONTRAST` == 1|
                                      analytical_postopv2$`CT ANGIO ABDOMEN/PELVIS - BODY` == 1|
                                      analytical_postopv2$`CT ANGIOGRAPHY PELVIS` == 1|
                                      analytical_postopv2$`CT CHEST ABDOMEN W/WO PELVIS W CONTRAST` == 1|
                                      analytical_postopv2$`CT CTA PULMONARY, ABDOMEN W, PELVIS W` == 1|
                                      analytical_postopv2$`CT PELVIS W/O CONTRAST`== 1|
                                      analytical_postopv2$`CT PELVIS W/O CONTRAST-MUSC` == 1|
                                      analytical_postopv2$`CT PELVIS W/WO CONTRAST` == 1|
                                      analytical_postopv2$`CT PELVIS WITH CONTRAST` == 1|
                                      analytical_postopv2$`CT PELVIS WITH CONTRAST-MUSC` == 1, 1, 0)
analytical_postopv2$dialysis <- ifelse(analytical_postopv2$HEMODIALYSIS == 1|
                                         analytical_postopv2$`INSERTION PERITONEAL DIALYSIS CATHETER`== 1|
                                         analytical_postopv2$`PERITONEAL DIALYSIS` == 1|
                                         analytical_postopv2$`PR DIALYSIS PROCEDURE` == 1|
                                         analytical_postopv2$`PR HEMODIALYSIS PROCEDURE W/ PHYS/QHP EVALUATION` == 1|
                                         analytical_postopv2$`PR UNSCHED DIALYSIS ESRD PT HOS` == 1|
                                         analytical_postopv2$`DIALYSIS SOLUTIONS`==1, 1, 0)
analytical_postopv2$VT <- ifelse(analytical_postopv2$`415.11` == 1|
                                   analytical_postopv2$`452` == 1|
                                   analytical_postopv2$`452.2` == 1, 1, 0)
analytical_postopv2$Anticoagulants <- ifelse(analytical_postopv2$`PR INJ ENOXAPARIN SODIUM 10 MG` == 1|
                                               analytical_postopv2$`ANTICOAGULANTS,COUMARIN TYPE` == 1|
                                               analytical_postopv2$`HEPARIN AND RELATED PREPARATIONS` == 1, 1, 0)
analytical_postopv2$bloodtrans <- ifelse(analytical_postopv2$`PACKED CELL TRANSFUSION` == 1|
                                           analytical_postopv2$`TRANSFUSE RED BLOOD CELLS` == 1|
                                           analytical_postopv2$`Transfusion of Nonautologous Red Blood Cells into Peripheral Vein, Percutaneous Approach` == 1|
                                           coalesce(analytical_postopv2$`RBC UNIT`,0) == 1, 1, 0)# can't find RBC UNIT in 2024 data
analytical_postopv2$antibiotics <- ifelse(analytical_postopv2$`ABSORBABLE SULFONAMIDE ANTIBACTERIAL AGENTS` == 1|
                                            analytical_postopv2$`NITROFURAN DERIVATIVES ANTIBACTERIAL AGENTS` == 1|
                                            analytical_postopv2$`PENICILLIN ANTIBIOTICS` == 1|
                                            analytical_postopv2$`QUINOLONE ANTIBIOTICS` == 1, 1, 0)



saveRDS(analytical_postopv2, paste0(fp_processed,
                                    "analytical_postopv2.rds"))




