# TODO: load model 
# Define variables to preserve
vars_to_keep <- c("pipeline_dir", "source_if_exists")

# Remove all objects except those in vars_to_keep
rm(list = setdiff(ls(), vars_to_keep))

# Rest of your script...
source("./global_vars.R")

# preop rate
SSI_coef_preop <- as.data.frame(read.csv(file = "../data/model/SSI_coef_20220906.csv",check.names=FALSE))
UTI_coef_preop <- as.data.frame(read.csv(file = "../data/model/UTI_coef_2022_0906.csv",check.names=FALSE))
SYSEP_coef_preop <- as.data.frame(read.csv(file = "../data/model/SYSEP_coef_20220906_est.csv",check.names=FALSE))
PNEU_coef_preop <- as.data.frame(read.csv(file = "../data/model/PNEU_coef_20220906.csv",check.names=FALSE))
load("../data/model/glmSSI.rdata")
# View(glmSSI)
# glimpse(glmSSI)
load("../data/model/glmUTI.rdata")
load("../data/model/glmSYSEP.rdata")
load("../data/model/glmPNEU.rdata")

cardiac_coef_preop <- as.data.frame(fread(file = "model/preop/cardiac_pre.csv",check.names=FALSE))
morb_coef_preop <- as.data.frame(fread(file = "model/preop/morb_pre.csv",check.names=FALSE))
nothome_coef_preop <- as.data.frame(fread(file = "model/preop/nothome_pre.csv",check.names=FALSE))
pulmonary_coef_preop <- as.data.frame(fread(file = "model/preop/pulmonary_pre.csv",check.names=FALSE))
renal_coef_preop <- as.data.frame(fread(file = "model/preop/renal_pre.csv",check.names=FALSE))
upradmin_coef_preop <- as.data.frame(fread(file = "model/preop/unplan_pre.csv",check.names=FALSE))
VTE_coef_preop <- as.data.frame(fread(file = "model/preop/VTE_pre.csv",check.names=FALSE))
mort_coef_preop <- as.data.frame(fread(file = "model/preop/mort_pre.csv",check.names=FALSE))
bleed_coef_preop <- as.data.frame(fread(file = "model/preop/bleed_pre.csv",check.names=FALSE))

# postop rate
SSI_coef_postop <- as.data.frame(read.csv(file = "../data/model/SSI_coefficients_1220.csv",check.names=FALSE))
UTI_coef_postop <- as.data.frame(read.csv(file = "../data/model/UTI_coefficients_1220.csv",check.names=FALSE))
SYSEP_coef_postop <- as.data.frame(read.csv(file = "../data/model/SYSEP_coefficients_0127.csv",check.names=FALSE))
PNEU_coef_postop <- as.data.frame(read.csv(file = "../data/model/PNEU_coefficients_1220.csv",check.names=FALSE))

cardiac_coef_postop <- as.data.frame(fread(file = "model/postop/cardiac_post.csv",check.names=FALSE))
morb_coef_postop <- as.data.frame(fread(file = "model/postop/morb_post.csv",check.names=FALSE))
nothome_coef_postop <- as.data.frame(fread(file = "model/postop/nothome_post.csv",check.names=FALSE))
pulmonary_coef_postop <- as.data.frame(fread(file = "model/postop/pulmonary_post.csv",check.names=FALSE))
renal_coef_postop <- as.data.frame(fread(file = "model/postop/renal_post.csv",check.names=FALSE))
upradmin_coef_postop <- as.data.frame(fread(file = "model/postop/upradmin_post.csv",check.names=FALSE))
VTE_coef_postop <- as.data.frame(fread(file = "model/postop/VTE_post.csv",check.names=FALSE))
bleed_coef_postop <- as.data.frame(fread(file = "model/postop/bleed_post.csv",check.names=FALSE))

# TODO: load datasets for predicted prob

analytical_preopv2 <- readRDS(paste0(fp_processed,
                                   "analytical_preopv2.rds"))

analytical_postopv2 <- readRDS(paste0(fp_processed,
                                    "analytical_postopv2.rds"))

#####################################################################################
## Preop rate

#fitSSI <- predict(glmSSI, analytical_preopv2, type="response")

analytical_preopv2 <- dummy_cols(analytical_preopv2, select_columns = "windex",remove_selected_columns = FALSE)
analytical_preopv2 <- dummy_cols(analytical_preopv2, select_columns = "windex2",remove_selected_columns = FALSE)
analytical_preopv2 <- dummy_cols(analytical_preopv2, select_columns = "WoundClass",remove_selected_columns = FALSE)
analytical_preopv2 <- dummy_cols(analytical_preopv2, select_columns = "ASA_class_epic_trans",remove_selected_columns = FALSE)

SSI_coef_preop <- tibble(Estimate = glmSSI$coefficients)
#SSI_coef_preop
analytical_preopv2$pred_prob_SSI_preop <- 1/(1+exp(-(SSI_coef_preop$Estimate[1]+
                                                       analytical_preopv2$`WoundClass_Clean Contaminated `*SSI_coef_preop$Estimate[2]+
                                                       analytical_preopv2$`WoundClass_Contaminated `*SSI_coef_preop$Estimate[3]+
                                                       analytical_preopv2$`WoundClass_Dirty or Infected `*SSI_coef_preop$Estimate[4]+
                                                       analytical_preopv2$`PrimarySurgeonSpecialty_Orthopedic Surgery`*SSI_coef_preop$Estimate[5]+
                                                       analytical_preopv2$SurgeryInpatientOrOutpatient_Outpatient*SSI_coef_preop$Estimate[6]+ 
                                                       analytical_preopv2$`OS CT BODY COMPARE-NO READ`*SSI_coef_preop$Estimate[7]+
                                                       analytical_preopv2$`80`*SSI_coef_preop$Estimate[8]+
                                                       analytical_preopv2$comb_560*SSI_coef_preop$Estimate[9]+
                                                       analytical_preopv2$blood_cult_comb*SSI_coef_preop$Estimate[10]
))) # no estimate from 7 - 10?

#all.equal(as.numeric(analytical_preopv2$pred_prob_SSI_preop),as.numeric(fitSSI))

################################################
# View(glmUTI)
## TODO: UTI preop
# test_uti <- analytical_preopv2
# #table(test_uti$Sex_Male)
# test_uti$Sex <- ifelse(test_uti$Sex_Male==1,"Male","Female")
# 
# fitUTI <- predict(glmUTI, test_uti, type="response")
# 
# test_diff <- as.numeric(analytical_preopv2$pred_prob_UTI_preop) - as.numeric(fitUTI)


analytical_preopv2$pred_prob_UTI_preop <- 1/(1+exp(-(UTI_coef_preop$Estimate[1]+
                                                       analytical_preopv2$Sex_Male*UTI_coef_preop$Estimate[2]+
                                                       analytical_preopv2$PrimarySurgeonSpecialty_Gynecology*UTI_coef_preop$Estimate[3]+
                                                       analytical_preopv2$PrimarySurgeonSpecialty_Urology*UTI_coef_preop$Estimate[4]+
                                                       analytical_preopv2$SurgeryInpatientOrOutpatient_Inpatient*UTI_coef_preop$Estimate[5]+
                                                       analytical_preopv2$`OS CT BODY COMPARE-NO READ`*UTI_coef_preop$Estimate[6]+
                                                       analytical_preopv2$`591`*UTI_coef_preop$Estimate[7])))

################################################
## TODO: SYSEP preop
# View(glmSYSEP)

#fitSYSEP <- predict(glmSYSEP, analytical_preopv2, type="response")

analytical_preopv2$pred_prob_SYSEP_preop <- 1/(1+exp(-(SYSEP_coef_preop$Estimate[1]+
                                                         analytical_preopv2$`WoundClass_Clean Contaminated `*SYSEP_coef_preop$Estimate[2]+
                                                         analytical_preopv2$`WoundClass_Contaminated `*SYSEP_coef_preop$Estimate[3]+
                                                         analytical_preopv2$`WoundClass_Dirty or Infected `*SYSEP_coef_preop$Estimate[4]+
                                                         analytical_preopv2$ASA_class_epic_trans_3*SYSEP_coef_preop$Estimate[5]+
                                                         analytical_preopv2$ASA_class_epic_trans_4or5*SYSEP_coef_preop$Estimate[6]+
                                                         analytical_preopv2$`PrimarySurgeonSpecialty_Orthopedic Surgery`*SYSEP_coef_preop$Estimate[7]+
                                                         analytical_preopv2$SurgeryInpatientOrOutpatient_Outpatient*SYSEP_coef_preop$Estimate[8]+
                                                         analytical_preopv2$`567`*SYSEP_coef_preop$Estimate[9]+
                                                         analytical_preopv2$`994`*SYSEP_coef_preop$Estimate[10]
)))

# all.equal(as.numeric(analytical_preopv2$pred_prob_SYSEP_preop),as.numeric(fitSYSEP))
#test_diff <- as.numeric(analytical_preopv2$pred_prob_SYSEP_preop) - as.numeric(fitSYSEP)

# table(analytical_preopv2$SurgeryInpatientOrOutpatient, useNA = "ifany")

# table(analytical_preopv2$`PrimarySurgeonSpecialty_Orthopedic Surgery`, 
#       useNA = "ifany")


# table(analytical_preopv2$ASA_class_epic_trans , useNA = "ifany")

# table(analytical_preopv2$`OS CT BODY COMPARE-NO READ`, useNA = "ifany")
# table(analytical_preopv2$`80`, useNA = "ifany")
# table(analytical_preopv2$comb_560, useNA = "ifany")

################################################
## TODO: PNEU preop
# View(glmPNEU)
#fitPNEU <- predict(glmPNEU, analytical_preopv2, type="response")


analytical_preopv2$pred_prob_PNEU_preop <- 1/(1+exp(-(PNEU_coef_preop$Estimate[1]+
                                                        analytical_preopv2$`windex_1-2`*PNEU_coef_preop$Estimate[2]+
                                                        analytical_preopv2$`windex_>=3`*PNEU_coef_preop$Estimate[3]+
                                                        analytical_preopv2$ASA_class_epic_trans_3*PNEU_coef_preop$Estimate[4]+
                                                        analytical_preopv2$ASA_class_epic_trans_4or5*PNEU_coef_preop$Estimate[5]+
                                                        analytical_preopv2$SurgeryInpatientOrOutpatient_Outpatient*PNEU_coef_preop$Estimate[6]+
                                                        analytical_preopv2$`150`*PNEU_coef_preop$Estimate[7]+
                                                        analytical_preopv2$`480`*PNEU_coef_preop$Estimate[8]+
                                                        analytical_preopv2$`501`*PNEU_coef_preop$Estimate[9]
)))
#all.equal(as.numeric(analytical_preopv2$pred_prob_PNEU_preop),as.numeric(fitPNEU))
#test_diff <- as.numeric(analytical_preopv2$pred_prob_PNEU_preop) - as.numeric(fitPNEU)
################################################################################
####### TODO: other comps
analytical_preopv2$pred_prob_cardiac_preop <- 1/(1+exp(-(cardiac_coef_preop$x[1]+
                                                     analytical_preopv2$ASA_class_epic_trans_3*cardiac_coef_preop$x[2]+
                                                     analytical_preopv2$ASA_class_epic_trans_4or5*cardiac_coef_preop$x[3]+
                                                     analytical_preopv2$SurgeryInpatientOrOutpatient_Outpatient*cardiac_coef_preop$x[4]+
                                                     analytical_preopv2$`PR NONINVASV EXTREM EXAM,MULT,BILAT`*cardiac_coef_preop$x[5]+
                                                     analytical_preopv2$STRESS.NUCLEAR*cardiac_coef_preop$x[6]+
                                                     coalesce(analytical_preopv2$`ANTI-OBESITY - ANOREXIC AGENTS`)*cardiac_coef_preop$x[7]+# couldn't find this
                                                     analytical_preopv2$`ANTIBODY PATIENT INTERPS 1-5`*cardiac_coef_preop$x[8]+
                                                     analytical_preopv2$POTASSIUM*cardiac_coef_preop$x[9]+
                                                     analytical_preopv2$`276.12`*cardiac_coef_preop$x[10]+
                                                     analytical_preopv2$`394.7`*cardiac_coef_preop$x[11]+
                                                     analytical_preopv2$`411.2`*cardiac_coef_preop$x[12]+
                                                     analytical_preopv2$`427.4`*cardiac_coef_preop$x[13]+
                                                     analytical_preopv2$`429.9`*cardiac_coef_preop$x[14]+
                                                     analytical_preopv2$`444.1`*cardiac_coef_preop$x[15]+
                                                     analytical_preopv2$`458.2`*cardiac_coef_preop$x[16]+
                                                     analytical_preopv2$`681.6`*cardiac_coef_preop$x[17]+
                                                     analytical_preopv2$`720.1`*cardiac_coef_preop$x[18]+
                                                     analytical_preopv2$`797`*cardiac_coef_preop$x[19]+
                                                     analytical_preopv2$`994.21`*cardiac_coef_preop$x[20]
)))

analytical_preopv2$pred_prob_morb_preop <- 1/(1+exp(-(morb_coef_preop$x[1]+
                                                  analytical_preopv2$`windex2_1-2`*morb_coef_preop$x[2]+
                                                  analytical_preopv2$`windex2_3-4`*morb_coef_preop$x[3]+#
                                                  analytical_preopv2$`windex2_>=5`*morb_coef_preop$x[4]+#
                                                  analytical_preopv2$ASA_class_epic_trans_3*morb_coef_preop$x[5]+
                                                  analytical_preopv2$ASA_class_epic_trans_4or5*morb_coef_preop$x[6]+
                                                  analytical_preopv2$`WoundClass_Clean Contaminated `*morb_coef_preop$x[7]+
                                                  analytical_preopv2$`WoundClass_Contaminated `*morb_coef_preop$x[8]+
                                                  analytical_preopv2$`WoundClass_Dirty or Infected ` *morb_coef_preop$x[9]+
                                                  analytical_preopv2$`PrimarySurgeonSpecialty_Orthopedic Surgery`*morb_coef_preop$x[10]+
                                                  analytical_preopv2$SurgeryInpatientOrOutpatient_Outpatient*morb_coef_preop$x[11]+
                                                  analytical_preopv2$CT.BODY.CHEST.PELV.W.ABD*morb_coef_preop$x[12]+
                                                  analytical_preopv2$`285`*morb_coef_preop$x[13]
)))

analytical_preopv2$pred_prob_nothome_preop <- 1/(1+exp(-(nothome_coef_preop$x[1]+
                                                     analytical_preopv2$Sex_Male*nothome_coef_preop$x[2]+
                                                     analytical_preopv2$`windex2_1-2`*nothome_coef_preop$x[3]+
                                                     analytical_preopv2$`windex2_3-4`*nothome_coef_preop$x[4]+#
                                                     analytical_preopv2$`windex2_>=5`*nothome_coef_preop$x[5]+#
                                                     analytical_preopv2$interger_age*nothome_coef_preop$x[6]+
                                                     analytical_preopv2$ASA_class_epic_trans_3*nothome_coef_preop$x[7]+
                                                     analytical_preopv2$ASA_class_epic_trans_4or5*nothome_coef_preop$x[8]+
                                                     analytical_preopv2$`PrimarySurgeonSpecialty_Neurological Surgery`*nothome_coef_preop$x[9]+
                                                     analytical_preopv2$`PrimarySurgeonSpecialty_Orthopedic Surgery`*nothome_coef_preop$x[10]+
                                                     analytical_preopv2$SurgeryInpatientOrOutpatient_Outpatient*nothome_coef_preop$x[11]
)))

analytical_preopv2$pred_prob_pulmonary_preop <- 1/(1+exp(-(pulmonary_coef_preop$x[1]+
                                                       analytical_preopv2$`windex2_1-2`*pulmonary_coef_preop$x[2]+
                                                       analytical_preopv2$`windex2_3-4`*pulmonary_coef_preop$x[3]+#
                                                       analytical_preopv2$`windex2_>=5`*pulmonary_coef_preop$x[4]+#
                                                       analytical_preopv2$interger_age*pulmonary_coef_preop$x[5]+
                                                       analytical_preopv2$ASA_class_epic_trans_3*pulmonary_coef_preop$x[6]+
                                                       analytical_preopv2$ASA_class_epic_trans_4or5*pulmonary_coef_preop$x[7]+
                                                       analytical_preopv2$`PrimarySurgeonSpecialty_Orthopedic Surgery`*pulmonary_coef_preop$x[8]+
                                                       analytical_preopv2$SurgeryInpatientOrOutpatient_Outpatient*pulmonary_coef_preop$x[9]+
                                                       analytical_preopv2$`150`*pulmonary_coef_preop$x[10]+
                                                       analytical_preopv2$`509.1`*pulmonary_coef_preop$x[11]+
                                                       analytical_preopv2$`740`*pulmonary_coef_preop$x[12]+
                                                       analytical_preopv2$`994.2`*pulmonary_coef_preop$x[13]
)))

analytical_preopv2$pred_prob_renal_preop <- 1/(1+exp(-(renal_coef_preop$x[1]+
                                                   analytical_preopv2$`windex2_1-2`*renal_coef_preop$x[2]+
                                                   analytical_preopv2$`windex2_3-4`*renal_coef_preop$x[3]+#
                                                   analytical_preopv2$`windex2_>=5`*renal_coef_preop$x[4]+#
                                                   analytical_preopv2$CENTRAL.LINE*renal_coef_preop$x[5]+
                                                   analytical_preopv2$`PR SPCL STN 2 I&R EXCPT MICROORG/ENZYME/IMCYT&IMHIS`*renal_coef_preop$x[6]+
                                                   analytical_preopv2$`251.1`*renal_coef_preop$x[7]+
                                                   analytical_preopv2$fluid_elec_acid_disorders*renal_coef_preop$x[8]+
                                                   analytical_preopv2$renal_failure*renal_coef_preop$x[9]+
                                                   analytical_preopv2$`290.2`*renal_coef_preop$x[10]+
                                                   analytical_preopv2$`797`*renal_coef_preop$x[11]
)))

analytical_preopv2$pred_prob_upradmin_preop <- 1/(1+exp(-(upradmin_coef_preop$x[1]+
                                                      analytical_preopv2$`windex2_1-2`*upradmin_coef_preop$x[2]+
                                                      analytical_preopv2$`windex2_3-4`*upradmin_coef_preop$x[3]+#
                                                      analytical_preopv2$`windex2_>=5`*upradmin_coef_preop$x[4]+#
                                                      analytical_preopv2$ASA_class_epic_trans_3*upradmin_coef_preop$x[5]+
                                                      analytical_preopv2$ASA_class_epic_trans_4or5*upradmin_coef_preop$x[6]+
                                                      analytical_preopv2$SurgeryInpatientOrOutpatient_Outpatient*upradmin_coef_preop$x[7]+
                                                      analytical_preopv2$`CLOS LARGE BOWEL BIOPSY`*upradmin_coef_preop$x[8]+
                                                      analytical_preopv2$CT.BODY.CHEST.PELV.W.ABD*upradmin_coef_preop$x[9]+
                                                      analytical_preopv2$`PR LORAZEPAM INJECTION 2 MG`*upradmin_coef_preop$x[10]+
                                                      analytical_preopv2$`UPDATE PATIENT CLASS I.E. INPATIENT / OBSERVATION`*upradmin_coef_preop$x[11]+
                                                      coalesce(analytical_preopv2$`ANTIHYPERTENSIVES, ACE INHIBITORS`,0)*upradmin_coef_preop$x[12]+# couldn't find this
                                                      coalesce(analytical_preopv2$`FOLIC ACID PREPARATIONS`,0)*upradmin_coef_preop$x[13]+# couldn't find this
                                                      coalesce(analytical_preopv2$`NOSE PREPARATIONS, VASOCONSTRICTORS(OTC)`,0)*upradmin_coef_preop$x[14]+# couldn't find this
                                                      analytical_preopv2$`345.1`*upradmin_coef_preop$x[15]+
                                                      analytical_preopv2$`386.9`*upradmin_coef_preop$x[16]+
                                                      analytical_preopv2$`577.2`*upradmin_coef_preop$x[17]+
                                                      analytical_preopv2$`599.8`*upradmin_coef_preop$x[18]
)))


analytical_preopv2$pred_prob_VTE_preop <- 1/(1+exp(-(VTE_coef_preop$x[1]+
                                                 analytical_preopv2$interger_age*VTE_coef_preop$x[2]+
                                                 analytical_preopv2$SurgeryInpatientOrOutpatient_Outpatient*VTE_coef_preop$x[3]+
                                                 analytical_preopv2$VT*VTE_coef_preop$x[4]+
                                                 analytical_preopv2$`281.12`*VTE_coef_preop$x[5]+
                                                 analytical_preopv2$`286.2`*VTE_coef_preop$x[6]+
                                                 analytical_preopv2$`386.9`*VTE_coef_preop$x[7]+
                                                 analytical_preopv2$`415`*VTE_coef_preop$x[8]
)))

analytical_preopv2$pred_prob_mort_preop <- 1/(1+exp(-(mort_coef_preop$x[1]+
                                                  analytical_preopv2$interger_age*mort_coef_preop$x[2]+
                                                  analytical_preopv2$ASA_class_epic_trans_3*mort_coef_preop$x[3]+
                                                  analytical_preopv2$ASA_class_epic_trans_4or5*mort_coef_preop$x[4]+
                                                  analytical_preopv2$`198.3`*mort_coef_preop$x[5]+
                                                  analytical_preopv2$`198.6`*mort_coef_preop$x[6]+
                                                  analytical_preopv2$`276`*mort_coef_preop$x[7]+
                                                  analytical_preopv2$`290.1`*mort_coef_preop$x[8]+
                                                  analytical_preopv2$`427.2`*mort_coef_preop$x[9]
)))

analytical_preopv2$pred_prob_bleed_preop <- 1/(1+exp(-(bleed_coef_preop$x[1]+
                                                   analytical_preopv2$`windex2_1-2`*bleed_coef_preop$x[2]+
                                                   analytical_preopv2$`windex2_3-4`*bleed_coef_preop$x[3]+#
                                                   analytical_preopv2$`windex2_>=5`*bleed_coef_preop$x[4]+#
                                                   analytical_preopv2$interger_age*bleed_coef_preop$x[5]+
                                                   analytical_preopv2$ASA_class_epic_trans_3*bleed_coef_preop$x[6]+
                                                   analytical_preopv2$ASA_class_epic_trans_4or5*bleed_coef_preop$x[7]+
                                                   analytical_preopv2$SurgeryInpatientOrOutpatient_Outpatient*bleed_coef_preop$x[8]+
                                                   analytical_preopv2$`OS CT BODY COMPARE-NO READ`*bleed_coef_preop$x[9]+
                                                   analytical_preopv2$anemia*bleed_coef_preop$x[10]+
                                                   analytical_preopv2$`737`*bleed_coef_preop$x[11]
)))

################################################################################
####### TODO: SAVE preop pred_prob
# saveRDS(ICD10_uni_surg_id_spec, paste0(fp_processed,
#                                        "ICD10_uni_surg_id_spec.rds"))

# View(analytical_preopv2)
# sum(is.na(analytical_preopv2$pred_prob_SSI_preop ))
# sum(is.na(analytical_preopv2$pred_prob_UTI_preop))
# sum(is.na(analytical_preopv2$pred_prob_SYSEP_preop ))
# sum(is.na(analytical_preopv2$pred_prob_PNEU_preop))

write.csv(analytical_preopv2,
          file = paste0(fp_processed, "preop_expected_rate.csv"),
          row.names=FALSE)


########################################################################
## TODO: pred_prob_SSI
analytical_postopv2$pred_prob_SSI <- 1/(1+exp(-(SSI_coef_postop$x[1]+
                                                  analytical_postopv2$`80`*SSI_coef_postop$x[2]+
                                                  analytical_postopv2$`1011`*SSI_coef_postop$x[3]+
                                                  analytical_postopv2$AntiBiotics_YN*SSI_coef_postop$x[4]+
                                                  analytical_postopv2$blood_cult_comb*SSI_coef_postop$x[5])))

########################################################################
## TODO: pred_prob_UTI
analytical_postopv2$pred_prob_UTI <- 1/(1+exp(-(UTI_coef_postop$x[1]+
                                                  analytical_postopv2$`590`*UTI_coef_postop$x[2]+
                                                  analytical_postopv2$`591`*UTI_coef_postop$x[3]+
                                                  analytical_postopv2$`592`*UTI_coef_postop$x[4]+
                                                  analytical_postopv2$AntiBiotics_YN*UTI_coef_postop$x[5]+
                                                  analytical_postopv2$Urine_cult_comb*UTI_coef_postop$x[6]+
                                                  # Problem 
                                                  analytical_postopv2$c_diff_comb*UTI_coef_postop$x[7]+
                                                  analytical_postopv2$comb_599*UTI_coef_postop$x[8])))

########################################################################
## TODO: pred_prob_SYSEP

analytical_postopv2$pred_prob_SYSEP <- 1/(1+exp(-(SYSEP_coef_postop$x[1]+
                                                    analytical_postopv2$AntiBiotics_YN*SYSEP_coef_postop$x[2]+
                                                    analytical_postopv2$`MAGNESIUM SERUM`*SYSEP_coef_postop$x[3]+
                                                    analytical_postopv2$comb_540*SYSEP_coef_postop$x[5]+
                                                    analytical_postopv2$comb_994*SYSEP_coef_postop$x[6]+
                                                    analytical_postopv2$CBC_auto_diff_comb*SYSEP_coef_postop$x[7]+
                                                    analytical_postopv2$blood_cult_comb*SYSEP_coef_postop$x[8])))

########################################################################
## TODO: pred_prob_PNEU
#analytical_postopv2$`1013` <- 0
analytical_postopv2$pred_prob_PNEU <- 1/(1+exp(-(PNEU_coef_postop$x[1]+
                                                   analytical_postopv2$`480`*PNEU_coef_postop$x[2]+
                                                   analytical_postopv2$`501`*PNEU_coef_postop$x[3]+
                                                   # Problem 
                                                   analytical_postopv2$`1013`*PNEU_coef_postop$x[4]+
                                                   analytical_postopv2$AntiBiotics_YN*PNEU_coef_postop$x[5]+
                                                   analytical_postopv2$`MAGNESIUM SERUM`*PNEU_coef_postop$x[6]+
                                                   analytical_postopv2$`VANCOMYCIN TROUGH`*PNEU_coef_postop$x[7]+
                                                   analytical_postopv2$respiratory_comb*PNEU_coef_postop$x[8]+
                                                   analytical_postopv2$BLOOD_GASES_comb*PNEU_coef_postop$x[9])))

#colnames(analytical_postopv2)
#showdf <- analytical_postopv2[,c("SurgeryID","pred_prob_SSI","PrimarySurgeonSpecialty")]
analytical_postopv2$pred_prob_cardiac <- 1/(1+exp(-(cardiac_coef_postop$x[1]+
                                                      analytical_postopv2$`342`*cardiac_coef_postop$x[2]+
                                                      analytical_postopv2$`394.7`*cardiac_coef_postop$x[3]+
                                                      analytical_postopv2$Coronary.artery.disease*cardiac_coef_postop$x[4]+
                                                      analytical_postopv2$arrythmias*cardiac_coef_postop$x[5]+
                                                      analytical_postopv2$`429`*cardiac_coef_postop$x[6]+
                                                      analytical_postopv2$sepshock*cardiac_coef_postop$x[7]+
                                                      analytical_postopv2$`CPAP OVERNIGHT`*cardiac_coef_postop$x[8]+
                                                      analytical_postopv2$`echo`*cardiac_coef_postop$x[9]+
                                                      analytical_postopv2$`INSERT ARTERIAL LINE`*cardiac_coef_postop$x[10]+
                                                      analytical_postopv2$Blood.products*cardiac_coef_postop$x[11]+
                                                      analytical_postopv2$`XR CHEST 2 VIEW (PA,LAT)`*cardiac_coef_postop$x[12]+
                                                      analytical_postopv2$`ADRENERGIC AGENTS,CATECHOLAMINES`*cardiac_coef_postop$x[13]+
                                                      analytical_postopv2$`ANTIDIURETIC AND VASOPRESSOR HORMONES`*cardiac_coef_postop$x[14]+
                                                      analytical_postopv2$`BETA-ADRENERGIC AGENTS`*cardiac_coef_postop$x[15]+
                                                      analytical_postopv2$`BICARBONATE PRODUCING/CONTAINING AGENTS`*cardiac_coef_postop$x[16]+
                                                      analytical_postopv2$`CKMB PANEL`*cardiac_coef_postop$x[17]+
                                                      analytical_postopv2$`POINT OF CARE TESTS`*cardiac_coef_postop$x[18]+
                                                      analytical_postopv2$troponin*cardiac_coef_postop$x[19])))

analytical_postopv2$pred_prob_morb <- 1/(1+exp(-(morb_coef_postop$x[1]+
                                                   analytical_postopv2$`80`*morb_coef_postop$x[2]+
                                                   analytical_postopv2$VT*morb_coef_postop$x[3]+
                                                   analytical_postopv2$`591`*morb_coef_postop$x[4]+
                                                   analytical_postopv2$`599.3`*morb_coef_postop$x[5]+
                                                   analytical_postopv2$`INCISION AND DRAINAGE HEAD/NECK`*morb_coef_postop$x[6]+
                                                   analytical_postopv2$`LAST TYPE & SCREEN (CLOT) STATUS`*morb_coef_postop$x[7]+
                                                   analytical_postopv2$bloodtrans*morb_coef_postop$x[8]+
                                                   analytical_postopv2$`PR BACTERIA IDENTIFICATION, AEROBIC ISOLATE`*morb_coef_postop$x[9]+
                                                   analytical_postopv2$antibiotics*morb_coef_postop$x[10]+
                                                   analytical_postopv2$`HEPARIN AND RELATED PREPARATIONS`*morb_coef_postop$x[11]+
                                                   analytical_postopv2$`AEROBIC CULTURE (EG: TISSUE, ABSCESS, WOUND, SINUS, ETC)`*morb_coef_postop$x[12]+
                                                   analytical_postopv2$`ARTERIAL BLOOD GAS`*morb_coef_postop$x[13]+
                                                   analytical_postopv2$`BASIC METABOLIC PANEL`*morb_coef_postop$x[14]+
                                                   analytical_postopv2$`CBC NO AUTO DIFF`*morb_coef_postop$x[15]+
                                                   analytical_postopv2$`POCT CRITICAL PANEL`*morb_coef_postop$x[16]+
                                                   analytical_postopv2$`URINE CULTURE`*morb_coef_postop$x[17])))

analytical_postopv2$pred_prob_nothome <- 1/(1+exp(-(nothome_coef_postop$x[1]+
                                                      analytical_postopv2$`285`*nothome_coef_postop$x[2]+
                                                      analytical_postopv2$Orthopedic.problems*nothome_coef_postop$x[3]+
                                                      analytical_postopv2$`ADMIT PATIENT`*nothome_coef_postop$x[4]+
                                                      analytical_postopv2$`FEMORAL NERVE BLOCK`*nothome_coef_postop$x[5]+
                                                      analytical_postopv2$Social.planning*nothome_coef_postop$x[6]+
                                                      analytical_postopv2$Discharge.planning*nothome_coef_postop$x[7]+
                                                      analytical_postopv2$`PR INJ, PROPOFOL, 10 MG`*nothome_coef_postop$x[8]+
                                                      analytical_postopv2$`RETURN PATIENT`*nothome_coef_postop$x[9]+
                                                      analytical_postopv2$`GENERAL ANESTHETICS,INJECTABLE-BENZODIAZEPINE TYPE`*nothome_coef_postop$x[10]+
                                                      analytical_postopv2$`GLUCOCORTICOIDS`*nothome_coef_postop$x[11]+
                                                      analytical_postopv2$`LAXATIVES, LOCAL/RECTAL`*nothome_coef_postop$x[12]+
                                                      analytical_postopv2$`LOOP DIURETICS`*nothome_coef_postop$x[13]+
                                                      analytical_postopv2$`NSAIDS, CYCLOOXYGENASE INHIBITOR - TYPE ANALGESICS`*nothome_coef_postop$x[14]+
                                                      analytical_postopv2$`TOPICAL ANTIFUNGALS`*nothome_coef_postop$x[15]+
                                                      analytical_postopv2$`BASIC METABOLIC PANEL`*nothome_coef_postop$x[16]+
                                                      analytical_postopv2$`CBC NO AUTO DIFF`*nothome_coef_postop$x[17]+
                                                      analytical_postopv2$`HEMATOCRIT`*nothome_coef_postop$x[18]+
                                                      analytical_postopv2$`POCT GLUCOSE CPT 82962 INTERFACED RESULT/DOCKED DEVICE`*nothome_coef_postop$x[19])))#

analytical_postopv2$pred_prob_pulmonary <- 1/(1+exp(-(pulmonary_coef_postop$x[1]+
                                                        analytical_postopv2$`38`*pulmonary_coef_postop$x[2]+
                                                        analytical_postopv2$`480`*pulmonary_coef_postop$x[3]+
                                                        analytical_postopv2$`501`*pulmonary_coef_postop$x[4]+
                                                        analytical_postopv2$XRAY*pulmonary_coef_postop$x[5]+
                                                        analytical_postopv2$intubation*pulmonary_coef_postop$x[6]+
                                                        analytical_postopv2$`RESTRAINTS NON-BEHAVIORAL`*pulmonary_coef_postop$x[7]+
                                                        analytical_postopv2$`TRANSFER PATIENT`*pulmonary_coef_postop$x[8]+
                                                        analytical_postopv2$`ANTIEMETIC/ANTIVERTIGO AGENTS`*pulmonary_coef_postop$x[9]+
                                                        analytical_postopv2$`EXPECTORANTS`*pulmonary_coef_postop$x[10]+
                                                        analytical_postopv2$BLOOD.GAS*pulmonary_coef_postop$x[11]+
                                                        analytical_postopv2$`CONGESTIVE HEART FAILURE BNP`*pulmonary_coef_postop$x[12]+
                                                        analytical_postopv2$`D DIMER QUANTITATIVE`*pulmonary_coef_postop$x[13]+
                                                        analytical_postopv2$POINT.OF.CARE*pulmonary_coef_postop$x[14]+
                                                        analytical_postopv2$`RESPIRATORY CULTURE`*pulmonary_coef_postop$x[15]+
                                                        analytical_postopv2$`VANCOMYCIN TROUGH`*pulmonary_coef_postop$x[16])))

analytical_postopv2$pred_prob_renal <- 1/(1+exp(-(renal_coef_postop$x[1]+
                                                    analytical_postopv2$`276.13`*renal_coef_postop$x[2]+
                                                    analytical_postopv2$`442`*renal_coef_postop$x[3]+
                                                    analytical_postopv2$`559`*renal_coef_postop$x[4]+
                                                    analytical_postopv2$`585.1`*renal_coef_postop$x[5]+
                                                    analytical_postopv2$dialysis*renal_coef_postop$x[6]+
                                                    analytical_postopv2$`IP CONSULT TO PHARMACY - TPN`*renal_coef_postop$x[7]+
                                                    analytical_postopv2$`IP CONSULT TO WOUND / OSTOMY / SKIN TEAM`*renal_coef_postop$x[8]+
                                                    analytical_postopv2$`US RENAL (KIDNEYS/BLADDER ONLY)`*renal_coef_postop$x[9]+
                                                    analytical_postopv2$`US UPPER EXTREMITY VENOUS BIL`*renal_coef_postop$x[10]+
                                                    analytical_postopv2$`WEIGH PATIENT`*renal_coef_postop$x[11]+
                                                    analytical_postopv2$IRON.REPLACE*renal_coef_postop$x[12]+
                                                    analytical_postopv2$`ACUTE HEPATITIS PANEL`*renal_coef_postop$x[13]+
                                                    analytical_postopv2$URINE*renal_coef_postop$x[14]+
                                                    analytical_postopv2$`LACTATE WHOLE BLOOD`*renal_coef_postop$x[15]+
                                                    analytical_postopv2$`NT-PROBNP`*renal_coef_postop$x[16]+
                                                    analytical_postopv2$`RENAL FUNCTION PANEL`*renal_coef_postop$x[17]+#
                                                    analytical_postopv2$`VANCOMYCIN RANDOM`*renal_coef_postop$x[18])))#

analytical_postopv2$pred_prob_upradmin <- 1/(1+exp(-(upradmin_coef_postop$x[1]+
                                                       analytical_postopv2$`80`*upradmin_coef_postop$x[2]+
                                                       analytical_postopv2$`850`*upradmin_coef_postop$x[3]+
                                                       analytical_postopv2$`851`*upradmin_coef_postop$x[4]+
                                                       analytical_postopv2$`ADMIT PATIENT`*upradmin_coef_postop$x[5]+
                                                       analytical_postopv2$`PREADMISSION ADMIT ORDER - RN TO RELEASE`*upradmin_coef_postop$x[6]+
                                                       analytical_postopv2$`UPDATE PATIENT CLASS I.E. INPATIENT / OBSERVATION`*upradmin_coef_postop$x[7])))

analytical_postopv2$pred_prob_VTE <- 1/(1+exp(-(VTE_coef_postop$x[1]+
                                                  analytical_postopv2$VT*VTE_coef_postop$x[2]+
                                                  analytical_postopv2$`.XR CHEST SINGLE (PA)`*VTE_coef_postop$x[3]+
                                                  analytical_postopv2$`CTA CHEST FOR PE`*VTE_coef_postop$x[4]+
                                                  analytical_postopv2$`OPN RT HEMICOLECTOMY NEC`*VTE_coef_postop$x[5]+
                                                  analytical_postopv2$Anticoagulants*VTE_coef_postop$x[6]+
                                                  analytical_postopv2$`US UPPER EXTREMITY VENOUS UNILATERAL`*VTE_coef_postop$x[7]+
                                                  analytical_postopv2$`FOLIC ACID PREPARATIONS`*VTE_coef_postop$x[8])))

analytical_postopv2$pred_prob_bleed <- 1/(1+exp(-(bleed_coef_postop$x[1]+
                                                    analytical_postopv2$`285`*bleed_coef_postop$x[2]+
                                                    analytical_postopv2$`CALCIUM REPLACEMENT`*bleed_coef_postop$x[3]+
                                                    analytical_postopv2$`CBC NO AUTO DIFF`*bleed_coef_postop$x[4]+
                                                    analytical_postopv2$`LAST TYPE & SCREEN (CLOT) STATUS`*bleed_coef_postop$x[5]+
                                                    analytical_postopv2$`PLASMA EXPANDERS`*bleed_coef_postop$x[6]+
                                                    analytical_postopv2$`POCT CRITICAL PANEL`*bleed_coef_postop$x[7]+
                                                    analytical_postopv2$`TEG PANEL`*bleed_coef_postop$x[8]+
                                                    analytical_postopv2$red_blood_cell_or_whole_blood_transfusion*bleed_coef_postop$x[9])))

Predicted_prob <- analytical_postopv2[,c("SurgeryID","arb_person_id",
                                         "pred_prob_SSI","pred_prob_UTI","pred_prob_SYSEP","pred_prob_PNEU",
                                         "pred_prob_cardiac","pred_prob_morb","pred_prob_nothome","pred_prob_pulmonary",
                                         "pred_prob_renal","pred_prob_upradmin","pred_prob_VTE","pred_prob_bleed")]
# 
colnames(Predicted_prob) <- c("SurgeryID","arb_person_id",
                              "pred_prob_SSI_postop","pred_prob_UTI_postop","pred_prob_SYSEP_postop","pred_prob_PNEU_postop",
                              "pred_prob_cardiac_postop","pred_prob_morb_postop","pred_prob_nothome_postop","pred_prob_pulmonary_postop",
                              "pred_prob_renal_postop","pred_prob_upradmin_postop","pred_prob_VTE_postop","pred_prob_bleed_postop")


# glimpse(Predicted_prob)
################################################################################
###### postop_observed_rate
write.csv(Predicted_prob,
          file = paste0(fp_processed, "postop_observed_rate.csv"),
          row.names=FALSE)


