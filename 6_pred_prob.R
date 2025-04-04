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
# postop rate
SSI_coef_postop <- as.data.frame(read.csv(file = "../data/model/SSI_coefficients_1220.csv",check.names=FALSE))
UTI_coef_postop <- as.data.frame(read.csv(file = "../data/model/UTI_coefficients_1220.csv",check.names=FALSE))
SYSEP_coef_postop <- as.data.frame(read.csv(file = "../data/model/SYSEP_coefficients_0127.csv",check.names=FALSE))
PNEU_coef_postop <- as.data.frame(read.csv(file = "../data/model/PNEU_coefficients_1220.csv",check.names=FALSE))

# TODO: load datasets for predicted prob

analytical_preopv2 <- readRDS(paste0(fp_processed,
                                   "analytical_preopv2.rds"))

analytical_postopv2 <- readRDS(paste0(fp_processed,
                                    "analytical_postopv2.rds"))

#####################################################################################
## Preop rate

fitSSI <- predict(glmSSI, analytical_preopv2, type="response")

analytical_preopv2 <- dummy_cols(analytical_preopv2, select_columns = "windex",remove_selected_columns = FALSE)
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
test_uti <- analytical_preopv2
#table(test_uti$Sex_Male)
test_uti$Sex <- ifelse(test_uti$Sex_Male==1,"Male","Female")

fitUTI <- predict(glmUTI, test_uti, type="response")

test_diff <- as.numeric(analytical_preopv2$pred_prob_UTI_preop) - as.numeric(fitUTI)


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

fitSYSEP <- predict(glmSYSEP, analytical_preopv2, type="response")

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
test_diff <- as.numeric(analytical_preopv2$pred_prob_SYSEP_preop) - as.numeric(fitSYSEP)

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
fitPNEU <- predict(glmPNEU, analytical_preopv2, type="response")


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
test_diff <- as.numeric(analytical_preopv2$pred_prob_PNEU_preop) - as.numeric(fitPNEU)

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
analytical_postopv2$`1013` <- 0
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
Predicted_prob <- analytical_postopv2[,c("SurgeryID","arb_person_id","pred_prob_SSI","pred_prob_UTI","pred_prob_SYSEP","pred_prob_PNEU","PrimarySurgeonSpecialty.x")]

#View(Predicted_prob)
colnames(Predicted_prob) <- c("SurgeryID","arb_person_id","pred_prob_SSI_postop","pred_prob_UTI_postop","pred_prob_SYSEP_postop","pred_prob_PNEU_postop","PrimarySurgeonSpecialty_raw")
#rowSums(analytical_postopv2[,which(substring(colnames(analytical_postopv2),1,3)==599)])
#table(analytical_postopv2$comb_599 )
#table(rowSums(analytical_postopv2[,which(substring(colnames(analytical_postopv2),1,3)==599)]))

# glimpse(Predicted_prob)
################################################################################
###### postop_observed_rate
write.csv(Predicted_prob,
          file = paste0(fp_processed, "postop_observed_rate.csv"),
          row.names=FALSE)


