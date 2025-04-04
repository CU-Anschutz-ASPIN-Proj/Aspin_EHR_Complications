# source("./3_load_data_coefs.R")

library(lubridate)
library(tidyr)
library(plyr)
library(dplyr)
library(tidyverse)
library(fastDummies)

###########################################################################
# TODO: the following code is to get Post-op: final_data_store_diag_postop.csv

# Diag <- read.csv(file = "data/C2730_Table4_Diagnosis_20240223.csv")
# #Diag$DiagnosisDate_ad <- as.Date(Diag$DiagnosisDate)
# Epic_data <- ICD10_uni_surg_id_spec

diag_cohort <- Diag[Diag$arb_person_id %in% Epic_data$arb_person_id,]

# phe_code <- read.table(file = "data/ICDMAP.txt",header = TRUE)

diag_cohort$ICD <- diag_cohort$DiagnosisCode
phe_code$flag <- as.factor(phe_code$flag)
phe_code[is.na(phe_code$flag)] -> "10"
comp_merge <- merge(diag_cohort,phe_code,by="ICD",all.x = TRUE)
comp_merge$phecode[is.na(comp_merge$phecode)] -> "Other"
make_dummyvf <- comp_merge[!is.na(comp_merge$phecode),]
make_dummyvf_keep <- make_dummyvf[(make_dummyvf$phecode %in% c(80,1011,501,590,591,1013) | 
                                     substring(make_dummyvf$phecode,1,3) %in% c(599,592,540,994,480)),]
need_merge <- make_dummyvf_keep

check_diff <- setdiff(unique(Epic_data$arb_person_id),unique(Diag$arb_person_id))

#### indicator for diag ####
inter_Epic <- intersect(unique(Epic_data$arb_person_id),unique(need_merge$arb_person_id))

nsqip_diag_merge <-  merge(need_merge,Epic_data,by = c("arb_person_id", "arb_encounter_id"), all = TRUE)


### Validation ###

nsqip_diag_merge$DiagnosisDate[nsqip_diag_merge$DiagnosisDate==""] <- NA
nsqip_diag_merge$as_date_diag <- as.Date(nsqip_diag_merge$DiagnosisDate)
nsqip_diag_merge$SurgeryDate_asDate <- as.Date(nsqip_diag_merge$SurgeryDate_asDate)

nsqip_diag_merge$length_diag_operation <- nsqip_diag_merge$SurgeryDate_asDate-nsqip_diag_merge$as_date_diag

### validation session ends ###

nsqip_diag_merge$ind_rec_yesno[is.na(nsqip_diag_merge$length_diag_operation)] <- 0
nsqip_diag_merge$ind_rec_yesno[!is.na(nsqip_diag_merge$length_diag_operation)] <- 1

nsqip_diag_merge$ind_rec_30days[nsqip_diag_merge$length_diag_operation <= 0 & nsqip_diag_merge$length_diag_operation >= -30] <- 1
nsqip_diag_merge$ind_rec_30days[nsqip_diag_merge$length_diag_operation < -30 | nsqip_diag_merge$length_diag_operation > 0 ] <- 0

nsqip_diag_merge$ind_operative_rec[nsqip_diag_merge$length_diag_operation < -30 ] <- 0
nsqip_diag_merge$ind_operative_rec[nsqip_diag_merge$length_diag_operation <= 0 & nsqip_diag_merge$length_diag_operation >= -30 ] <- 1
nsqip_diag_merge$ind_operative_rec[nsqip_diag_merge$length_diag_operation > 0 ] <- 2
nsqip_diag_merge$ind_operative_rec[is.na(nsqip_diag_merge$length_diag_operation)] <- NA

# table(nsqip_diag_merge$DiagnosisCodeType)

diag_cohort_icd10 <- nsqip_diag_merge[!is.na(nsqip_diag_merge$DiagnosisCodeType),]

ICD10_rec_excl <- diag_cohort_icd10
make_dummy <- diag_cohort_icd10[diag_cohort_icd10$ind_operative_rec==1 & !is.na(diag_cohort_icd10$ind_operative_rec),]

make_dummyvf_keep <- make_dummy

dummydf1 <- dummy_cols(make_dummyvf_keep, select_columns = "phecode")
dficd1 <- dummydf1

# colnames(dficd1)[283:285]
# ncol(dficd1)

n <- which(colnames(dficd1) == "phecode_80")

for (i in n:ncol(dficd1)) {
  colnames(dficd1)[i] <- substring(colnames(dficd1)[i], 9)
}

for (i in n:ncol(dficd1)) {
  dficd1[,i] <- ifelse(dficd1[,i] == 0, NA, 1)
}


#start1 <- Sys.time()
dficd2 <- 
  dficd1 %>% 
  group_by(SurgeryID) %>%
  fill_(names(dficd1)[n:ncol(dficd1)], .direction = "up") 
#end1 <- Sys.time()
#spendtime1 <- end1-start1

dficd2 <- as.data.frame(dficd2)

first_row_dficd3 <- dficd2[!duplicated(dficd2$SurgeryID),]

transpose_icd10 <- first_row_dficd3

# colnames(transpose_icd10)[283 : 285]

for (i in n:ncol(transpose_icd10)) {
  transpose_icd10[,i][is.na(transpose_icd10[,i])] <- 0 
}

store_patient_names_96 <- setdiff(unique(Epic_data$SurgeryID),unique(make_dummyvf_keep$SurgeryID))

comp_patients <- Epic_data[Epic_data$SurgeryID %in% store_patient_names_96,]

unique_comp_patients <- comp_patients[!duplicated(comp_patients$SurgeryID),]

final_data <- rbind.fill(transpose_icd10,unique_comp_patients)
final_data <- as.data.frame(final_data)

# colnames(final_data)[283 : 285]

#View(final_data[16000:16362,104:110])
for (i in n:ncol(final_data)) {
  final_data[,i][is.na(final_data[,i])] <- 0 
}

final_data_store <- final_data[,1:(ncol(final_data)-1)]

#write.csv(final_data_store,file="/Users/yaxuzhuang/OneDrive - The University of Colorado Denver/Katie Project/2022_0711/diag_postop/final_data_store_diag_postop.csv")
final_data_store_diag_postop <- final_data_store

saveRDS(final_data_store_diag_postop, paste0(fp_processed,
                                       "final_data_store_diag_postop.rds"))

# glimpse(final_data_store_diag_postop)
#########################################################################
# TODO: ## com score
## TODO: this is to get Pre-op: 2022_0711/combid_pre/com_score_preop.rdata
## 2022_0711/gen_data_preop/comscore_preop_1003_useSurgeryID.R

library(comorbidity)


Epic_data1 <- Epic_data[,c("arb_person_id","SurgeryID","SurgeryDate_asDate")]

diag_cohort <- Diag[Diag$arb_person_id %in% Epic_data1$arb_person_id,]

need_merge <- diag_cohort[,c("arb_person_id","DiagnosisDate","DiagnosisCodeType","DiagnosisCode")]
#### indicator for diag ####
nsqip_diag_merge <-  merge(need_merge,Epic_data1,by = "arb_person_id", all = TRUE)

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

make_dummyvf_keep <- make_dummyvf[(make_dummyvf$phecode %in% c(80,591,567,994,150,480,501) | 
                                     substring(make_dummyvf$phecode,1,3) %in% c(560)),]

need_merge <- make_dummyvf_keep

#write.csv(unique(Diag$arb_person_id),file="/Users/yaxuzhuang/OneDrive - The University of Colorado Denver/Katie Project/2022_0711/diag_postop/Diag_all_population.csv")
check_diff <- setdiff(unique(Epic_data$arb_person_id),unique(Diag$arb_person_id))

# check_diff
# length(check_diff)

#### indicator for diag ####
inter_Epic <- intersect(unique(Epic_data$arb_person_id),unique(need_merge$arb_person_id))

nsqip_diag_merge <-  merge(need_merge,Epic_data,by = "arb_person_id", all = TRUE)

nsqip_diag_merge$DiagnosisDate[nsqip_diag_merge$DiagnosisDate==""] <- NA
nsqip_diag_merge$as_date_diag <- as.Date(nsqip_diag_merge$DiagnosisDate)
nsqip_diag_merge$SurgeryDate_asDate <- as.Date(nsqip_diag_merge$SurgeryDate_asDate)
#table(nsqip_diag_merge$DiagnosisDate)

nsqip_diag_merge$length_diag_operation <- nsqip_diag_merge$SurgeryDate_asDate-nsqip_diag_merge$as_date_diag


nsqip_diag_merge$ind_rec_yesno[is.na(nsqip_diag_merge$length_diag_operation)] <- 0
nsqip_diag_merge$ind_rec_yesno[!is.na(nsqip_diag_merge$length_diag_operation)] <- 1
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
#make_dummy <- diag_cohort_icd10[diag_cohort_icd10$ind_operative_rec==1 & !is.na(diag_cohort_icd10$ind_operative_rec),]
make_dummy <- diag_cohort_icd10[diag_cohort_icd10$pre_op_diag==1 & !is.na(diag_cohort_icd10$pre_op_diag),]

make_dummyvf_keep <- make_dummy

#save(make_dummyvf_keep,file = "/Users/yaxuzhuang/OneDrive - The University of Colorado Denver/Katie Project/2022_0711/diag_postop/make_dummyvf_keep.Rdata")
#load("/Users/yaxuzhuang/Library/CloudStorage/OneDrive-TheUniversityofColoradoDenver/Katie Project/2022_0711/diag_postop/make_dummyvf_keep.Rdata")


dummydf1 <- dummy_cols(make_dummyvf_keep, select_columns = "phecode")
dficd1 <- dummydf1

# colnames(dficd1)[283: 288]

n <- which(colnames(dficd1) == "phecode_80")

for (i in n:ncol(dficd1)) {
  colnames(dficd1)[i] <- substring(colnames(dficd1)[i], 9)
}

for (i in n:ncol(dficd1)) {
  dficd1[,i] <- ifelse(dficd1[,i] == 0, NA, 1)
}


dficd2 <- 
  dficd1 %>% 
  group_by(SurgeryID) %>%
  fill_(names(dficd1)[n:ncol(dficd1)], .direction = "up") 

dficd2 <- as.data.frame(dficd2)

#dficd2$SurgeryID
#View(dficd2)


first_row_dficd3 <- dficd2[!duplicated(dficd2$SurgeryID),]

#length(unique(post_operative_30days$SurgeryID))


transpose_icd10 <- first_row_dficd3

for (i in n:ncol(transpose_icd10)) {
  transpose_icd10[,i][is.na(transpose_icd10[,i])] <- 0 
}


############################# step8: add back the patients who do not have ICD10 records within 30 days #####

store_patient_names_96 <- setdiff(unique(Epic_data$SurgeryID),unique(make_dummyvf_keep$SurgeryID))


comp_patients <- Epic_data[Epic_data$SurgeryID %in% store_patient_names_96,]

unique_comp_patients <- comp_patients[!duplicated(comp_patients$SurgeryID),]

#colnames(unique_comp_patients)[4] <- colnames(transpose_icd10)[4]
#colnames(unique_comp_patients)[4]

final_data <- rbind.fill(transpose_icd10,unique_comp_patients)
final_data <- as.data.frame(final_data)


#View(final_data[16000:16362,104:110])
for (i in n:ncol(final_data)) {
  final_data[,i][is.na(final_data[,i])] <- 0 
}


final_data_store <- final_data[,1:(ncol(final_data)-1)]

#write.csv(final_data_store,file="/Users/yaxuzhuang/OneDrive - The University of Colorado Denver/Katie Project/2022_0711/diag_postop/final_data_store_diag_preop.csv")

final_data_store_diag_preop <- final_data_store

saveRDS(final_data_store_diag_preop, paste0(fp_processed,
                                             "final_data_store_diag_preop.rds"))

# glimpse(final_data_store_diag_preop)

rm(final_data_store, Diag, final_data, make_dummy, make_dummyvf, 
   make_dummyvf_keep, phe_code, unique_comp_patients, transpose_icd10,
   need_merge, nsqip_diag_merge, first_row_dficd3, diag_cohort,
   diag_cohort_icd10, dficd1, dficd2, comp_merge, comp_patients,
   dummydf1)

#########################################################################
# TODO: this is to get final_data_store_lab_postop

lab_positive <- Lab
lab_positive$LabResult[lab_positive$LabResult==""] <- "unknown"
lab_positive$LabResult[is.na(lab_positive$LabResult)] <- "unknown"

lab_positive$ind_positive[substring(lab_positive$LabResult,1,3)=="No " | substring(lab_positive$LabResult,1,3)=="NO " | grepl("Negative", lab_positive$LabResult, ignore.case = T)==TRUE] <- 0
lab_positive$ind_positive[!(substring(lab_positive$LabResult,1,3)=="No " | substring(lab_positive$LabResult,1,3)=="NO " | grepl("Negative", lab_positive$LabResult, ignore.case = T)==TRUE)] <- 1
lab_positive2 <- lab_positive[lab_positive$ind_positive==1,]


lab <- lab_positive2[,-ncol(lab_positive2)]

# TODO: GET THE DATA file cultureAllInfections.csv
cultures <- read.csv(file = "../data/cultureAllInfections.csv")
# View(cultures)
store_cult_names <- as.factor(cultures$LabPanelName)
# TODO: may change from lab <- lab_positive2[,-ncol(lab_positive2)] to lab <- read.csv
lab_cohort <- Lab[Lab$arb_person_id %in% Epic_data$arb_person_id,]

# TODO: include more urine culture here 
lab_cdiff <- Lab %>% dplyr::select(LabPanelName) %>%
  filter(str_detect(LabPanelName, regex("C[^/]DIFF", ignore_case = TRUE)) |
           str_detect(LabPanelName, regex("CDIFF", ignore_case = TRUE)) )

CDIFFs <- table(lab_cdiff) %>% names()

lab_cohort <- lab_cohort[(lab_cohort$LabPanelName %in% c("AEROBIC CULTURE (EG: TISSUE, ABSCESS, WOUND, SINUS, ETC","URINE CULTURE - RWHS","AFB BLOOD CULTURE - SOUTH","SCREEN, YEAST SCREENING CULTURE",
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
                                  "LAB USE ONLY - URINE CULTURE, ROUTINE", "LAB USE ONLY - URINE CULTURE RT 997870", 
                                  "LAB USE ONLY - URINE CULTURE, PRENATAL, W/GBS RESULT")) 
                         | (lab_cohort$LabPanelName %in% as.factor(cultures$LabPanelName)) |
                           (lab_cohort$LabPanelName %in% CDIFFs) ,]



# glimpse(lab_cohort)

a <- unique(lab_cohort$LabPanelName)

nsqip_lab_merge <-  merge(lab_cohort, Epic_data, by = c("arb_person_id", "arb_encounter_id"), all = TRUE)

nsqip_lab_merge$LabCollectedDate[nsqip_lab_merge$LabCollectedDate==""] <- NA
nsqip_lab_merge$as_date_lab <- as.Date(nsqip_lab_merge$LabCollectedDate)

nsqip_lab_merge$SurgeryDate_asDate <- as.Date(nsqip_lab_merge$SurgeryDate_asDate)
nsqip_lab_merge$length_lab_operation <- nsqip_lab_merge$SurgeryDate_asDate-nsqip_lab_merge$as_date_lab

nsqip_lab_merge$ind_labrec_yesno[is.na(nsqip_lab_merge$length_lab_operation)] <- 0
nsqip_lab_merge$ind_labrec_yesno[!is.na(nsqip_lab_merge$length_lab_operation)] <- 1

nsqip_lab_merge$ind_labrec_30days[nsqip_lab_merge$length_lab_operation <= 0 & nsqip_lab_merge$length_lab_operation >= -30] <- 1
nsqip_lab_merge$ind_labrec_30days[nsqip_lab_merge$length_lab_operation < -30 | nsqip_lab_merge$length_lab_operation > 0 ] <- 0

nsqip_lab_merge$ind_operative_rec[nsqip_lab_merge$length_lab_operation < -30 ] <- 0
nsqip_lab_merge$ind_operative_rec[nsqip_lab_merge$length_lab_operation <= 0 & nsqip_lab_merge$length_lab_operation >= -30 ] <- 1
nsqip_lab_merge$ind_operative_rec[nsqip_lab_merge$length_lab_operation > 0 ] <- 2
nsqip_lab_merge$ind_operative_rec[is.na(nsqip_lab_merge$length_lab_operation)] <- NA


df3 <- nsqip_lab_merge

include_pat <- df3

make_dummy <- include_pat[include_pat$ind_operative_rec==1 & !is.na(include_pat$ind_operative_rec),]


lab_panel2 <- make_dummy[make_dummy$LabPanelName=="ZZMORPHOLOGY",]


#AEROBIC CULTURE (EG: TISSUE, ABSCESS, WOUND, SINUS, ETC)
#AEROBIC CULTURE - MHS ONLY
#ANAEROBIC CULTURE, (EG:TISSUE, ABSCESS, WOUND, SINUS, ETC.)
#ANAEROBIC CULTURE - MHS ONLY
#BLOOD CULTURE
#CULTURE AEROBIC, ROUT, COMPOSITE
#BLOOD CULTURE SET 2
#CULTURE ANAEROBIC
#BLOOD CULTURE POSITIVE WORKUP - RWHS
#BLOOD CULTURE POS WORKUP HIGH#
#CULTURE BLOOD, LAB COLL 1

#UA DIPSTICK W/ REFLEX TO MICROSCOPIC EXAM IF IND (NO CULTURE REFLEX, EXCEPT EPH)
#UA COMPLETE URINALYSIS (NO CULTURE REFLEX)
#UA COMPLETE URINALYSIS W/ CULTURE REFLEX IF INDICATED
#URINE CULTURE
#URINE CULTURE - MHS ONLY
#UA MICROSCOPIC ONLY (NO CULTURE REFLEX,EXCEPT EPH)

#CDIFFICILE TOXIN PCR
#C DIFFICILE BY PCR

make_dummy_keep_lbNames <- make_dummy


make_dummy_keep_lbNames <- as.data.frame(make_dummy_keep_lbNames)


dummydf1 <- dummy_cols(make_dummy_keep_lbNames, select_columns = "LabPanelName")


dficd1 <- dummydf1

# colnames(dficd1)[284:289]
# ncol(dficd1)

# colnames(dficd1)[316:ncol(dficd1)]

# n <- which(colnames(dficd1) == "LabPanelName_ACANTHAMOEBA CULTURE")
# TODO: fix this to the following
n <- grep("^LabPanelName_", colnames(dficd1))[1]

for (i in n:ncol(dficd1)) {
  colnames(dficd1)[i] <- substring(colnames(dficd1)[i], 14)
}

for (i in n:ncol(dficd1)) {
  dficd1[,i] <- ifelse(dficd1[,i] == 0, NA, 1)
}

dficd2 <- 
  dficd1 %>% 
  group_by(SurgeryID) %>%
  fill_(names(dficd1)[n:ncol(dficd1)], .direction = "up") 

dficd2 <- as.data.frame(dficd2)


first_row_dficd3 <- dficd2[!duplicated(dficd2$SurgeryID),]

#### 16261 #####

#length(unique(post_operative_30days$SurgeryID))

transpose_icd10 <- first_row_dficd3

for (i in n:ncol(transpose_icd10)) {
  transpose_icd10[,i][is.na(transpose_icd10[,i])] <- 0 
  
}


############################# step8: add back the patients who do not have ICD10 records within 30 days #####
comb_surg_id <- c(transpose_icd10$SurgeryID)
store_patient_names_96 <- setdiff(unique(Epic_data$SurgeryID),comb_surg_id)

comp_patients <- Epic_data[Epic_data$SurgeryID %in% store_patient_names_96,]

unique_comp_patients <- comp_patients[!duplicated(comp_patients$SurgeryID),]

final_data <- rbind.fill(transpose_icd10,unique_comp_patients)
final_data <- as.data.frame(final_data)

#View(final_data[16000:16362,104:110])
for (i in n:ncol(final_data)) {
  final_data[,i][is.na(final_data[,i])] <- 0 
}

#final_data_store <- final_data
final_data_store_lab_postop <- final_data

saveRDS(final_data_store_lab_postop, paste0(fp_processed,
                                       "final_data_store_lab_postop.rds"))

#########################################################################
# TODO: final_data_store_preop_lab

# TODO: include more Blood culture

lab_cohort <- lab_cohort[(lab_cohort$LabPanelName %in% c("AEROBIC CULTURE (EG: TISSUE, ABSCESS, WOUND, SINUS, ETC",
                                "AEROBIC CULTURE - MHS ONLY", "AFB BLOOD CULTURE - SOUTH", "ANAEROBIC CULTURE, (EG:TISSUE, ABSCESS, WOUND, SINUS, ETC.)","ANAEROBIC CULTURE",
                    " ANAEROBIC CULTURE - MHS ONLY","BLOOD CULTURE","CULTURE AEROBIC, ROUT, COMPOSITE","BLOOD CULTURE SET 2","BLOOD CULTURE POS WORKUP PEDS",
                          "CULTURE ANAEROBIC","BLOOD CULTURE POSITIVE WORKUP - RWHS","BLOOD CULTURE POS WORKUP HIGH#","CULTURE BLOOD, LAB COLL 1","ANAEROBIC CULTURE - RWHS",
                    "BLOOD CULTURE, FUNGAL (MOLD ONLY)", "BLOOD CULTURE, ISOLATOR BACTERIA", "BLOOD CULTURE, ISOLATOR FUNGUS",
                    "FUNGUS CULTURE - BLOOD", "LAB USE ONLY - MRSA/SA BLOOD CULTURE PCR")),]


a <- unique(lab_cohort$LabPanelName)

#### indicator for diag ####
nsqip_lab_merge <- merge(lab_cohort,Epic_data,by = "arb_person_id", all = TRUE)


#save(nsqip_lab_merge,file = "/Users/yaxuzhuang/OneDrive - The University of Colorado Denver/Katie Project/2022_0711/preop_lab/nsqip_lab_merge.Rdata")

nsqip_lab_merge$LabCollectedDate[nsqip_lab_merge$LabCollectedDate==""] <- NA
nsqip_lab_merge$as_date_lab <- as.Date(nsqip_lab_merge$LabCollectedDate)

nsqip_lab_merge$SurgeryDate_asDate <- as.Date(nsqip_lab_merge$SurgeryDate_asDate)
nsqip_lab_merge$length_lab_operation <- nsqip_lab_merge$SurgeryDate_asDate-nsqip_lab_merge$as_date_lab

nsqip_lab_merge$ind_labrec_yesno[is.na(nsqip_lab_merge$length_lab_operation)] <- 0
nsqip_lab_merge$ind_labrec_yesno[!is.na(nsqip_lab_merge$length_lab_operation)] <- 1


nsqip_lab_merge$ind_labrec_30days[nsqip_lab_merge$length_lab_operation <= 0 & nsqip_lab_merge$length_lab_operation >= -30] <- 1
nsqip_lab_merge$ind_labrec_30days[nsqip_lab_merge$length_lab_operation < -30 | nsqip_lab_merge$length_lab_operation > 0 ] <- 0

nsqip_lab_merge$ind_operative_rec[nsqip_lab_merge$length_lab_operation < -30 ] <- 0
nsqip_lab_merge$ind_operative_rec[nsqip_lab_merge$length_lab_operation <= 0 & nsqip_lab_merge$length_lab_operation >= -30 ] <- 1
nsqip_lab_merge$ind_operative_rec[nsqip_lab_merge$length_lab_operation > 0 ] <- 2
nsqip_lab_merge$ind_operative_rec[is.na(nsqip_lab_merge$length_lab_operation)] <- NA

nsqip_lab_merge$pre_op_lab <- ifelse(nsqip_lab_merge$length_lab_operation > 0 & nsqip_lab_merge$length_lab_operation <= 365,1,0)


df3 <- nsqip_lab_merge

include_pat <- df3

#make_dummy <- include_pat[include_pat$ind_operative_rec==1 & !is.na(include_pat$ind_operative_rec),]
make_dummy <- include_pat[include_pat$pre_op_lab==1 & !is.na(include_pat$pre_op_lab),]

lab_panel2 <- make_dummy[make_dummy$LabPanelName=="ZZMORPHOLOGY",]


#AEROBIC CULTURE (EG: TISSUE, ABSCESS, WOUND, SINUS, ETC)
#AEROBIC CULTURE - MHS ONLY
#ANAEROBIC CULTURE, (EG:TISSUE, ABSCESS, WOUND, SINUS, ETC.)
#ANAEROBIC CULTURE - MHS ONLY
#BLOOD CULTURE
#CULTURE AEROBIC, ROUT, COMPOSITE
#BLOOD CULTURE SET 2
#CULTURE ANAEROBIC
#BLOOD CULTURE POSITIVE WORKUP - RWHS
#BLOOD CULTURE POS WORKUP HIGH#
#CULTURE BLOOD, LAB COLL 1

#UA DIPSTICK W/ REFLEX TO MICROSCOPIC EXAM IF IND (NO CULTURE REFLEX, EXCEPT EPH)
#UA COMPLETE URINALYSIS (NO CULTURE REFLEX)
#UA COMPLETE URINALYSIS W/ CULTURE REFLEX IF INDICATED
#URINE CULTURE
#URINE CULTURE - MHS ONLY
#UA MICROSCOPIC ONLY (NO CULTURE REFLEX,EXCEPT EPH)

#CDIFFICILE TOXIN PCR
#C DIFFICILE BY PCR

make_dummy_keep_lbNames <- make_dummy

make_dummy_keep_lbNames <- as.data.frame(make_dummy_keep_lbNames)

#dummydf1 <- dummy.data.frame(make_dummy_keep_lbNames, names = c("LabPanelName") , sep = "")
dummydf1 <- dummy_cols(make_dummy_keep_lbNames, select_columns = "LabPanelName")


dficd1 <- dummydf1


# colnames(dficd1)[285 : 292]
n <- grep("^LabPanelName_", colnames(dficd1))[1]

for (i in n:ncol(dficd1)) {
  colnames(dficd1)[i] <- substring(colnames(dficd1)[i], 14)
}

for (i in n:ncol(dficd1)) {
  dficd1[,i] <- ifelse(dficd1[,i] == 0, NA, 1)
}

dficd2 <- 
  dficd1 %>% 
  group_by(SurgeryID) %>%
  fill_(names(dficd1)[n:ncol(dficd1)], .direction = "up") 

dficd2 <- as.data.frame(dficd2)



first_row_dficd3 <- dficd2[!duplicated(dficd2$SurgeryID),]

#### 16261 #####

#length(unique(post_operative_30days$SurgeryID))


transpose_icd10 <- first_row_dficd3

for (i in n:ncol(transpose_icd10)) {
  transpose_icd10[,i][is.na(transpose_icd10[,i])] <- 0 
}

############################# step8: add back the patients who do not have ICD10 records within 30 days #####
store_patient_names_96 <- setdiff(unique(Epic_data$SurgeryID),unique(make_dummy_keep_lbNames$SurgeryID))


comp_patients <- Epic_data[Epic_data$SurgeryID %in% store_patient_names_96,]


unique_comp_patients <- comp_patients[!duplicated(comp_patients$SurgeryID),]

#colnames(unique_comp_patients)[4] <- colnames(transpose_icd10)[4]
#colnames(unique_comp_patients)[4]

final_data <- rbind.fill(transpose_icd10,unique_comp_patients)
final_data <- as.data.frame(final_data)

#View(final_data[16000:16362,104:110])
for (i in n:ncol(final_data)) {
  final_data[,i][is.na(final_data[,i])] <- 0 
  
}


final_data_store_preop_lab <- final_data_store <- final_data

saveRDS(final_data_store_preop_lab, paste0(fp_processed,
                                            "final_data_store_preop_lab.rds"))

rm(Lab_p1, Lab_p2, final_data, comp_patients, unique_comp_patients, transpose_icd10,
   first_row_dficd3, dficd1, dficd2,
   make_dummy_keep_lbNames, dummydf1, make_dummy, df3, nsqip_lab_merge,
   include_pat, lab_panel2)

#################################################################################
## TODO: final_data_store_medi_postop

# Medi <- read.csv(file = "data/C2730_Table7_Medications_20240223.csv")

#write.csv(unique(Medi$arb_person_id),file="/Users/yaxuzhuang/OneDrive - The University of Colorado Denver/Katie Project/2022_0711/medi_postop/Medi_all_population.csv")
#write.csv(unique(Epic_data$arb_person_id),file="/Users/yaxuzhuang/OneDrive - The University of Colorado Denver/Katie Project/2022_0711/medi_postop/Surgery_all_population.csv")

check_diff <- setdiff(unique(Epic_data$arb_person_id),unique(Medi$arb_person_id))


medi_cohort <- Medi[Medi$arb_person_id %in% Epic_data$arb_person_id,]

medi_cohort <- medi_cohort[medi_cohort$TherapeuticClass=="ANTIBIOTICS",]

nsqip_medi_merge <- merge(medi_cohort,Epic_data,by = "arb_person_id", all = TRUE)

### Validation ###


dim_epic <- Epic_data[Epic_data$arb_person_id==1590586, ]
dim_medi <- medi_cohort[medi_cohort$arb_person_id==1590586, ]
dim_nsqip_medi_merge <- nsqip_medi_merge[nsqip_medi_merge$arb_person_id==1590586, ]


nsqip_medi_merge$SurgeryDate_asDate <- as.Date(nsqip_medi_merge$SurgeryDate_asDate)
nsqip_medi_merge$as_date_med <- as.Date(nsqip_medi_merge$OrderedDate)

nsqip_medi_merge$length_med_operation <- nsqip_medi_merge$SurgeryDate_asDate-nsqip_medi_merge$as_date_med
#View(nsqip_medi_merge[,c("as_date_operation","as_date_med","length_med_operation")])

nsqip_medi_merge$ind_medrec_yesno[is.na(nsqip_medi_merge$length_med_operation)] <- 0
nsqip_medi_merge$ind_medrec_yesno[!is.na(nsqip_medi_merge$length_med_operation)] <- 1

nsqip_medi_merge$ind_medrec_30days[nsqip_medi_merge$length_med_operation <= 0 & nsqip_medi_merge$length_med_operation >= -30] <- 1
nsqip_medi_merge$ind_medrec_30days[nsqip_medi_merge$length_med_operation < -30 | nsqip_medi_merge$length_med_operation > 0 ] <- 0

nsqip_medi_merge$ind_operative_rec[nsqip_medi_merge$length_med_operation < -30 ] <- 0
nsqip_medi_merge$ind_operative_rec[nsqip_medi_merge$length_med_operation <= -2 & nsqip_medi_merge$length_med_operation >= -30 ] <- 1
nsqip_medi_merge$ind_operative_rec[nsqip_medi_merge$length_med_operation > -2 ] <- 2
nsqip_medi_merge$ind_operative_rec[is.na(nsqip_medi_merge$length_med_operation)] <- NA

df3 <- nsqip_medi_merge

include_pat <- df3

make_dummy <- include_pat[include_pat$ind_operative_rec==1 & !is.na(include_pat$ind_operative_rec),]
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
## TODO: final_data_store_medi_preop

medi_cohort <- Medi[Medi$arb_person_id %in% Epic_data$arb_person_id,]
medi_cohort <- medi_cohort[(medi_cohort$PharmaceuticalClass=="URINARY TRACT ANTISPASMODIC/ANTIINCONTINENCE AGENT"),]

nsqip_medi_merge <- merge(medi_cohort,Epic_data,by = "arb_person_id", all = TRUE)

nsqip_medi_merge$as_date_med <- as.Date(nsqip_medi_merge$OrderedDate)
nsqip_medi_merge$SurgeryDate_asDate <- as.Date(nsqip_medi_merge$SurgeryDate_asDate)

nsqip_medi_merge$length_med_operation <- nsqip_medi_merge$SurgeryDate_asDate-nsqip_medi_merge$as_date_med
#View(nsqip_medi_merge[,c("as_date_operation","as_date_med","length_med_operation")])

nsqip_medi_merge$ind_medrec_yesno[is.na(nsqip_medi_merge$length_med_operation)] <- 0
nsqip_medi_merge$ind_medrec_yesno[!is.na(nsqip_medi_merge$length_med_operation)] <- 1

nsqip_medi_merge$ind_medrec_30days[nsqip_medi_merge$length_med_operation <= 0 & nsqip_medi_merge$length_med_operation >= -30] <- 1
nsqip_medi_merge$ind_medrec_30days[nsqip_medi_merge$length_med_operation < -30 | nsqip_medi_merge$length_med_operation > 0 ] <- 0

nsqip_medi_merge$ind_operative_rec[nsqip_medi_merge$length_med_operation < -30 ] <- 0
nsqip_medi_merge$ind_operative_rec[nsqip_medi_merge$length_med_operation <= 0 & nsqip_medi_merge$length_med_operation >= -30 ] <- 1
nsqip_medi_merge$ind_operative_rec[nsqip_medi_merge$length_med_operation > 0 ] <- 2
nsqip_medi_merge$ind_operative_rec[is.na(nsqip_medi_merge$length_med_operation)] <- NA

nsqip_medi_merge$pre_op_med <- ifelse(nsqip_medi_merge$length_med_operation > 0 & nsqip_medi_merge$length_med_operation <= 365,1,0)

df3 <- nsqip_medi_merge
include_pat <- df3

make_dummy <- include_pat[include_pat$pre_op_med==1 & !is.na(include_pat$pre_op_med),]
#make_dummy <- include_pat[include_pat$ind_operative_rec==1 & !is.na(include_pat$ind_operative_rec),]

make_dummy$`URINARY TRACT ANTISPASMODIC/ANTIINCONTINENCE AGENT` <- 1
transpose_icd10 <- make_dummy[!duplicated(make_dummy$arb_person_id),]

store_patient_names_96 <- setdiff(unique(Epic_data$arb_person_id),unique(transpose_icd10$arb_person_id))

comp_patients <- as.data.frame(cbind(store_patient_names_96,0))
colnames(comp_patients) <- c("arb_person_id","URINARY TRACT ANTISPASMODIC/ANTIINCONTINENCE AGENT")

final_data <- rbind.fill(transpose_icd10,comp_patients)
final_data$`URINARY TRACT ANTISPASMODIC/ANTIINCONTINENCE AGENT`[is.na(final_data$`URINARY TRACT ANTISPASMODIC/ANTIINCONTINENCE AGENT`)] <- 0 

final_data_store_medi_preop <- final_data[,c("arb_person_id","URINARY TRACT ANTISPASMODIC/ANTIINCONTINENCE AGENT")]

#write.csv(final_data_store,file="/Users/yaxuzhuang/OneDrive - The University of Colorado Denver/Katie Project/2022_0711/medi_postop/final_data_store_medi_preop.csv")

saveRDS(final_data_store_medi_preop, paste0(fp_processed,
                                             "final_data_store_medi_preop.rds"))

rm(Medi, final_data)

####################################################################################
## TODO: final_data_store_proc_preop
# Proc <- read.csv(file = "data/C2730_Table5_Procedures_20240223.csv")



proc_cohort <- Proc[Proc$arb_person_id %in% Epic_data$arb_person_id,]
proc_cohort <- proc_cohort[(proc_cohort$ProcedureName=="OS CT BODY COMPARE-NO READ"),] # problem
# glimpse(proc_cohort)

nsqip_proc_merge <- merge(proc_cohort,Epic_data,by = "arb_person_id", all = TRUE)

# to order date, comp old dataset, use table 10 for UCH patients, find cpt codes for the vars
nsqip_proc_merge$ProcedureEventDate[nsqip_proc_merge$ProcedureEventDate==""] <- NA
# nsqip_proc_merge$as_date_proc <- as.Date(nsqip_proc_merge$ProcedureEventDate)
nsqip_proc_merge$as_date_proc <- as.Date(nsqip_proc_merge$ProcedureOrderDate)
nsqip_proc_merge$SurgeryDate_asDate <- as.Date(nsqip_proc_merge$SurgeryDate_asDate)


nsqip_proc_merge$length_proc_operation <- nsqip_proc_merge$SurgeryDate_asDate-nsqip_proc_merge$as_date_proc
#View(nsqip_proc_merge[,c("ProcedureDate","as_date_proc","as_date_operation","length_proc_operation")])

nsqip_proc_merge$ind_procrec_yesno[is.na(nsqip_proc_merge$length_proc_operation)] <- 0
nsqip_proc_merge$ind_procrec_yesno[!is.na(nsqip_proc_merge$length_proc_operation)] <- 1

nsqip_proc_merge$ind_procrec_30days[nsqip_proc_merge$length_proc_operation <= 0 & nsqip_proc_merge$length_proc_operation >= -30] <- 1
nsqip_proc_merge$ind_procrec_30days[nsqip_proc_merge$length_proc_operation < -30 | nsqip_proc_merge$length_proc_operation > 0 ] <- 0

nsqip_proc_merge$ind_operative_rec[nsqip_proc_merge$length_proc_operation < -30 ] <- 0
nsqip_proc_merge$ind_operative_rec[nsqip_proc_merge$length_proc_operation <= 0 & nsqip_proc_merge$length_proc_operation >= -30 ] <- 1
nsqip_proc_merge$ind_operative_rec[nsqip_proc_merge$length_proc_operation > 0 ] <- 2
nsqip_proc_merge$ind_operative_rec[is.na(nsqip_proc_merge$length_proc_operation)] <- NA

nsqip_proc_merge$pre_op_proc <- ifelse(nsqip_proc_merge$length_proc_operation > 0 & nsqip_proc_merge$length_proc_operation <= 365,1,0)

df3 <- nsqip_proc_merge
include_pat <- df3
make_dummy <- include_pat[include_pat$pre_op_proc==1 & !is.na(include_pat$pre_op_proc),]

make_dummy$`OS CT BODY COMPARE-NO READ` <- 1



transpose_icd10 <- make_dummy[!duplicated(make_dummy$SurgeryID),]

############################# step8: add back the patients who do not have ICD10 records within 30 days #####

store_patient_names_96 <- setdiff(unique(Epic_data$SurgeryID),unique(transpose_icd10$SurgeryID))

######

comp_patients <- Epic_data[Epic_data$SurgeryID %in% store_patient_names_96,]


unique_comp_patients <- comp_patients[!duplicated(comp_patients$SurgeryID),]


final_data <- rbind.fill(transpose_icd10,unique_comp_patients)
final_data <- as.data.frame(final_data)


######
final_data$`OS CT BODY COMPARE-NO READ`[is.na(final_data$`OS CT BODY COMPARE-NO READ`)] <- 0 

final_data_store_proc_preop_0922 <- final_data

saveRDS(final_data_store_proc_preop_0922, paste0(fp_processed,
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

patient_cohort <- patient[patient$arb_person_id %in% Epic_data$arb_person_id,c("arb_person_id","Sex"),]
patient_cohort_uni <- patient_cohort[!duplicated(patient_cohort$arb_person_id),]

df_merge_gender <- merge(Epic_data[,c("arb_person_id","SurgeryID")],patient_cohort_uni,by="arb_person_id",all.x = TRUE)

df_storee <- dummy_cols(df_merge_gender, select_columns ="Sex")

#dummydf1 <- dummy_cols(make_dummy_keep_lbNames, select_columns = "LabPanelName")

pre_op_gender <- df_storee

saveRDS(pre_op_gender, paste0(fp_processed,
                                          "pre_op_gender.rds"))
#write.csv(df_storee,file="/Users/yaxuzhuang/OneDrive - The University of Colorado Denver/Katie Project/2022_0711/preop_lab/pre_op_gender.csv")
rm(patient, patient_cohort, patient_cohort_uni)

##############################################################################
## TODO: prepare analytical_preopv2
## TODO: preop_expected_rate_20221005
# diag_pred <- final_data_store_diag_preop
# 
# lab_pred <- final_data_store_preop_lab
# 
# #medi_pred <- read.csv(file="/Users/yaxuzhuang/OneDrive - The University of Colorado Denver/Katie Project/2022_0711/medi_postop/final_data_store_medi_preop.csv",check.names=FALSE)
# proc_pred_preop <- final_data_store_proc_preop_0922
# # table(proc_pred_preop$`OS CT BODY COMPARE-NO READ`)
# 
# # colnames(proc_pred_preop)
# 
# # clean_surg_preop_pred1006
# 
# surg_pred_preop <- clean_surg_preop_pred1006
# 
# # table(surg_pred_preop$`OS CT BODY COMPARE-NO READ`)
# 
# gender_preop <- pre_op_gender
# 
# 
# # diag_pred <- diag_pred[,-1]
# # lab_pred <- lab_pred[,-1]
# # #medi_pred <- medi_pred[,-1]
# # proc_pred_preop <- proc_pred_preop[,-1]
# # surg_pred_preop <- surg_pred_preop[,-1]
# # gender_preop <- gender_preop[,-1]
# 
# 
# # SSI_coef_preop <- as.data.frame(read.csv(file = "data/preop/SSI_coef_20220906.csv",check.names=FALSE))
# # UTI_coef_preop <- as.data.frame(read.csv(file = "data/preop/UTI_coef_2022_0906.csv",check.names=FALSE))
# # SYSEP_coef_preop <- as.data.frame(read.csv(file = "data/preop/SYSEP_coef_20220906_est.csv",check.names=FALSE))
# # PNEU_coef_preop <- as.data.frame(read.csv(file = "data/preop/PNEU_coef_20220906.csv",check.names=FALSE))
# 
# #load("/Users/yaxuzhuang/Library/CloudStorage/OneDrive-TheUniversityofColoradoDenver/Katie Project/2022_0711/combid_pre/com_score_preop.rdata")
# 
# 
# #Epic_data_sub <- Epic_data[,c("arb_person_id","PrimarySurgeonSpecialty","SurgeryDate_asDate","SurgeryID","PrimarySurgeonName")]
# 
# # colnames(diag_pred)[284:288]
# n <- which(colnames(diag_pred) == "80")
# 
# diag_pred_clean <- diag_pred[,c(which(colnames(diag_pred)=="SurgeryID"), 
#                                 n:ncol(diag_pred))]
# 
# # table(diag_pred_clean$`OS CT BODY COMPARE-NO READ`)
# 
# # %>%rename(`80` = phecode_80) # problem with 80
# 
# analytical_preop <- merge(surg_pred_preop,diag_pred_clean,by="SurgeryID")
# 
# # colnames(lab_pred)[287:ncol(lab_pred)]
# 
# n <- grep("^ANAEROBIC CULTURE,", colnames(lab_pred))[1]
# 
# # TODO: ncol(lab_pred)-4 to remove ] "arb_encounter_id"   "EpicCptCode"                                                
# # [9] "OmopCptCode"     "OmopHcpcsCode"
# 
# lab_pred_clean <- lab_pred[,c(which(colnames(lab_pred)=="SurgeryID"),
#                               n:(ncol(lab_pred)-4))]
# 
# analytical_preopdf2 <- merge(analytical_preop,lab_pred_clean,by="SurgeryID")
# 
# 
# #all.equal(analytical_preopdf2$arb_person_id.x,analytical_preopdf2$arb_person_id)
# 
# # colnames(proc_pred_preop)[294: 299]
# 
# proc_pred_preop_clean <- proc_pred_preop[,
#                       c(which(colnames(proc_pred_preop)=="SurgeryID"),
#                 which(colnames(proc_pred_preop)=="OS CT BODY COMPARE-NO READ"))]
# 
# # glimpse(proc_pred_preop_clean)
# # colnames(proc_pred_preop)[290:300]
# 
# # TODO: FIX BUG
# # table(analytical_preopdf2$`OS CT BODY COMPARE-NO READ`)
# # glimpse(analytical_preopdf2)
# # table(proc_pred_preop_clean$`OS CT BODY COMPARE-NO READ`)
# # glimpse(proc_pred_preop_clean)
# 
# #analytical_preopdf3 <- merge(analytical_preopdf2,medi_pred,by="arb_person_id")
# analytical_preopdf4 <- merge(analytical_preopdf2,proc_pred_preop_clean,by="SurgeryID")
# 
# # colnames(gender_preop)[3:8]
# 
# gender_preop_clean <- gender_preop[,c(which(colnames(gender_preop)=="SurgeryID"),
#                                       which(colnames(gender_preop)=="Sex_Male"))]
# #analytical_preopdf5 <- merge(analytical_preopdf4,surg_pred_preop,by="arb_person_id")
# analytical_preopdf6 <- merge(analytical_preopdf4,gender_preop_clean,by="SurgeryID")
# 
# com_score_preop_clean <- com_score_preop[,c(which(colnames(com_score_preop)=="SurgeryID"),ncol(com_score_preop))]
# 
# analytical_preopdf7 <- merge(analytical_preopdf6,com_score_preop_clean,by="SurgeryID")
# 
# analytical_preopv2 <- analytical_preopdf7
# 
# analytical_preopv2$comb_560 <- ifelse(rowSums(
#   analytical_preopv2[,which(substring(colnames(analytical_preopv2),1,3)==560)])>=1,1,0)
# 
# # colnames(analytical_preopv2)[295:297]
# 
# # colnames(analytical_preopv2)[which(colnames(analytical_preopv2) 
# # == "80"): ncol(analytical_preopv2)]
# 
# cultures_to_combine <- grep("CULTURE", 
#      colnames(analytical_preopv2)[which(colnames(analytical_preopv2) 
#                              == "80"): ncol(analytical_preopv2)],
#      value = TRUE)
# 
# message(cultures_to_combine)
# 
# # analytical_preopv2$blood_cult_comb <- ifelse(rowSums(analytical_preopv2[,c(296:306)])>=1,1,0)
# analytical_preopv2$blood_cult_comb <- ifelse(rowSums(analytical_preopv2[,
#                                                cultures_to_combine])>=1,1,0)
# analytical_preopv2$WoundClass <- ifelse(
#   is.na(analytical_preopv2$WoundClass), NA,
#   paste0(analytical_preopv2$WoundClass, " ")
# )
# 
# 
# # typeof(analytical_preopv2)
# # glimpse(analytical_preopv2)
# # table(analytical_preopv2$WoundClass, useNA = "ifany")
# # table(analytical_preopv2$SurgeryInpatientOrOutpatient, useNA = "ifany")
# 
# # table(analytical_preopv2$SurgeryInpatientOrOutpatient_Inpatient , useNA = "ifany")
# # table(analytical_preopv2$SurgeryInpatientOrOutpatient_Outpatient , useNA = "ifany")
# 
# saveRDS(analytical_preopv2, paste0(fp_processed,
#                                        "analytical_preopv2.rds"))
# # TODO: load to save time 
# # analytical_preopv2 <- readRDS(paste0(fp_processed, "analytical_preopv2.rds"))
# 
# # analytical_preopv2$WoundClass
# 
# #analytical_preopv2$blood_cult_comb <- ifelse(analytical_preopv2$`ANAEROBIC CULTURE, (EG:TISSUE, ABSCESS, WOUND, SINUS, ETC.)`+
# #                                                analytical_preopv2$`BLOOD CULTURE`+
# #                                                analytical_preopv2$`CULTURE ANAEROBIC`>=1,1,0)
# 
# #####################################################################################
# ## Preop rate
# ## TODO: fit model SSI
# 
# fitSSI <- predict(glmSSI, analytical_preopv2, type="response")
# 
# analytical_preopv2 <- dummy_cols(analytical_preopv2, select_columns = "windex",remove_selected_columns = FALSE)
# analytical_preopv2 <- dummy_cols(analytical_preopv2, select_columns = "WoundClass",remove_selected_columns = FALSE)
# analytical_preopv2 <- dummy_cols(analytical_preopv2, select_columns = "ASA_class_epic_trans",remove_selected_columns = FALSE)
# 
# SSI_coef_preop <- tibble(Estimate = glmSSI$coefficients)
# #SSI_coef_preop
# analytical_preopv2$pred_prob_SSI_preop <- 1/(1+exp(-(SSI_coef_preop$Estimate[1]+
#                                                        analytical_preopv2$`WoundClass_Clean Contaminated `*SSI_coef_preop$Estimate[2]+
#                                                        analytical_preopv2$`WoundClass_Contaminated `*SSI_coef_preop$Estimate[3]+
#                                                        analytical_preopv2$`WoundClass_Dirty or Infected `*SSI_coef_preop$Estimate[4]+
#                                                        analytical_preopv2$`PrimarySurgeonSpecialty_Orthopedic Surgery`*SSI_coef_preop$Estimate[5]+
#                                                        analytical_preopv2$SurgeryInpatientOrOutpatient_Outpatient*SSI_coef_preop$Estimate[6]+ 
#                                                        analytical_preopv2$`OS CT BODY COMPARE-NO READ`*SSI_coef_preop$Estimate[7]+
#                                                        analytical_preopv2$`80`*SSI_coef_preop$Estimate[8]+
#                                                        analytical_preopv2$comb_560*SSI_coef_preop$Estimate[9]+
#                                                        analytical_preopv2$blood_cult_comb*SSI_coef_preop$Estimate[10]
# ))) # no estimate from 7 - 10?
# 
# #all.equal(as.numeric(analytical_preopv2$pred_prob_SSI_preop),as.numeric(fitSSI))
# 
# ################################################
# # View(glmUTI)
# ## TODO: UTI preop
# test_uti <- analytical_preopv2
# #table(test_uti$Sex_Male)
# test_uti$Sex <- ifelse(test_uti$Sex_Male==1,"Male","Female")
# 
# fitUTI <- predict(glmUTI, test_uti, type="response")
# 
# test_diff <- as.numeric(analytical_preopv2$pred_prob_UTI_preop) - as.numeric(fitUTI)
# 
# 
# analytical_preopv2$pred_prob_UTI_preop <- 1/(1+exp(-(UTI_coef_preop$Estimate[1]+
#                                                        analytical_preopv2$Sex_Male*UTI_coef_preop$Estimate[2]+
#                                                        analytical_preopv2$PrimarySurgeonSpecialty_Gynecology*UTI_coef_preop$Estimate[3]+
#                                                        analytical_preopv2$PrimarySurgeonSpecialty_Urology*UTI_coef_preop$Estimate[4]+
#                                                        analytical_preopv2$SurgeryInpatientOrOutpatient_Inpatient*UTI_coef_preop$Estimate[5]+
#                                                        analytical_preopv2$`OS CT BODY COMPARE-NO READ`*UTI_coef_preop$Estimate[6]+
#                                                        analytical_preopv2$`591`*UTI_coef_preop$Estimate[7])))
# 
# ################################################
# ## TODO: SYSEP preop
# # View(glmSYSEP)
# 
# fitSYSEP <- predict(glmSYSEP, analytical_preopv2, type="response")
# 
# analytical_preopv2$pred_prob_SYSEP_preop <- 1/(1+exp(-(SYSEP_coef_preop$Estimate[1]+
#                                                          analytical_preopv2$`WoundClass_Clean Contaminated `*SYSEP_coef_preop$Estimate[2]+
#                                                          analytical_preopv2$`WoundClass_Contaminated `*SYSEP_coef_preop$Estimate[3]+
#                                                          analytical_preopv2$`WoundClass_Dirty or Infected `*SYSEP_coef_preop$Estimate[4]+
#                                                          analytical_preopv2$ASA_class_epic_trans_3*SYSEP_coef_preop$Estimate[5]+
#                                                          analytical_preopv2$ASA_class_epic_trans_4or5*SYSEP_coef_preop$Estimate[6]+
#                                                          analytical_preopv2$`PrimarySurgeonSpecialty_Orthopedic Surgery`*SYSEP_coef_preop$Estimate[7]+
#                                                          analytical_preopv2$SurgeryInpatientOrOutpatient_Outpatient*SYSEP_coef_preop$Estimate[8]+
#                                                          analytical_preopv2$`567`*SYSEP_coef_preop$Estimate[9]+
#                                                          analytical_preopv2$`994`*SYSEP_coef_preop$Estimate[10]
# )))
# 
# # all.equal(as.numeric(analytical_preopv2$pred_prob_SYSEP_preop),as.numeric(fitSYSEP))
# test_diff <- as.numeric(analytical_preopv2$pred_prob_SYSEP_preop) - as.numeric(fitSYSEP)
# 
# # table(analytical_preopv2$SurgeryInpatientOrOutpatient, useNA = "ifany")
# 
# # table(analytical_preopv2$`PrimarySurgeonSpecialty_Orthopedic Surgery`, 
# #       useNA = "ifany")
# 
# 
# # table(analytical_preopv2$ASA_class_epic_trans , useNA = "ifany")
# 
# # table(analytical_preopv2$`OS CT BODY COMPARE-NO READ`, useNA = "ifany")
# # table(analytical_preopv2$`80`, useNA = "ifany")
# # table(analytical_preopv2$comb_560, useNA = "ifany")
# 
# ################################################
# ## TODO: PNEU preop
# # View(glmPNEU)
# fitPNEU <- predict(glmPNEU, analytical_preopv2, type="response")
# 
# 
# analytical_preopv2$pred_prob_PNEU_preop <- 1/(1+exp(-(PNEU_coef_preop$Estimate[1]+
#                                                         analytical_preopv2$`windex_1-2`*PNEU_coef_preop$Estimate[2]+
#                                                         analytical_preopv2$`windex_>=3`*PNEU_coef_preop$Estimate[3]+
#                                                         analytical_preopv2$ASA_class_epic_trans_3*PNEU_coef_preop$Estimate[4]+
#                                                         analytical_preopv2$ASA_class_epic_trans_4or5*PNEU_coef_preop$Estimate[5]+
#                                                         analytical_preopv2$SurgeryInpatientOrOutpatient_Outpatient*PNEU_coef_preop$Estimate[6]+
#                                                         analytical_preopv2$`150`*PNEU_coef_preop$Estimate[7]+
#                                                         analytical_preopv2$`480`*PNEU_coef_preop$Estimate[8]+
#                                                         analytical_preopv2$`501`*PNEU_coef_preop$Estimate[9]
# )))
# #all.equal(as.numeric(analytical_preopv2$pred_prob_PNEU_preop),as.numeric(fitPNEU))
# test_diff <- as.numeric(analytical_preopv2$pred_prob_PNEU_preop) - as.numeric(fitPNEU)
# 
# ################################################################################
# ####### TODO: SAVE preop pred_prob
# # saveRDS(ICD10_uni_surg_id_spec, paste0(fp_processed,
# #                                        "ICD10_uni_surg_id_spec.rds"))
# 
# # View(analytical_preopv2)
# # sum(is.na(analytical_preopv2$pred_prob_SSI_preop ))
# # sum(is.na(analytical_preopv2$pred_prob_UTI_preop))
# # sum(is.na(analytical_preopv2$pred_prob_SYSEP_preop ))
# # sum(is.na(analytical_preopv2$pred_prob_PNEU_preop))
# 
# write.csv(analytical_preopv2,
#           file = paste0(fp_processed, "preop_expected_rate.csv"),
#                       row.names=FALSE)
# 
# ################################################################################
# 
# 
# ###############################################################################
# ######### TODO: # Postop rate
# 
# diag_pred <-final_data_store_diag_postop
# lab_pred <- final_data_store_lab_postop
# medi_pred <- final_data_store_medi_postop
# 
# Epic_data_sub <- Epic_data[,c("SurgeryID","arb_person_id","PrimarySurgeonSpecialty")]
# 
# analytical_postop <- merge(Epic_data_sub,diag_pred)
# analytical_postopv <- merge(analytical_postop,medi_pred)
# # Warning message:
# # In merge.data.frame(analytical_postopv, lab_pred, by = c("SurgeryID",  :
# #  column names ‘EpicCptCode.x’, ‘OmopCptCode.x’, ‘OmopHcpcsCode.x’, 
# #  ‘EpicCptCode.y’, ‘OmopCptCode.y’, ‘OmopHcpcsCode.y’ are duplicated in the result
#  
# analytical_postopv2 <- merge(analytical_postopv,lab_pred,
#                          by = c("SurgeryID","arb_person_id", "arb_encounter_id"))
# 
# # analytical_postopv2$`80` <- analytical_postopv2$phecode_80
# # colnames(analytical_postopv2)
# analytical_postopv2$comb_599 <- ifelse(rowSums(analytical_postopv2[,which(substring(colnames(analytical_postopv2),1,3)==599)])>=1,1,0)
# analytical_postopv2$comb_592 <- ifelse(rowSums(analytical_postopv2[,which(substring(colnames(analytical_postopv2),1,3)==592)])>=1,1,0)
# analytical_postopv2$comb_540 <- ifelse(rowSums(analytical_postopv2[,which(substring(colnames(analytical_postopv2),1,3)==540)])>=1,1,0)
# analytical_postopv2$comb_994 <- ifelse(rowSums(analytical_postopv2[,which(substring(colnames(analytical_postopv2),1,3)==994)])>=1,1,0)
# analytical_postopv2$comb_480 <- ifelse(rowSums(analytical_postopv2[,which(substring(colnames(analytical_postopv2),1,3)==480)])>=1,1,0)
# 
# # ANAEROBIC CULTURE
# # TODO: combine more cultures as 2024 
# 
# blood_cultures_to_combine <- grep("BLOOD", 
#                                   colnames(analytical_postopv2)[which(colnames(analytical_postopv2) 
#                                                                       == "80"): ncol(analytical_postopv2)],
#                                   value = TRUE)
# # can't subset columns not exist
# blood_cultures_to_combine <- c(blood_cultures_to_combine, 
#                                # "CULTURE ANAEROBIC", "ANAEROBIC CULTURE",
#                                "ANAEROBIC CULTURE, (EG:TISSUE, ABSCESS, WOUND, SINUS, ETC.)")
# blood_cultures_to_combine <- blood_cultures_to_combine[!blood_cultures_to_combine
#                                    %in% c("ARTERIAL BLOOD GAS", "VENOUS BLOOD GAS")]
# 
# message(blood_cultures_to_combine)
# 
# # cultures_to_combine
# 
# analytical_postopv2$blood_cult_comb <- ifelse(rowSums(analytical_postopv2[,
#                                       blood_cultures_to_combine])>=1,1,0)
# 
# 
# urine_cultures_to_combine <- grep("URINE CULTURE", 
#                             colnames(analytical_postopv2)[which(colnames(analytical_postopv2) 
#                                                                == "80"): ncol(analytical_postopv2)],
#                             value = TRUE)
# 
# urine_cultures_to_combine <- c(urine_cultures_to_combine,
#                                "UA DIPSTICK W/ REFLEX TO MICROSCOPIC EXAM IF IND (NO CULTURE REFLEX, EXCEPT EPH)",
#                                "UA COMPLETE URINALYSIS (NO CULTURE REFLEX)")
# message(urine_cultures_to_combine)
# 
# analytical_postopv2$Urine_cult_comb <- ifelse(rowSums(analytical_postopv2[,
#                                       urine_cultures_to_combine])>=1,1,0)
# 
# # TODO: double check if CDIFFICILE TOXIN PCR exists 
# # TODO: CDIFFICILE
# # TODO: string search LabPanelName analytical_postopv2$c_diff_comb*UTI_coef_postop$x[7]+
# # There is one patient since 20240101, has labpanelname  CDIFFICILE TOXIN PCR
# # analytical_postopv2$c_diff_comb <- ifelse(analytical_postopv2$`CDIFFICILE TOXIN PCR`>=1,1,0)
# 
# # table(df_CDIFF_search$LabPanelName) %>% names()
# # [1] "C DIFF GDH/TOXIN"                                 "C DIFFICILE BY PCR"                              
# # [3] "C DIFFICILE TOXIN/GDH"                            "CDIFFICILE TOXIN PCR"                            
# # [5] "LAB USE ONLY - C.DIFFICILE TOXIN REFLEX"          "LAB USE ONLY - C.DIFFICILE TOXIN REFLEX (GI PCR)"
# 
# CDIFFs_to_combine <- grep("DIFF", colnames(analytical_postopv2)[which(colnames(analytical_postopv2) 
#                                           == "80"): ncol(analytical_postopv2)],
#                                   value = TRUE) %>%
#                  grep("CBC", ., invert = TRUE, value = TRUE)
# 
# message(CDIFFs_to_combine)
# 
# # sum(analytical_postopv2[, CDIFFs_to_combine])
# 
# analytical_postopv2$c_diff_comb <- ifelse(rowSums(analytical_postopv2[,
#                                           CDIFFs_to_combine, drop = FALSE])>=1,1,0)
# 
# # sum(analytical_postopv2$c_diff_comb)
# 
# analytical_postopv2$CBC_auto_diff_comb <- ifelse(analytical_postopv2$`CBC NO AUTO DIFF`+ 
#                                                    analytical_postopv2$`CBC WITH AUTO DIFF`+analytical_postopv2$`CBC WITH MANUAL DIFF IF AUTO FAILS (PERFORMABLE)`>=1,1,0)
# 
# analytical_postopv2$respiratory_comb <- ifelse(analytical_postopv2$`RESPIRATORY CULTURE`>=1,1,0)
# 
# # Prob
# analytical_postopv2$BLOOD_GASES_comb <- ifelse(analytical_postopv2$`ARTERIAL BLOOD GAS`+
#                                                  analytical_postopv2$`VENOUS BLOOD GAS`>=1,1,0)
# 
# saveRDS(analytical_postopv2, paste0(fp_processed,
#                                    "analytical_postopv2.rds"))
# # SSI_coef_postop
# ########################################################################
# ## TODO: pred_prob_SSI
# analytical_postopv2$pred_prob_SSI <- 1/(1+exp(-(SSI_coef_postop$x[1]+
#                                                   analytical_postopv2$`80`*SSI_coef_postop$x[2]+
#                                                   analytical_postopv2$`1011`*SSI_coef_postop$x[3]+
#                                                   analytical_postopv2$AntiBiotics_YN*SSI_coef_postop$x[4]+
#                                                   analytical_postopv2$blood_cult_comb*SSI_coef_postop$x[5])))
# 
# ########################################################################
# ## TODO: pred_prob_UTI
# analytical_postopv2$pred_prob_UTI <- 1/(1+exp(-(UTI_coef_postop$x[1]+
#                                                   analytical_postopv2$`590`*UTI_coef_postop$x[2]+
#                                                   analytical_postopv2$`591`*UTI_coef_postop$x[3]+
#                                                   analytical_postopv2$`592`*UTI_coef_postop$x[4]+
#                                                   analytical_postopv2$AntiBiotics_YN*UTI_coef_postop$x[5]+
#                                                   analytical_postopv2$Urine_cult_comb*UTI_coef_postop$x[6]+
#                                                   # Problem 
#                                                   analytical_postopv2$c_diff_comb*UTI_coef_postop$x[7]+
#                                                   analytical_postopv2$comb_599*UTI_coef_postop$x[8])))
# 
# ########################################################################
# ## TODO: pred_prob_SYSEP
# 
# analytical_postopv2$pred_prob_SYSEP <- 1/(1+exp(-(SYSEP_coef_postop$x[1]+
#                                                     analytical_postopv2$AntiBiotics_YN*SYSEP_coef_postop$x[2]+
#                                                     analytical_postopv2$`MAGNESIUM SERUM`*SYSEP_coef_postop$x[3]+
#                                                     analytical_postopv2$comb_540*SYSEP_coef_postop$x[5]+
#                                                     analytical_postopv2$comb_994*SYSEP_coef_postop$x[6]+
#                                                     analytical_postopv2$CBC_auto_diff_comb*SYSEP_coef_postop$x[7]+
#                                                     analytical_postopv2$blood_cult_comb*SYSEP_coef_postop$x[8])))
# 
# ########################################################################
# ## TODO: pred_prob_PNEU
# analytical_postopv2$`1013` <- 0
# analytical_postopv2$pred_prob_PNEU <- 1/(1+exp(-(PNEU_coef_postop$x[1]+
#                                                    analytical_postopv2$`480`*PNEU_coef_postop$x[2]+
#                                                    analytical_postopv2$`501`*PNEU_coef_postop$x[3]+
#                                                    # Problem 
#                                                    analytical_postopv2$`1013`*PNEU_coef_postop$x[4]+
#                                                    analytical_postopv2$AntiBiotics_YN*PNEU_coef_postop$x[5]+
#                                                    analytical_postopv2$`MAGNESIUM SERUM`*PNEU_coef_postop$x[6]+
#                                                    analytical_postopv2$`VANCOMYCIN TROUGH`*PNEU_coef_postop$x[7]+
#                                                    analytical_postopv2$respiratory_comb*PNEU_coef_postop$x[8]+
#                                                    analytical_postopv2$BLOOD_GASES_comb*PNEU_coef_postop$x[9])))
# 
# #colnames(analytical_postopv2)
# #showdf <- analytical_postopv2[,c("SurgeryID","pred_prob_SSI","PrimarySurgeonSpecialty")]
# Predicted_prob <- analytical_postopv2[,c("SurgeryID","arb_person_id","pred_prob_SSI","pred_prob_UTI","pred_prob_SYSEP","pred_prob_PNEU","PrimarySurgeonSpecialty.x")]
# 
# #View(Predicted_prob)
# colnames(Predicted_prob) <- c("SurgeryID","arb_person_id","pred_prob_SSI_postop","pred_prob_UTI_postop","pred_prob_SYSEP_postop","pred_prob_PNEU_postop","PrimarySurgeonSpecialty_raw")
# #rowSums(analytical_postopv2[,which(substring(colnames(analytical_postopv2),1,3)==599)])
# #table(analytical_postopv2$comb_599 )
# #table(rowSums(analytical_postopv2[,which(substring(colnames(analytical_postopv2),1,3)==599)]))
# 
# # glimpse(Predicted_prob)
# ################################################################################
# ###### postop_observed_rate
# write.csv(Predicted_prob,
#           file = paste0(fp_processed, "postop_observed_rate.csv"),
#           row.names=FALSE)


