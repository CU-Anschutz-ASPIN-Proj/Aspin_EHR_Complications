# Define variables to preserve
vars_to_keep <- c("pipeline_dir", "source_if_exists")

# Remove all objects except those in vars_to_keep
rm(list = setdiff(ls(), vars_to_keep))

# Rest of your script...


library(dummy)
library(lubridate)
library(tidyr)
library(dplyr)
library(tidyverse)
library(readxl)
library(readr)
library(magrittr)

source("./global_vars.R")

# Replace table 6 with deduplicated table 6

# TODO: generate the rds file below
surg <- read_rds(file = fp_tab6_Surgeon_nullcpt)
print(dim(surg))
# sum(is.na(surg$AsaRatingName))
# colnames(surg)

# which(surg$PrimarySurgeonName == "CRIBARI, CHRIS")

# unique(surg$PrimarySurgeonName) 

adms_eli <- read_excel("../data/Surgeon_2023_with_Dep_MZ_addition_11_01_24.xlsx", 
                  sheet = 6)%>%
               dplyr::rename("Surgical.Specialty" = "ServiceLine")
# which(adms_eli$PrimarySurgeonName == "CRIBARI, CHRIS")
# View(adms_eli)

# adms_eli <- read.csv(file="data/Surgeon_spec_list_ARD.csv")[,c(4,5)]

surg$SurgeryDate_asDate <- as.Date(surg$SurgeryDate)

surg_no_anes <- surg%>%
  left_join(adms_eli,by="PrimarySurgeonName")

#surg_no_nas <- surg_no_anes[surg_no_anes$Surgical.Specialty!="n/a",]
surg_no_nas <- surg_no_anes[!is.na(surg_no_anes$Surgical.Specialty),]

surg_no_nas <- surg_no_nas[!is.na(surg_no_nas$SurgeryDate_asDate),]
surg_df2 <- surg_no_nas
# surg_df2 <- surg_no_nas[surg_no_nas$PrimarySurgeonSpecialty != c("","*Unspecified"),]

surg_df2$SurgeryDate <- NULL
surg_df2$SurgeryID <- 1:nrow(surg_df2)

ICD10_uni_surg_id_spec <- surg_df2

# TODO: change save path/Done
saveRDS(ICD10_uni_surg_id_spec, paste0(fp_processed,
                                       "ICD10_uni_surg_id_spec.rds"))
rm(surg, surg_df2, adms_eli, surg_no_anes, surg_no_nas)

