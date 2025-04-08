# Aspin_EHR_Complications

## Aspin Infectious and Non-Infectious Complications Predictive Probability Generation Pipeline

## Data files Summary Table: name, location, description, timestamp  

## TODO
1. Guannan Upload Aspin 2025 infectious Complication pipeline
2. Guannan check RBC UNIT (postop), "ANTI-OBESITY - ANOREXIC AGENTS","ANTIHYPERTENSIVES, ACE INHIBITORS","FOLIC ACID PREPARATIONS","NOSE PREPARATIONS, VASOCONSTRICTORS(OTC)" (preop)
3. Guannan check impact of Lab positive or not "lab_positive <- Lab
lab_positive$LabResult[lab_positive$LabResult==""] <- "unknown"
lab_positive$LabResult[is.na(lab_positive$LabResult)] <- "unknown"

lab_positive$ind_positive[substring(lab_positive$LabResult,1,3)=="No " | substring(lab_positive$LabResult,1,3)=="NO " | grepl("Negative", lab_positive$LabResult, ignore.case = T)==TRUE] <- 0
lab_positive$ind_positive[!(substring(lab_positive$LabResult,1,3)=="No " | substring(lab_positive$LabResult,1,3)=="NO " | grepl("Negative", lab_positive$LabResult, ignore.case = T)==TRUE)] <- 1
lab_positive2 <- lab_positive[lab_positive$ind_positive==1,]


lab <- lab_positive2[,-ncol(lab_positive2)]"    

3. Yi to change code to more pipeline format as "Aspin 2025 infectious Complication pipeline"  
4. Double check, Misha May Paper, use the same "ICD10_uni_surg_id_spec.rds" (20250203_2024) helper datafile, run 2024 thoracic 13 complications, 

## Naming Convetion 

Different Folder name based on the date of data, but same names for processed data files. 

## Final Target 
Modularity (bt step, )
Important Steps all processed data files. 
Achieve shared code and processed data files through those processed data files. Such as share "ICD10_uni_surg_id_spec.rds" or share predictive probability 


## Notes
For Misha May paper, only for UCH Thoracic Surgeons, filtered by UCH thoracic "# TODO: filter by UCH 
uch_list <- read_excel("../data/2024_10_28_UCH_surgeons_and_pivot_tables_YS.xlsx", 
                       sheet = 6) %>%
  dplyr::rename("specialty" = "ServiceLine",
                "name" = "PrimarySurgeonName")

# length(unique(dat5$PrimarySurgeonName))

dat5 %<>% filter(PrimarySurgeonName %in% uch_list$name)"
