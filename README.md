# Aspin_EHR_Complications

## Aspin Infectious and Non-Infectious Complications Predictive Probability Generation Pipeline
Aspin_EHR_Complications/pipeline

## Data files Summary Table: name, location, description, timestamp  
Data files: \\data\dept\SOM\ACCORDS\PiFolders\PI_Colborn\UCHealthSOR\ASPIN_Data

## TODO
1. - [x] Guannan Upload Aspin 2025 infectious Complication pipeline
2. - [x] Guannan check RBC UNIT (postop), "ANTI-OBESITY - ANOREXIC AGENTS","ANTIHYPERTENSIVES, ACE INHIBITORS","FOLIC ACID PREPARATIONS","NOSE PREPARATIONS, VASOCONSTRICTORS(OTC)" (preop)
3. - [x] Delete Lab positive part: Guannan check impact of Lab positive or not "lab_positive <- Lab
lab_positive$LabResult[lab_positive$LabResult==""] <- "unknown"
lab_positive$LabResult[is.na(lab_positive$LabResult)] <- "unknown"

lab_positive$ind_positive[substring(lab_positive$LabResult,1,3)=="No " | substring(lab_positive$LabResult,1,3)=="NO " | grepl("Negative", lab_positive$LabResult, ignore.case = T)==TRUE] <- 0
lab_positive$ind_positive[!(substring(lab_positive$LabResult,1,3)=="No " | substring(lab_positive$LabResult,1,3)=="NO " | grepl("Negative", lab_positive$LabResult, ignore.case = T)==TRUE)] <- 1
lab_positive2 <- lab_positive[lab_positive$ind_positive==1,]


lab <- lab_positive2[,-ncol(lab_positive2)]"    

4. - [x] Yi to change code to more pipeline format as "Aspin 2025 infectious Complication pipeline"    
5. - [ ] Double check, Misha May Paper, use the same "ICD10_uni_surg_id_spec.rds" (20250203_2024) helper datafile, run 2024 thoracic 13 complications.    
6. - [ ] Guannan run this pipeline with 2024 HDC data, to compare infectious part with Aspin reports results (Guannan Previous), compare with the non infectious part (Yi previous results). Compare predictive probabilities.    
7. - [ ] Table 1, patients characteristics, Age Mean(SD); Sex Female, male; Race; Ethnicity  
8. - [ ] Table 2,  incidence rate and O/E
9. - [ ] Two Figures, one combined incidence rate, one O/E, reuse code
10. - [ ] Optimize lists to filter LabPanelName, ProcedureName, PharmaceuticalClass, as for now, add unique() and try to add as many as possible.

## Naming Convention 

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
                "name" = "PrimarySurgeonName") # length(unique(dat5$PrimarySurgeonName))

dat5 %<>% filter(PrimarySurgeonName %in% uch_list$name)"
