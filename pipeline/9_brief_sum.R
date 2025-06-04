# Define variables to preserve
vars_to_keep <- c("pipeline_dir", "source_if_exists")

# Remove all objects except those in vars_to_keep
rm(list = setdiff(ls(), vars_to_keep))

library(tidyverse)
library(openxlsx)
library(magrittr)
library(lubridate)
library(kableExtra)
source("./global_vars.R")

datpost <- read_csv(paste0(fp_processed, "postop_observed_rate.csv"))
# pull surgery date, specialty and other data, CuMedCptCode
datpre <- read_csv(paste0(fp_processed, "preop_expected_rate.csv"))

########## merge to get data
dat5 <- merge(datpre, datpost, 
              by = c("SurgeryID", "arb_person_id"), all = T)
# dim(dat5)
# colnames(dat5)[grepl("date",colnames(dat5),  ignore.case = TRUE)]

# dat5$SurgeryDate_asDate

complication_cols <- list(
  "SSI" = c("pred_prob_SSI_postop", "pred_prob_SSI_preop"),
  
  "Sepsis" = c("pred_prob_SYSEP_postop", "pred_prob_SYSEP_preop"),
  
  "Pneumonia" = c("pred_prob_PNEU_postop", "pred_prob_PNEU_preop"),
  
  "UTI" = c("pred_prob_UTI_postop", "pred_prob_UTI_preop"),
  "cardiac"= c("pred_prob_cardiac_postop", "pred_prob_cardiac_preop"),
  "morb"= c("pred_prob_morb_postop", "pred_prob_morb_preop"),
  "nothome"= c("pred_prob_nothome_postop", "pred_prob_nothome_preop"),
  "pulmonary"= c("pred_prob_pulmonary_postop", "pred_prob_pulmonary_preop"),
  "renal"= c("pred_prob_renal_postop", "pred_prob_renal_preop"),
  "upradmin"= c("pred_prob_upradmin_postop", "pred_prob_upradmin_preop"),
  "VTE"= c("pred_prob_VTE_postop", "pred_prob_VTE_preop"),
  "bleed" = c("pred_prob_bleed_postop", "pred_prob_bleed_preop"))

pred_prob_cols_postop <- c(
  "pred_prob_SSI_postop", "pred_prob_UTI_postop", "pred_prob_SYSEP_postop", "pred_prob_PNEU_postop",
  "pred_prob_cardiac_postop", "pred_prob_morb_postop", "pred_prob_nothome_postop", "pred_prob_pulmonary_postop",
  "pred_prob_renal_postop", "pred_prob_upradmin_postop", "pred_prob_VTE_postop", "pred_prob_bleed_postop"
)

# Generate the corresponding _preop column names
pred_prob_cols_preop <- gsub("_postop$", "_preop", pred_prob_cols_postop)

# Combine the original _postop list with the new _preop list
pred_prob_cols <- c(pred_prob_cols_postop, pred_prob_cols_preop)

# dim(dat5)

########## 

dat5$DOS <- as.Date(dat5$SurgeryDate_asDate)
dat5$year <- year(dat5$DOS)

dat5$quar <- quarter(dat5$DOS)

dat5$quar <- factor(dat5$quar, levels = c(1, 2, 3, 4))

# colnames(dat5)


# Calculate mean predicted probability for each column (pre‐ vs post‐op)
prevalence_df <- tibble(column = pred_prob_cols) %>%
  mutate(
    prevalence = map_dbl(column, ~ mean(dat5[[.x]], na.rm = TRUE)),
    timing = if_else(str_detect(column, "_preop$"), "preop", "postop"),
    complication = str_remove(str_remove(column, "^pred_prob_"), "_preop$|_postop$")
  ) %>%
  select(complication, timing, prevalence) %>%
  pivot_wider(names_from = timing, values_from = prevalence)

# Display as a table
prevalence_df %>%
  kable(
    col.names = c("Complication", "Post‐op Prevalence", "Pre‐op Prevalence"),
    digits = 3
  )


