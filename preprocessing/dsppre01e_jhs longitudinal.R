rm(list = ls());gc();source(".Rprofile")
#  --> not there in jhs
anthro_vars <- c("sbp","dbp","height","wc","bmi","hc","triceps","iliac","abdominal","medial")
#  --> not there in jhs
# ,"ast","alt","uric_acid" --> not there in cardia
lab_vars <- c("hba1c","insulinf","glucosef","glucose2h","tgl","hdlc","ldlc",
              "serumcreatinine","urinecreatinine","egfr","apo_a","apo_b","vldlc")

jhs_newdm = readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/processed/final_dataset_temp.RDS")) %>% 
  dplyr::filter(study == "jhs") %>% 
  mutate(dmagediag_cluster = round(dmagediag,2),
         original_study_id = as.numeric(original_study_id))



jhs_analysis <- readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/interim/jhspre01_jhs_analysis.RDS")) %>% 
  dplyr::filter(aric == 0) %>%
  arrange(study_id,visit)  %>% 
  group_by(study_id) %>% 
  # Self-reported diabetes carried forward as 1 if 1
  mutate(diabetes = zoo::na.locf(diabetes,na.rm=FALSE),
         dmagediag = zoo::na.locf(dmagediag,na.rm=FALSE)) %>% 
  mutate(dmagediag = case_when(visit == 1 & diabetes == 1 & (dmagediag == Inf | is.na(dmagediag)) ~ -1,
                               diabetes == 1 ~ min(dmagediag,na.rm=TRUE), # Some cases where dmagediag is missing but diabetes == 1
                               (is.na(diabetes)|diabetes == 0) & (glucosef >=126 | hba1c >= 6.5) ~ age,
                               TRUE ~ NA_real_
  )) %>% 
  mutate(dmagediag_updated = case_when(!is.na(dmagediag) & age < dmagediag  ~ -1, # For diabetes == 1 cases
                                       TRUE ~ dmagediag)) %>% 
  mutate(dmagediag_updated = case_when(!is.na(dmagediag_updated) ~ min(dmagediag_updated,na.rm=TRUE),
                                       TRUE ~ NA_real_)) %>% 
  
  ungroup()   %>% 
  
  mutate(
    dmduration = case_when(is.na(dmagediag_updated) | dmagediag_updated == -1 ~ NA_real_,
                           TRUE ~ age-dmagediag_updated),
    
    race_eth = "NH Black",
    female = case_when(female == "Female" ~ 1,
                       female == "Male" ~ 0,
                       TRUE ~ NA_real_)
    
  ) 

write_csv(jhs_analysis,paste0(path_diabetes_subphenotypes_adults_folder,"/working/qc/dsppre01e_jhs_analysis from diabetes_subphenotypes_predictors.csv"))


