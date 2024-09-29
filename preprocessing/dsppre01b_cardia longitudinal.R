rm(list = ls());gc();source(".Rprofile")
# "hc","triceps","iliac","abdominal","medial" --> not there in cardia
anthro_vars <- c("sbp","dbp","height","wc","bmi")
#  --> not there in aric
# ,"ast","alt","uric_acid" --> not there in cardia
lab_vars <- c("hba1c","insulinf","glucosef","glucose2h","tgl","hdlc","ldlc",
              "serumcreatinine","urinecreatinine","egfr","apo_a","apo_b","vldlc")

cardia_newdm = readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/cleaned/cardia_newdm.RDS")) 
cardia_baselinedm = readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/interim/cardia_baseline_dm.RDS")) 


cardia_dat_all <- readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/interim/cardia_dat_all.RDS")) %>%
  dplyr::filter(!study_id %in% cardia_baselinedm$study_id) %>% 
  #dplyr::filter(year!=0)%>% 
  dplyr::mutate(ratio_th=tgl/hdlc,
                glucosef2=glucosef*0.0555,
                insulinf2=insulinf*6,
                female = case_when(
                  female == 1 ~ 0,  
                  female == 2 ~ 1,  
                  TRUE ~ NA_integer_
                ),
                race_rev = case_when(
                  race == 4 ~ "AA",
                  race == 5 ~ "White",
                  TRUE ~ NA_character_  
                ),
                race = case_when(
                  race == 4 ~ "NH Black",
                  race == 5 ~ "NH White",
                  TRUE ~ NA_character_), 
                med_col_use = case_when(
                  med_chol_now == 2 ~ 1,
                  TRUE ~ 0), 
                med_bp_use = case_when (
                  (med_hbp_ever == 2 | med_hbp_now ==2) ~1,
                  TRUE ~ 0)
  ) %>% 
  dplyr::filter(!is.na(age))

cardia_longitudinal = cardia_dat_all %>% 
  arrange(study_id,year) %>% 
  dplyr::select(-dmagediag) %>% 
  # Bringing the updated dmagediag from aric_events
  left_join(cardia_newdm %>% 
              dplyr::select(study_id,dmagediag),
            by=c("study_id")) %>% 
  mutate(
    available_labs = rowSums(!is.na(.[,lab_vars])),
    available_anthro = rowSums(!is.na(.[,anthro_vars]))) %>% 
  dplyr::select(study_id,year,age,dmagediag,available_labs,available_anthro,one_of(anthro_vars),one_of(lab_vars))


saveRDS(cardia_longitudinal,paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01b_cardia.RDS"))

cardia_longitudinal <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01b_cardia.RDS"))
write_csv(cardia_longitudinal,paste0(path_prediabetes_subphenotypes_folder,"/working/longitudinal/cardia.csv"))

