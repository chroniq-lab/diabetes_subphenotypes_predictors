rm(list = ls());gc();source(".Rprofile")
#  ,"hc","triceps","iliac","abdominal","medial"--> not there in jhs
anthro_vars <- c("sbp","dbp","height","wc","bmi")
# "glucose2h","apo_a","apo_b","vldlc","ast","alt","uric_acid" --> not there in jhs
lab_vars <- c("hba1c","insulinf","glucosef","tgl","hdlc","ldlc",
              "serumcreatinine","urinecreatinine","egfr")
med_vars <- c("med_chol_use","med_bp_use","med_dep_use")
lifsy_vars <- c("smoking")

jhs_newdm = readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/cleaned/jhs_newdm.RDS")) 
jhs_baselinedm = readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/interim/jhs_baseline_dm.RDS")) 

jhs_analysis <- readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/interim/jhspre01_jhs_analysis.RDS")) %>% 
  dplyr::filter(aric == 0) %>% 
  dplyr::filter(!study_id %in% jhs_baselinedm$study_id) %>%
  arrange(study_id,visit) %>% 
  mutate(race_eth = "NH Black",
         female = case_when(female == "Female" ~ 1,
                            female == "Male" ~ 0,
                            TRUE ~ NA_real_)) %>% 
  mutate(
    smoking = case_when(
      smk_evr == 0 & smk_cur == 0 ~ "Never",
      smk_evr == 1 & smk_cur == 0 ~ "Former",
      smk_cur == 1 ~ "Current",
      TRUE ~ "Never"
    ),
    med_chol_use = case_when(
      med_statins_past == "Y" | med_statins == "Y" | med_statins_self == "Y" ~ 1,
      med_statins_past == "N" & med_statins == "N" & med_statins_self == "N" ~ 0,
      TRUE ~ 0
    ),
    med_bp_use = case_when(
      med_bp_past == "Y" | med_bp == "Y" | med_bp_self == "Y" ~ 1,
      med_bp_past == "N" & med_bp == "N" & med_bp_self == "N" ~ 0,
      TRUE ~ 0
    ),
    med_dep_use = case_when(med_dep_past == "Y" ~ 1,
                            med_dep_past == "N" ~ 0,
                            TRUE ~ 0)
  )

# jhs_longitudinal --------------

# Haven't filtered out observations where age >= dmagediag

jhs_longitudinal = jhs_analysis %>% 
  dplyr::select(-dmagediag) %>% 
  # Bringing the updated dmagediag from aric_events
  left_join(jhs_newdm %>% 
              dplyr::select(study_id,dmagediag),
            by=c("study_id")) %>% 
  mutate(
    available_labs = rowSums(!is.na(.[,lab_vars])),
    available_anthro = rowSums(!is.na(.[,anthro_vars]))) %>% 
  dplyr::select(study_id,age,dmagediag,available_labs,available_anthro,female,race_eth,
                one_of(anthro_vars),one_of(lab_vars),one_of(med_vars),one_of(lifsy_vars))



saveRDS(jhs_longitudinal,paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01e_jhs.RDS"))
jhs_longitudinal <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01e_jhs.RDS"))


