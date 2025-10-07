rm(list = ls());gc();source(".Rprofile")
# ,"hc","triceps","iliac","abdominal","medial" --> not there in mesa
anthro_vars <- c("sbp","dbp","height","wc","bmi")
# "glucose2h","vldlc","ast","alt","apo_a","apo_b","uric_acid" --> not there in mesa
lab_vars <- c("hba1c","insulinf2","glucosef2","tgl","hdlc","ldlc",
              "serumcreatinine","urinealbumin","urinecreatinine","uacr","egfr")
med_vars <- c("med_chol_use","med_bp_use","med_dep_use")
lifsy_vars <- c("smoking")

# N = 989
mesa_newdm = readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/cleaned/mesa_newdm.RDS")) 
mesa_baselinedm = readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/interim/mesa_baseline_dm.RDS")) 

mesa_dat_all <- readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/interim/mesa_dat_all.RDS")) %>% 
  arrange(study_id,exam)%>% 
  group_by(study_id)%>% 
  mutate(dmagediag_ever = min(dia_med_age_st,na.rm=TRUE)) %>% ## dia_med_age_st, available at exam1, the earliest age when started diabetes treatment. 
  ungroup() %>% 
  mutate(dmagediag_ever = case_when(dmagediag_ever == Inf ~ NA_real_,
                                    TRUE ~ dmagediag_ever)) %>% 

  dplyr::filter(!study_id %in% mesa_baselinedm$study_id) %>% 
  distinct(study_id,exam,bmi,.keep_all =TRUE) %>%
  dplyr::filter(!is.na(age)) %>% 
  
  arrange(study_id,exam) %>% 
  dplyr::mutate(ratio_th=tgl/hdlc,
                glucosef2=glucosef*0.0555,
                insulinf2=insulinr*6,
                female = 1 - female,
                race_rev = case_when(
                  race == 1 ~ "White",
                  race == 3 ~ "AA",
                  race == 2 | 4 ~ "Other",
                  TRUE ~ NA_character_  
                ),
                race = case_when(
                  race == 1 ~ "NH White",
                  race == 3 ~ "NH Black",
                  race == 2 ~ "Other",
                  race == 4 ~ "Hispanic",
                  TRUE ~ NA_character_  
                ),
                smoking = case_when(
                  smk == 3 | smk_cig == 2 | smk_cgr == 2 | smk_pipe == 2 | smk_tob == 2 ~ "Current",
                  smk == 1 | smk == 2 | smk_cig == 1 | smk_cgr == 1 | smk_pipe == 1 | smk_tob == 1 ~ "Former",
                  smk == 0 & smk_cig == 0 & smk_cgr == 0 & smk_pipe == 0 & smk_tob == 0 ~ "Never",
                  TRUE ~ "Never"
                ),
                med_bp_use = case_when(
                  med_bp == 1 ~ 1,
                  med_bp == 0 ~ 0,
                  TRUE ~ 0
                ),
                med_chol_use = case_when(
                  med_lipid == 1 | med_statins == 1 ~ 1,
                  med_bp == 0 & med_statins == 0 ~ 0,
                  TRUE ~ 0
                ),
                med_dep_use = case_when(
                  med_dep == 1 ~ 1,
                  med_dep == 0 ~ 0,
                  TRUE ~ 0
                )) 


# N = 5,105, OBS = 22,139
mesa_nodm <- mesa_dat_all %>% 
  dplyr::filter(!study_id %in% mesa_newdm$study_id) %>% 
  dplyr::filter(is.na(dmagediag_ever))


mesa_longitudinal = mesa_dat_all %>%
  # only keep newDM + noDM
  dplyr::filter(study_id %in% mesa_newdm$study_id | study_id %in% mesa_nodm$study_id) %>% 
  dplyr::select(-dmagediag_ever,-dia_med_age_st) %>% 
  arrange(study_id,exam) %>% 
  # Bringing the updated dmagediag from aric_events
  left_join(mesa_newdm %>% 
              dplyr::select(study_id,dmagediag),
            by=c("study_id")) %>% 
  mutate(
    available_labs = rowSums(!is.na(.[,lab_vars])),
    available_anthro = rowSums(!is.na(.[,anthro_vars]))) %>% 
  dplyr::select(study_id,exam,age,dmagediag,available_labs,available_anthro,female,race,ethnicity,
                one_of(anthro_vars),one_of(lab_vars),one_of(med_vars),one_of(lifsy_vars))



saveRDS(mesa_longitudinal,paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01f_mesa.RDS"))

mesa_longitudinal <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01f_mesa.RDS"))
write_csv(mesa_longitudinal,paste0(path_prediabetes_subphenotypes_folder,"/working/longitudinal/mesa.csv"))
