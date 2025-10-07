rm(list = ls());gc();source(".Rprofile")
# "hc","triceps","iliac","abdominal","medial" --> not there in cardia
anthro_vars <- c("sbp","dbp","height","wc","bmi")
#  --> not there in aric
# ,"ast","alt","uric_acid" --> not there in cardia
lab_vars <- c("hba1c","insulinf","glucosef","glucose2h","tgl","hdlc","ldlc",
              "serumcreatinine","urinecreatinine","egfr","apo_a","apo_b","vldlc")
med_vars <- c("med_bp_use","med_chol_use","med_dep_use")
lifsy_vars <- c("smoking")

# N = 623
cardia_newdm = readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/cleaned/cardia_newdm.RDS")) 
cardia_baselinedm = readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/interim/cardia_baseline_dm.RDS")) 


cardia_dat_all <- readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/interim/cardia_dat_all.RDS")) %>%
  arrange(study_id,year)%>% 
  group_by(study_id)%>% 
  mutate(dmagediag_ever = min(dmagediag,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(dmagediag_ever = case_when(dmagediag_ever == Inf ~ NA_real_,
                                    TRUE ~ dmagediag_ever)) %>% 

  dplyr::filter(!study_id %in% cardia_baselinedm$study_id) %>% 
  distinct(study_id,year,bmi,.keep_all =TRUE) %>% 
  dplyr::filter(!is.na(age)) %>% 

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
                med_chol_use = case_when(
                  med_chol_now == 1 ~ 1,
                  med_chol_now == 2 ~ 0,
                  # med_chol_now == 8 ~ "Unknown",
                  TRUE ~ 0), 
                med_bp_use = case_when (
                  med_hbp_now == 1 | med_hbp_ever == 1 ~ 1,
                  med_hbp_now == 2 & med_hbp_ever == 2 ~ 0,
                  # med_hbp_now == 8 | med_hbp_ever == 8 ~ "Unknown",
                  TRUE ~ 0),
                med_dep_use = case_when (
                  med_dep_now == 1 ~ 1,
                  med_dep_now == 2 ~ 0,
                  # med_dep_now == 8 ~ "Unknown",
                  TRUE ~ 0),
                smoking = case_when(
                  cigr_st == 0 | (is.na(cigr_st) & smk_st == 3) ~ "Never",
                  cigr_st == 1 ~ "Former",
                  cigr_st == 2 | (is.na(cigr_st) & smk_st == 1) ~ "Current",
                  TRUE ~ "Never"
                )
  ) 


# N = 4,162
cardia_nodm <- cardia_dat_all %>% 
  dplyr::filter(!study_id %in% cardia_newdm$study_id) %>% 
  dplyr::filter(is.na(dmagediag_ever))


cardia_longitudinal = cardia_dat_all %>% 
  # only keep newDM + noDM
  dplyr::filter(study_id %in% cardia_newdm$study_id | study_id %in% cardia_nodm$study_id) %>% 
  dplyr::select(-dmagediag,-dmagediag_ever) %>% 
  arrange(study_id,year) %>% 
  # Bringing the updated dmagediag from aric_events
  left_join(cardia_newdm %>% 
              dplyr::select(study_id,dmagediag),
            by=c("study_id")) %>% 
  mutate(
    available_labs = rowSums(!is.na(.[,lab_vars])),
    available_anthro = rowSums(!is.na(.[,anthro_vars]))) %>% 
  dplyr::select(study_id,year,age,female,race_rev,race,dmagediag,available_labs,available_anthro,
                one_of(anthro_vars),one_of(lab_vars),one_of(med_vars),one_of(lifsy_vars))


saveRDS(cardia_longitudinal,paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01b_cardia.RDS"))

cardia_longitudinal <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01b_cardia.RDS"))
write_csv(cardia_longitudinal,paste0(path_prediabetes_subphenotypes_folder,"/working/longitudinal/cardia.csv"))

