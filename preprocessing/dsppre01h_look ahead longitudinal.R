rm(list = ls());gc();source(".Rprofile")
# ,"hc","triceps","iliac","abdominal","medial","wc" --> not there in la
anthro_vars <- c("sbp","dbp","height","weight","bmi")
# "glucose2h","insulinf","ast","apo_a","apo_b","uric_acid","alt","totalc" --> not there in la
lab_vars <- c("hba1c","glucosef","tgl","hdlc","ldlc","vldlc",
              "serumcreatinine","urinealbumin","urinecreatinine","uacr","egfr")


# no fasting insulin; fasting glucose in mg/gl 
la_dat_all <- readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/interim/la_dat_all.RDS")) %>% 
  arrange(study_id,visit) %>% 
  dplyr::mutate(ratio_th=tgl/hdlc
  ) %>% 
  dplyr::filter(!is.na(bsage))

# N = 4,901
la_longitudinal = la_dat_all %>% 
  arrange(study_id,visit) %>% 
  mutate(
    available_labs = rowSums(!is.na(.[,lab_vars])),
    available_anthro = rowSums(!is.na(.[,anthro_vars]))) %>% 
  dplyr::select(study_id,visit,bsage,dmagediag,dmduration,available_labs,available_anthro,
                one_of(anthro_vars),one_of(lab_vars),female,race_eth,alcohol,smoking,dmfamilyhistory)



saveRDS(la_longitudinal,paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01h_la.RDS"))

la_longitudinal <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01h_la.RDS"))
write_csv(la_longitudinal,paste0(path_prediabetes_subphenotypes_folder,"/working/longitudinal/look ahead.csv"))
