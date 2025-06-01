rm(list = ls());gc();source(".Rprofile")

library(mice)
library(purrr)
library(emmeans)
library(contrast)

source("functions/egfr_ckdepi_2021.R")
mi_dfs <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/processed/mi_dfs.RDS"))


analytic_dfs <- list()

for(i in 1:mi_dfs$m) {
  df <- complete(mi_dfs, action = i) %>% 
    mutate(egfr_ckdepi_2021 = egfr_ckdepi_2021(scr = serumcreatinine,female = female,age = age),
           time_to_event = censored_age - age) 


  analytic_df <- df %>% 
    arrange(study,study_id,joint_id,age) %>% 
    group_by(study,study_id,joint_id) %>%
    mutate(event = case_when(
      newdm_event == 1 & (age == censored_age) ~ 1,  # event is 1 for the last wave
      TRUE ~ 0 
    )
  ) %>%
  ungroup() # 1,743 events
  
  
  analytic_dfs[[i]] <- analytic_df
}

saveRDS(analytic_dfs, paste0(path_diabetes_subphenotypes_predictors_folder,"/working/processed/dspan03_predictors analytic dfs.RDS"))



