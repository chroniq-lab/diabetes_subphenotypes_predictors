rm(list = ls());gc();source(".Rprofile")

library(mice)
library(purrr)
library(emmeans)
library(contrast)

mi_dfs <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/processed/mi_dfs.RDS"))

ipcw_dfs <- list()

for(i in 1:mi_dfs$m) {
  df <- complete(mi_dfs, action = 1) 
  
  fup15y <- df %>%
    group_by(study, study_id) %>%
    mutate(min_age = min(age),
           race = race_eth) %>%
    # Restrict to observations within 15 years of earliest age
    dplyr::filter(age <= (min_age + 15)) %>%
    mutate(event = case_when(# Individuals who are never diagnosed
      is.na(dmagediag) ~ 0,
      
      # Individuals who are diagnosed within 15 years of earliest wave
      dmagediag <= (min_age + 15) ~ 1,
      
      # Individuals who are diagnosed after 15 years of earliest wave
      TRUE ~ 0
      
    )) %>% 
    # without diagnosed T2D at baseline 
    dplyr::filter(event == 0 | ((age <= dmagediag) & (min_age < dmagediag))) %>% 
    ungroup() %>% 
    mutate(joint_id = paste(study, study_id, sep = "_"))
  
  # T2D: >= 1 wave before diagnosis; no T2D: >= 2 wave from baseline
  wave_df <- fup15y %>% 
    group_by(study, study_id) %>%
    mutate(has_age_after_min = any(age > min_age),
           has_age_before_dmagediag = any(age < dmagediag)) %>% 
    dplyr::filter(
      (event == 0 & has_age_after_min) |
        (event == 1 & has_age_before_dmagediag)
    ) %>% 
    mutate(censored_age = case_when(is.na(dmagediag) ~ max(age),
                                    
                                    !is.na(dmagediag) & dmagediag <= (min_age + 15) ~ dmagediag,
                                    
                                    TRUE ~ max(age)),
           time_to_event = censored_age - age) %>% 
    select(-has_age_after_min, -has_age_before_dmagediag) %>% 
    ungroup()
  
  # cluster available id at the age of T2D detection
  cluster_avaid <- wave_df %>% 
    mutate(age_int = case_when(study == "dppos" ~ as.integer(age),
                               TRUE ~ age),
           dmagediag_int = case_when(study == "dppos" ~ as.integer(dmagediag),
                               TRUE ~ dmagediag)) %>% 
    group_by(study, study_id) %>%
    dplyr::filter(event == 0 | ((age_int == dmagediag_int) & !is.na(cluster))) %>% 
    ungroup() %>% 
    select(-age_int,-dmagediag_int)
  
  cluster_ava <- wave_df %>% 
    dplyr::filter((joint_id %in% cluster_avaid$joint_id))
  
  
  ipcw_dfs[[i]] <- cluster_ava
}

saveRDS(ipcw_dfs, paste0(path_diabetes_subphenotypes_predictors_folder,"/working/processed/ipcw_dfs.RDS"))




### FIX #######

# apply IPCW for people without cluster
dm_df <- wave_df %>% 
  dplyr::filter(event == 1) %>% 
  mutate(clu_ava = case_when(joint_id %in% cluster_avaid$joint_id ~ 1,
                             TRUE ~ 0))

censor_model <- glm(clu_ava ~ study + female + race + min_age, data = dm_df, family = "binomial")



dm_df1 <- dm_df %>%
  mutate(prob_uncensored = predict(censor_model, type = "response")
         # ,
         # ipcw = if_else(clu_ava == 1, 1 / prob_uncensored, 1 / (1 - prob_uncensored))
  )

# Merge the IPCW weights back to the original dataset
ipc_weights <- dm_df %>% 
  select(joint_id, ipcw)  # Select only necessary columns

wave_df <- wave_df %>%
  left_join(ipc_weights, by = "joint_id") %>% 
  mutate(ipcw = if_else(is.na(ipcw), 1, ipcw))

















