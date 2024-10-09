rm(list = ls());gc();source(".Rprofile")

aric <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01a_aric.RDS")) %>% 
  mutate(study = "aric")
cardia <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01b_cardia.RDS")) %>% 
  mutate(study = "cardia", study_id = as.character(study_id))
dppos <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01c_dppos.RDS")) %>% 
  mutate(study = "dppos", study_id = as.character(study_id))
jhs <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01e_jhs.RDS")) %>% 
  mutate(study = "jhs", study_id = as.character(study_id))
mesa <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01f_mesa.RDS")) %>% 
  mutate(study = "mesa", study_id = as.character(study_id))

merged_df <- bind_rows(aric, cardia, dppos, jhs, mesa)

saveRDS(merged_df, paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01_merged df.RDS"))
#-------------------------------------------------------------------------------------------------------------------------------

# DM cases, 5y before diagnosis records, N = 3342
dm_df <- merged_df %>% 
  dplyr::filter(!is.na(dmagediag)) %>% 
  mutate(
    time_diff = dmagediag - age,  
    time_diff5 = abs(time_diff - 5) 
  ) %>%
  group_by(study_id) %>%
  dplyr::filter(time_diff > 0 & time_diff5 <= 1) %>%  
  slice_min(order_by = time_diff5, with_ties = FALSE) %>%  
  ungroup()


# Non-DM cases
ndm_df <- merged_df %>% 
  dplyr::filter(is.na(dmagediag) & !is.na(age)) %>% 
  group_by(study_id) %>%
  mutate(age_range = max(age) - min(age)) %>%  
  dplyr::filter(age_range >= 4) %>% 
  ungroup() %>%
  dplyr::select(-age_range)

# pair with previous year (time period < 3)
ndm_df_joined <- ndm_df %>% 
  rename(current_age = age) %>% 
  mutate(current_age_minus5 = current_age - 5) %>% 
  left_join(ndm_df %>% 
              dplyr::select(study_id,age) %>% 
              rename(previous_age = age), by = "study_id") %>%
  dplyr::filter(age.y < age.x) %>%
  mutate(age_diff = abs(age.y - age_5)) %>%
  dplyr::filter(age_diff < 2) %>% 
  group_by(study_id, age.x) %>%
  slice(1) %>%
  rename(current_age = age.x, previous_age = age.y)

# match DM and non-DM cases based on age, bmi, hba1c















