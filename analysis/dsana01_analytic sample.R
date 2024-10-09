rm(list = ls());gc();source(".Rprofile")



aric_longitudinal <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01a_aric.RDS")) %>% 
  dplyr::filter(is.na(dmagediag) | (age < dmagediag))

cardia_longitudinal <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01b_cardia.RDS")) %>% 
  dplyr::filter(is.na(dmagediag) | (age < dmagediag))

jhs_longitudinal <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01e_jhs.RDS")) %>% 
  dplyr::filter(is.na(dmagediag) | (age < dmagediag))

dppos_longitudinal <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01c_dppos.RDS")) %>% 
  dplyr::filter(is.na(dmagediag) | (age < dmagediag))

mesa_longitudinal <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01f_mesa.RDS")) %>% 
  dplyr::filter(is.na(dmagediag) | (age < dmagediag))

