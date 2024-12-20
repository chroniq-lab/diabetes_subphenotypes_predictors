rm(list = ls());gc();source(".Rprofile")

library(survival)
library(survminer)
library(ggsurvfit)
library(broom)
library(tidyr)
library(lme4)

ipcw_dfs <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/processed/ipcw_dfs.RDS"))

# Cox PH model
for (i in 1:ipcw_dfs$m) {
  df <- complete(mi_dfs, action = i) 
  
  cluster_df <- df %>% 
    mutate(mard = case_when(cluster == "MARD" ~ 1,
                            TRUE ~ 0),
           mod = case_when(cluster == "MOD" ~ 1,
                           TRUE ~ 0),
           sidd = case_when(cluster == "SIDD" ~ 1,
                            TRUE ~ 0),
           sird = case_when(cluster == "SIRD" ~ 1,
                            TRUE ~ 0))
  
  # Cox PH - baseline data
  cross_df <- cluster_df %>% 
    group_by(study_id,study) %>% 
    dplyr::filter(age == min(age)) %>% 
    ungroup()
  
  overall_cp[[i]] <- coxph(Surv(time_to_event, event) ~ study + race + female + age + bmi + hba1c + homa2b + homa2ir 
                           + ldlc + sbp + egfr_ckdepi_2021, 
                           data = cross_df, weights = ipcw_weights)
  
  mard_cp[[i]] <- coxph(Surv(time_to_event, mard) ~ study + race + female + age + bmi + hba1c + homa2b + homa2ir 
                        + ldlc + sbp + egfr_ckdepi_2021, 
                        data = cross_df, weights = ipcw_weights)
  
  mod_cp[[i]] <- coxph(Surv(time_to_event, mod) ~ study + race + female + age + bmi + hba1c + homa2b + homa2ir 
                       + ldlc + sbp + egfr_ckdepi_2021, 
                       data = cross_df, weights = ipcw_weights)
  
  sidd_cp[[i]] <- coxph(Surv(time_to_event, sidd) ~ study + race + female + age + bmi + hba1c + homa2b + homa2ir 
                        + ldlc + sbp + egfr_ckdepi_2021, 
                        data = cross_df, weights = ipcw_weights)
  
  sird_cp[[i]] <- coxph(Surv(time_to_event, sird) ~ study + race + female + age + bmi + hba1c + homa2b + homa2ir 
                        + ldlc + sbp + egfr_ckdepi_2021, 
                        data = cross_df, weights = ipcw_weights)
  
  
}


# Pooling coefficients ------------
source("functions/clean_mi_contrasts.R")

overall_cp_out = clean_mi_contrasts(overall_cp,link="coxph")











#--------------------------------------------------------------------------------------------------------------------
# TDCM - longitudinal data

for (i in 1:ipcw_dfs$m) {
  df <- complete(mi_dfs, action = i) 
  
  cluster_df <- df %>% 
    mutate(mard = case_when(cluster == "MARD" ~ 1,
                            TRUE ~ 0),
           mod = case_when(cluster == "MOD" ~ 1,
                           TRUE ~ 0),
           sidd = case_when(cluster == "SIDD" ~ 1,
                            TRUE ~ 0),
           sird = case_when(cluster == "SIRD" ~ 1,
                            TRUE ~ 0))
  
  tdcm_df <- cluster_df %>%
    arrange(study, study_id, age) %>%
    group_by(study, study_id) %>%
    mutate(
      tstart = case_when(row_number() == 1 ~ age, 
                         TRUE ~ dplyr::lag(age, n = 1)), 
      tstop = age
    ) %>%
    ungroup() %>% 
    # dplyr::filter(tstart < tstop)
    dplyr::filter((tstart < tstop) & (tstop <= censored_age))

  
  overall_tdcm[[i]] <- coxph(Surv(tstart, tstop, event) ~ study + female + race + min_age + bmi + hba1c + homa2b 
                             + homa2ir + ldlc + sbp + egfr_ckdepi_2021, 
                             data = tdcm_df, weights = ipcw_weights)
  
  mard_tdcm[[i]] <- coxph(Surv(tstart, tstop, mard) ~ study + female + race + min_age + bmi + hba1c + homa2b 
                          + homa2ir + ldlc + sbp + egfr_ckdepi_2021, 
                          data = tdcm_df, weights = ipcw_weights)
  
  mod_tdcm[[i]] <- coxph(Surv(tstart, tstop, mod) ~ study + female + race + min_age + bmi + hba1c + homa2b 
                         + homa2ir + ldlc + sbp + egfr_ckdepi_2021, 
                         data = tdcm_df, weights = ipcw_weights)
  
  sidd_tdcm[[i]] <- coxph(Surv(tstart, tstop, sidd) ~ study + female + race + min_age + bmi + hba1c + homa2b 
                          + homa2ir + ldlc + sbp + egfr_ckdepi_2021, 
                          data = tdcm_df, weights = ipcw_weights)
  
  sird_tdcm[[i]] <- coxph(Surv(tstart, tstop, sird) ~ study + female + race + min_age + bmi + hba1c + homa2b 
                          + homa2ir + ldlc + sbp + egfr_ckdepi_2021, 
                          data = tdcm_df, weights = ipcw_weights)
  
  
}



#--------------------------------------------------------------------------------------------------------------------
# mixed effect model

for (i in 1:ipcw_dfs$m) {
  df <- complete(mi_dfs, action = i) 
  
  cluster_df <- df %>% 
    mutate(mard = case_when(cluster == "MARD" ~ 1,
                            TRUE ~ 0),
           mod = case_when(cluster == "MOD" ~ 1,
                           TRUE ~ 0),
           sidd = case_when(cluster == "SIDD" ~ 1,
                            TRUE ~ 0),
           sird = case_when(cluster == "SIRD" ~ 1,
                            TRUE ~ 0))
  
  
  overall_mix[[i]] <- glmer(event ~ time_to_event + (1|study) + female + race + min_age + bmi + hba1c + homa2b 
                             + homa2ir + ldlc + sbp + egfr_ckdepi_2021, 
                             data = tdcm_df, weights = ipcw_weights, family = binomial(link = "logit"))
  
  mard_mix[[i]] <- glmer(mard ~ time_to_event + (1|study) + female + race + min_age + bmi + hba1c + homa2b 
                         + homa2ir + ldlc + sbp + egfr_ckdepi_2021, 
                         data = tdcm_df, weights = ipcw_weights, family = binomial(link = "logit"))
  
  mod_mix[[i]] <- glmer(mod ~ time_to_event + (1|study) + female + race + min_age + bmi + hba1c + homa2b 
                        + homa2ir + ldlc + sbp + egfr_ckdepi_2021, 
                        data = tdcm_df, weights = ipcw_weights, family = binomial(link = "logit"))
  
  sidd_mix[[i]] <- glmer(sidd ~ time_to_event + (1|study) + female + race + min_age + bmi + hba1c + homa2b 
                         + homa2ir + ldlc + sbp + egfr_ckdepi_2021, 
                         data = tdcm_df, weights = ipcw_weights, family = binomial(link = "logit"))
  
  sird_mix[[i]] <- glmer(sird ~ time_to_event + (1|study) + female + race + min_age + bmi + hba1c + homa2b 
                         + homa2ir + ldlc + sbp + egfr_ckdepi_2021, 
                         data = tdcm_df, weights = ipcw_weights, family = binomial(link = "logit"))
  
  
}



overall_mix_out = clean_mi_contrasts(overall_mix,link="glmer log")




















