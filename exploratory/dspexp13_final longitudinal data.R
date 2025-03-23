rm(list=ls());gc();source(".Rprofile")

# Longitudinal dataset: new + no T2D; 8 cohorts; historical + follow-up

accord_long = readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dspexp01_accord new dm.RDS")) %>% 
  mutate(study = "accord") %>% 
  mutate(bmi = weight/((height/100)^2),
         ratio_th=tgl/hdlc) %>% 
  rename(age = bsage,
         wave = visit)

la_long <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dspexp07_look ahead new dm.RDS")) %>% 
  mutate(study = "look ahead") %>% 
  mutate(ratio_th=tgl/hdlc) %>% 
  rename(age = bsage,
         wave = visit) 
 
aric_long <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dspexp02_aric new and no dm.RDS")) %>% 
  mutate(study_id = as.integer(sub("C", "", study_id)),
         study = "aric") %>% 
  mutate(race_eth = case_when(
    race_rev == "White" ~ "White",
    race_rev == "AA" ~ "Black",
    TRUE ~ NA_character_
  ),
  smoking = case_when(
    smk_cur %in% c("1", "T", "Y") ~ "Present",
    smk_evr %in% c("1", "T", "Y") ~ "Past",
    (smk_evr %in% c("0", "N")) & (smk_cur %in% c("0", "N")) ~ "Never",
    TRUE ~ "Missing"
  )) %>%
  rename(wave = visit) %>% 
  select(-race,-race_rev) 

cardia_long <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dspexp03_cardia new and no dm.RDS")) %>% 
  mutate(study = "cardia") %>% 
  mutate(race_eth = case_when(
    race_rev == "White" ~ "White",
    race_rev == "AA" ~ "Black",
    TRUE ~ NA_character_
  )) %>% 
  rename(wave = year) %>% 
  select(-race_rev)

jhs_long <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dspexp06_jhs new and no dm.RDS")) %>% 
  mutate(study = "jhs") %>% 
  mutate(ratio_th=tgl/hdlc) %>% 
  rename(wave = visit)

mesa_long <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dspexp08_mesa new and no dm.RDS")) %>% 
  mutate(study = "mesa") %>% 
  mutate(insulinf = insulinf2/6,
         glucosef = glucosef2/0.0555) %>% 
  rename(race_eth = race,
         wave = exam)

dppos_long <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dspexp04_dpp new and no dm.RDS")) %>% 
  mutate(study = "dpp/dppos") %>% 
  mutate(ratio_th=tgl/hdlc)

hrs_long <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dspexp05_hrs new and no dm.RDS")) %>% 
  mutate(study = "hrs") 



#-------------------------------------------------------------------------------------------------------------
dataset_list <- list(
  accord_long,
  aric_long,
  cardia_long,
  dppos_long,
  hrs_long,
  jhs_long,
  la_long,
  mesa_long
)


prepare_data <- function(df) {
  df %>% 
    select(study_id,study,wave,female,sex,race_eth,age,dmagediag,dmduration,dmfamilyhistory,
           alcohol,smoking,insulinf,glucosef,glucose2h,
           height,weight,bmi,wc,sbp,dbp,hba1c,totalc,ldlc,hdlc,vldlc,tgl,wc,apo_a,apo_b,
           serumcreatinine,urinealbumin,urinecreatinine,uacr,uric_acid,egfr,alt)
    mutate(
      race_eth = case_when(is.na(race_eth) | race_eth == "Unknown" ~ "Unknown",
                               race_eth == "black" ~ "Black",
                               race_eth == "white" ~ "White",
                               race_eth == "other" ~ "Other",
                               race_eth == "Hispanic" | race_eth == "hispanic" ~ "Hispanic",
                               TRUE ~ race_eth),
      sex = case_when(female == 1 ~ "female",
                      female == 0 ~ "male",
                      TRUE ~ "unknown")
    ) 
}
# N = 494,117
pooled_df <- bind_rows(lapply(dataset_list, prepare_data)) %>% 
  mutate(new_id = paste(study, study_id, sep = "_"), 
         # assign "Other race" to 3 ppl with multiple race_eth
         race_eth = case_when(new_id == "mesa_1076" | new_id == "mesa_1359" | new_id == "mesa_2614" ~ "Other",
                                  TRUE ~ race_eth))

saveRDS(pooled_df, paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dspexp13_final longitudinal data.RDS"))

