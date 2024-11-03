rm(list = ls());gc();source(".Rprofile")
# ,"hc","triceps","iliac","abdominal","medial" --> not there in aric
anthro_vars <- c("sbp","dbp","height","wc","bmi")
# "vldlc","ast","alt" --> not there in aric
lab_vars <- c("hba1c","insulinf","glucosef","glucose2h","tgl","hdlc","ldlc",
              "serumcreatinine","urinecreatinine","egfr","apo_a","apo_b","uric_acid")

accord_newdm = readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/cleaned/accord_newdm.RDS")) 


accord = readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/cleaned/accord.RDS")) 