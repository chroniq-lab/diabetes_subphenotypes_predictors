rm(list=ls());gc();source(".Rprofile")

#--------------------------------------------------------------------------------------------------------------
### la ###
#--------------------------------------------------------------------------------------------------------------
# N = 4901
la_total = readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/cleaned/look_ahead.RDS")) 
# N = 877
la_newdm = readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/cleaned/la_newdm.RDS")) 


# N = 2871
la_female <- la_total %>%
  dplyr::filter(female == 1)  

# N = 1654
la_racemin <- la_total %>%
  dplyr::filter(!is.na(race_eth)) %>% 
  dplyr::filter(race_eth != "NH White") 
