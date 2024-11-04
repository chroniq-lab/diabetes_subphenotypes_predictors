rm(list=ls());gc();source(".Rprofile")

#--------------------------------------------------------------------------------------------------------------
### la ###
#--------------------------------------------------------------------------------------------------------------
# N = 4901
la_total = readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/cleaned/look_ahead.RDS")) 
# N = 877
la_newdm = readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/cleaned/la_newdm.RDS")) 


# N = 565
la_female <- la_newdm %>%
  dplyr::filter(female == 1)  

# N = 299
la_racemin <- la_newdm %>%
  dplyr::filter(!is.na(race_eth)) %>% 
  dplyr::filter(race_eth != "NH White") 
