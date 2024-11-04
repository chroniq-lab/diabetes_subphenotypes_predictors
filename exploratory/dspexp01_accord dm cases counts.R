rm(list=ls());gc();source(".Rprofile")

#--------------------------------------------------------------------------------------------------------------
### ACCORD ###
#--------------------------------------------------------------------------------------------------------------
# N = 10251
accord_total = readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/cleaned/accord.RDS")) 
# N = 601
accord_newdm = readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/cleaned/accord_newdm.RDS")) 


# N = 235
accord_female <- accord_newdm %>%
  dplyr::filter(female == 1)  

# N = 237
accord_racemin <- accord_newdm %>%
  dplyr::filter(!is.na(race_eth)) %>% 
  dplyr::filter(race_eth != "NH White") 
