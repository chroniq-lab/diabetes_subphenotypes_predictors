
aric <- fup15y %>% 
  dplyr::filter(study == "cardia") %>% 
  group_by(study, study_id) %>%
  mutate(min_age = min(age)) %>%
  mutate(fuptime = age - min_age,
         bac_ava = case_when(!is.na(bmi) & !is.na(hba1c) ~ 1,
                             TRUE ~ 0),
         clu_ava = case_when(!is.na(cluster) ~ 1,
                             TRUE ~ 0)) %>% 
  mutate(event = case_when(# Individuals who are never diagnosed
    is.na(dmagediag) ~ 0,
    
    # Individuals who are diagnosed within 15 years of earliest wave
    dmagediag <= (min_age + 15) ~ 1,
    
    # Individuals who are diagnosed after 15 years of earliest wave
    TRUE ~ 0
    
  )) %>% 
  ungroup()

aric_filtered <- aric %>%
  dplyr::filter(clu_ava == 1)

aric_filtered <- aric %>%
  dplyr::filter(bac_ava == 1)


# Plotting the histogram
ggplot(aric_filtered, aes(x = fuptime)) + 
  geom_histogram(binwidth = 1,    # Set bin width to 1 or choose an appropriate value
                 fill = "blue",   # Color of the bars
                 color = "black") + # Border color of the bars
  labs(title = "Histogram of fuptime for bac_ava = 1",
       x = "fuptime",
       y = "Frequency") +
  theme_minimal() 


aric_filtered <- aric %>%
  dplyr::filter(event == 1 & clu_ava == 1)

ggplot(aric_filtered, aes(x = fuptime)) + 
  geom_histogram(binwidth = 1,    # Set bin width to 1 or choose an appropriate value
                 fill = "blue",   # Color of the bars
                 color = "black") + # Border color of the bars
  labs(title = "Histogram of fuptime for event = 1",
       x = "fuptime",
       y = "Frequency") +
  theme_minimal() 


ggplot(aric_filtered, aes(x = fuptime, fill = factor(event))) + 
  geom_histogram(binwidth = 1, color = "black") +  # You can adjust the binwidth as needed
  labs(title = "Histogram of fuptime for clu_ava = 1",
       x = "fuptime",
       y = "Frequency",
       fill = "Event Type") +
  scale_fill_manual(values = c("red", "green"), labels = c("No Diagnosis", "Diagnosed within 15 Years")) + 
  theme_minimal()


#------------------------------------------------------------------------------------------
final_dataset_temp <- readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/cleaned/final_dataset_temp.RDS"))

# no missnng values in cluster, bmi, a1c 
clusters = read_csv(paste0(path_diabetes_subphenotypes_adults_folder,"/working/processed/dec_an02_clean_kmeans_5var_mi_knn_cluster.csv")) %>% 
  dplyr::select(-one_of("...1")) %>% 
  left_join(final_dataset_temp %>% 
              dplyr::select(study_id,original_study_id),
            by=c("study_id")) %>% 
  rename(cluster_study_id = study_id)


df <- final_dataset_temp %>% 
  select(study,study_id,age,dmagediag,cluster,bmi,hba1c)

































