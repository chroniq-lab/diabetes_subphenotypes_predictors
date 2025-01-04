

analytic_df <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/processed/dsppre01_analytic df.RDS")) %>% 
  mutate(egfr_ckdepi_2021 = egfr_ckdepi_2021(scr = serumcreatinine,female = female,age = age),
         joint_id = paste(study, study_id, sep = "_")) %>%
  group_by(study,study_id,joint_id) %>% 
  mutate(min_age = min(age),
         t = age - min_age) %>% 
  ungroup()


library(ggplot2)

# Calculate availability of hba1c
availability_data <- analytic_df %>%
  group_by(study, t) %>%
  summarize(available = mean(!is.na(bmi)), .groups = 'drop')

ggplot(availability_data, aes(x = t, y = available, color = study, group = study)) +
  geom_line() +
  labs(x = "Time", y = "Proportion of Available Data (%)", title = "Data Availability of BMI Over Time") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_wrap(~ study, nrow = 3, ncol = 2) +  # Faceting
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size = 10, face = "bold")
  )



df <- analytic_df %>%
  dplyr::filter(study == "aric") %>% 
  mutate(diabetes = if_else(!is.na(dmagediag), 1, 0))

# Calculate the proportion of individuals with diabetes at each time point
diabetes_rate <- df %>%
  group_by(t) %>%
  summarize(rate = mean(diabetes), .groups = 'drop')

ggplot(diabetes_rate, aes(x = t, y = rate)) +
  geom_line() +
  geom_point() +  # Adding points can help emphasize data points on the line
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Time", y = "Percentage with Diabetes", title = "Rate of Diabetes Over Time") +
  theme_minimal()