rm(list = ls());gc();source(".Rprofile")


# restrict to 15y of follow-up time for trajectory analysis
analytic_df = readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/processed/dspan01_analytic sample.RDS")) %>% 
  dplyr::filter(t <= 0 & t >= -15) %>% 
  arrange(joint_id,t) %>% 
  distinct(joint_id,t,.keep_all=TRUE) %>% 
  mutate(across(one_of(c("subtype","race","study")),.fns=~as.factor(.))) %>% 
  mutate(dpp_intervention = case_when(dpp_intervention == 1 ~ "intervention arm",
                                      TRUE ~ "others"))

# Step 1: Prepare long-format dataset
biomarkers_long <- analytic_df %>%
  select(joint_id, t, subtype, hba1c, bmi, homa2b, homa2ir) %>%
  pivot_longer(cols = c(hba1c, bmi, homa2b, homa2ir),
               names_to = "biomarker",
               values_to = "value") %>%
  dplyr::filter(!is.na(value))  # remove NA rows for cleaner lines

# Optional: nicer labels
biomarkers_long <- biomarkers_long %>%
  mutate(
    biomarker = factor(biomarker,
                       levels = c("hba1c", "bmi", "homa2b", "homa2ir"),
                       labels = c("HbA1c (%)", "BMI (kg/m²)", "HOMA2-%B", "HOMA2-IR")
    ),
    subtype = factor(subtype, levels = c("NOT2D", "MOD", "SIRD", "SIDD", "MARD"))
  )


library(purrr)

plots_by_subtype <- biomarkers_long %>%
  split(.$subtype) %>%
  map(~ ggplot(.x, aes(x = t, y = value, group = joint_id)) +
        geom_line(alpha = 0.15, color = "grey40") +
        geom_point(alpha = 0.2, size = 0.6) +
        facet_wrap(~biomarker, scales = "free_y", ncol = 2) +
        theme_bw() +
        labs(x = "Time (years)",y = NULL) +
        scale_x_continuous(limits = c(-15, 0), breaks = seq(-15, 0, by = 5)) +
        theme(
          strip.text = element_text(size = 11, face = "bold"),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 11)
        ))


plots_by_subtype$MOD
plots_by_subtype$SIDD


walk2(plots_by_subtype, names(plots_by_subtype), ~ggsave(
  filename = paste0(path_diabetes_subphenotypes_predictors_folder,"/figures/spaghetti_", .y, ".png"),
  plot = .x, width = 8, height = 6, dpi = 300))





