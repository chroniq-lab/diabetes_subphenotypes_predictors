rm(list = ls());gc();source(".Rprofile")

library(gridExtra)
library(ggplot2)
library(survminer)

tdcm_coef <- read_csv("analysis/dspan03_tdcm pooled results with multiple imputation.csv") %>% 
  select(iv, HR, estimate, lci, uci, model) %>% 
  dplyr::filter(!iv %in% c("raceNH Black","raceNH White","raceOther","female1","min_age"),
                model != "Overall") %>% 
  mutate(term = case_when(
    iv == "sbp" ~ "SBP",
    iv == "ldlc" ~ "LDL-C",
    iv == "homa2b" ~ "HOMA2-%B",
    iv == "homa2ir" ~ "HOMA2-IR",
    iv == "hba1c" ~ "HbA1c",
    iv == "bmi" ~ "BMI",
    iv == "egfr_ckdepi_2021" ~ "EGFR",
    TRUE ~ iv  
  ),
  term = factor(term))


# forest plot

plot_forest <- ggplot(tdcm_coef, aes(y = term, x = estimate, xmin = lci, xmax = uci, color = model)) + 
  geom_pointrange(position = position_dodge(width = 0.7), size = 0.7) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "darkgrey") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_color_manual(values = cluster_colors) +
  scale_x_continuous(limits = c(0, 2.5)) +
  labs(
    x = "HR (95% CI)",
    y = NULL,
    title = "A: Hazard Ratios by Biomarkers",
    color = "Subtype"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 12, hjust = 1.2, vjust = -1.6),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5)
  ) +
  geom_text(
    aes(x = uci + 0.08, label = HR),
    position = position_dodge(width = 0.7),
    vjust = 0.2,
    hjust = -0.05,
    fontface = "bold",
    size = 5
  ) +
  geom_text(
    aes(x = 0.5, label = model),
    position = position_dodge(width = 0.7),
    hjust = 1,
    fontface = "bold",
    size = 4
  ) 


#-------------------------------------------------------------------------------------------------------------------
# incidence plot

ipcw_dfs <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/processed/ipcw_dfs.RDS"))

cox_fit <- list()

for (i in 1:length(ipcw_dfs)) {
  df <- ipcw_dfs[[i]]  
  
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
    ungroup() %>% 
    mutate(across(c(bmi, hba1c, homa2b, homa2ir, ldlc, sbp, egfr_ckdepi_2021), ~replace(., is.infinite(.), NA))) %>% 
    mutate(race = case_when(race == "NH Other" ~ "Other", 
                            TRUE ~ race))
  
  df <- cross_df %>% 
    mutate(cluster = factor(cluster,
                            levels = c("MOD", "SIDD", "MARD", "SIRD"),
                            labels = c("MOD", "SIDD", "MARD", "SIRD")),
           study_aric = case_when(study == "aric" ~ 1,
                                  TRUE ~ 0),
           study_cardia = case_when(study == "cardia" ~ 1,
                                    TRUE ~ 0),
           study_dppos = case_when(study == "dppos" ~ 1,
                                   TRUE ~ 0),
           study_jhs = case_when(study == "jhs" ~ 1,
                                 TRUE ~ 0),
           study_mesa = case_when(study == "mesa" ~ 1,
                                  TRUE ~ 0),
    ) %>% 
    dplyr::filter(time_to_event <= 10)
  
  
  cox_fit[[i]] = coxph(Surv(time_to_event, event) ~ strata(cluster) + study_aric + study_cardia + study_dppos + study_jhs + study_mesa +
                    + race + female + age + bmi + hba1c + homa2b + homa2ir 
                  + ldlc + sbp + egfr_ckdepi_2021, 
                  data = df, weights = ipcw_cluster)

}


plot_incidence = survfit2(cox_fit[[1]]) %>% 
  ggsurvfit(.,type = "risk") +
  xlab("Time to Diabetes (years)") +
  ylab("") +
  ggtitle("B: Cumulative Incidence Curve for 10 Years (95% CI)") +
  add_confidence_interval() +
  theme_bw() +
  
  scale_color_manual(values=cluster_colors) +
  scale_fill_manual(values=cluster_colors) +
  theme(axis.text = element_text(size = 12),
        # legend.text = element_text(size = 12),
        # legend.position = "bottom",
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(0,10),breaks=seq(0,10,by=2)) +
  scale_y_continuous(limits = c(0,1),breaks=seq(0,1,by=0.2))



final_plot <- grid.arrange(plot_forest, plot_incidence, ncol = 2)










