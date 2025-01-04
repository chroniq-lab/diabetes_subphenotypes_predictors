rm(list = ls());gc();source(".Rprofile")

coxph_coef <- read_csv("analysis/dspan03_cox ph with multiple imputation.csv") %>% 
  dplyr::filter(model == "m1") %>% 
  select(-model)

library(ggplot2)
library(forestplot)
library(ggpubr)

coxph_long <- coxph_coef %>%
  gather(key = "outcome", value = "HR", Overall, MARD, MOD, SIDD, SIRD) %>%
  mutate(outcome = factor(outcome, levels = c("Overall", "MARD", "MOD", "SIDD", "SIRD"))) %>% 
  separate(HR, into = c("estimate", "ci_range"), sep = " \\(") %>%
  separate(ci_range, into = c("ci_low", "ci_high"), sep = ", ") %>%
  mutate(ci_high = sub("\\)", "", ci_high),
         estimate = as.numeric(estimate),
         ci_low = as.numeric(ci_low),
         ci_high = as.numeric(ci_high))


coxph_forest_plot <- ggplot(coxph_long, aes(x = estimate, y = term)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), 
                 position = position_dodge(width = 0.5), height = 0.2) +
  facet_grid(. ~ outcome, scales = "free_x", switch = "x") +
  theme_bw() +
  labs(x = "Hazard Ratio (95% CI)", y = "Covariates") +
  scale_x_continuous(limits = c(0, 4), breaks = 0:4) +  # Set x-axis limits and breaks
  theme(
    strip.text.x = element_text(angle = 0),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) 

ggsave(coxph_forest_plot,filename=paste0(path_diabetes_subphenotypes_predictors_folder,"/figures/forest plot of cox ph hazard ratio by covariates.jpg"),width=12,height =5.5)


#--------------------------------------------------------------------------------------------------------------------

tdcm_coef <- read_csv("analysis/dspan03_tdcm with multiple imputation.csv") %>% 
  dplyr::filter(model == "m1") %>% 
  select(-model)

library(ggplot2)
library(forestplot)
library(ggpubr)

tdcm_long <- tdcm_coef %>%
  gather(key = "outcome", value = "HR", Overall, MARD, MOD, SIDD, SIRD) %>%
  mutate(outcome = factor(outcome, levels = c("Overall", "MARD", "MOD", "SIDD", "SIRD"))) %>% 
  separate(HR, into = c("estimate", "ci_range"), sep = " \\(") %>%
  separate(ci_range, into = c("ci_low", "ci_high"), sep = ", ") %>%
  mutate(ci_high = sub("\\)", "", ci_high),
         estimate = as.numeric(estimate),
         ci_low = as.numeric(ci_low),
         ci_high = as.numeric(ci_high))


tdcm_forest_plot <- ggplot(tdcm_long, aes(x = estimate, y = term)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), 
                 position = position_dodge(width = 0.5), height = 0.2) +
  facet_grid(. ~ outcome, scales = "free_x", switch = "x") +
  theme_bw() +
  labs(x = "Hazard Ratio (95% CI)", y = "Covariates") +
  scale_x_continuous(limits = c(0, 2), breaks = 0:2) +  # Set x-axis limits and breaks
  theme(
    strip.text.x = element_text(angle = 0),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) 

ggsave(tdcm_forest_plot,filename=paste0(path_diabetes_subphenotypes_predictors_folder,"/figures/forest plot of tdcm hazard ratio by covariates.jpg"),width=12,height =5.5)
