rm(list = ls());gc();source(".Rprofile")

library(mice)
library(survival)
library(survAUC)
library(pROC)
library(Metrics)
library(purrr)

source("functions/egfr_ckdepi_2021.R")

mi_dfs <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/processed/dsphyc301_mi_dfs.RDS"))

clean_df <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/processed/dspan01_analytic sample.RDS")) %>% 
  select(joint_id,age,dpp_intervention,smoking,med_chol_use,med_bp_use,med_dep_use) %>% 
  mutate(dpp_intervention = case_when(
    dpp_intervention == 1 ~ 1,
    TRUE ~ 0
  )) # n = 1,722

# choose the 1st imputed dataset
df <- complete(mi_dfs, action = 1) %>% 
  rename(joint_id = original_joint_id) %>%
  mutate(egfr_ckdepi_2021 = egfr_ckdepi_2021(scr = serumcreatinine,female = female,age = age),
         time_to_event = censored_age - age) %>% 
  left_join(clean_df,
            by = c("joint_id","age")) %>% 
  mutate(smoking = case_when(is.na(smoking) ~ "Never",
                             TRUE ~ smoking),
         med_bp_use = case_when(is.na(med_bp_use) ~ 0,
                                TRUE ~ med_bp_use),
         med_chol_use = case_when(is.na(med_chol_use) ~ 0,
                                  TRUE ~ med_chol_use),
         med_dep_use = case_when(is.na(med_dep_use) ~ 0,
                                 TRUE ~ med_dep_use)) %>% 

  arrange(study,study_id,joint_id,age) %>% 
  group_by(study,study_id,joint_id) %>%
  ungroup() 


coxph_df <- df %>%
  dplyr::filter(age == earliest_age) %>% 
  # For baseline analysis, use the original newdm_event as the outcome
  mutate(event = newdm_event) %>%
  mutate(across(c(bmi, hba1c, homa2b, homa2ir, ldlc, sbp, egfr_ckdepi_2021), ~replace(., is.infinite(.), NA))) %>% 
  # error due to 0 ppl in NH Other (sidd == 1), ignore this category
  mutate(race = case_when(race == "NH Other" ~ "Other", 
                          TRUE ~ race),
         race = relevel(factor(race), ref = "NH White"),
         race3 = fct_collapse(race,
                              "NH Black" = "NH Black",
                              "Hispanic" = "Hispanic",
                              "White/Other" = c("NH White","Other")),
         race3 = relevel(race3, ref = "White/Other")) %>% 
  # scaling
  mutate(sbp_scaled = sbp/10,
         ldlc_scaled = ldlc/10,
         homa2b_scaled = homa2b/10,
         egfr_ckdepi_2021_scaled = egfr_ckdepi_2021/10) %>% 
  mutate(subtype = case_when(is.na(cluster) ~ "NOT2D",
                             TRUE ~ cluster)) %>%
  mutate(subtype = factor(subtype, levels=c("NOT2D","MARD","MOD","SIDD","SIRD")),
         # 1 if in DPPOS intervention arm, else 0 (also 0 for all non-DPPOS rows)
         dppos_interv = as.numeric(study == "dppos") * as.numeric(dpp_intervention))

# Cox PH models

overall_coxph <- coxph(Surv(time_to_event, newdm_event) ~ study + female + race + earliest_age + bmi + hba1c + homa2b_scaled 
                            + homa2ir + ldlc_scaled + sbp_scaled + egfr_ckdepi_2021_scaled + dpp_intervention
                            + smoking + med_chol_use + med_bp_use + med_dep_use, 
                            data = coxph_df)


# create the predictiveness curve ----------------------------------------------------------------------
data = coxph_df
model = overall_coxph
time_horizon = 8


# Predicted linear predictor
lp <- predict(model, newdata = data, type = "lp")

# Get baseline survival
# base_surv <- survival::basehaz(model, centered = FALSE)
base_surv <- basehaz(model, centered = TRUE)

S0 <- approx(base_surv$time, exp(-base_surv$hazard), xout = time_horizon, rule = 2)$y

# Predicted survival and risk
surv_prob <- S0^exp(lp)
risk_prob <- 1 - surv_prob

# Observed event within time_horizon
obs_event <- with(data, ifelse(!is.na(time_to_event) & !is.na(event) &
                                 time_to_event <= time_horizon & event == 1, 1, 0))



quantiles <- c(0.10, 0.25, 0.50, 0.75, 0.90)

# corresponding threshold values
risk_thresholds <- quantile(risk_prob, probs = 1 - quantiles, na.rm = TRUE)
risk_thresholds

source("functions/evaluate_coxph_quantile.R")

res <- evaluate_coxph_quantile(model, data, time_horizon)
res


cbind(quantile = res$threshold,
      threshold_value = risk_thresholds,
      res[, c("sensitivity", "specificity", "F1")])


library(ggplot2)

# 1. build data
pred_curve <- data.frame(risk_prob = risk_prob) %>%
  mutate(percentile = percent_rank(risk_prob) * 100) %>%
  group_by(bin = cut(percentile, breaks = seq(0, 100, by = 1))) %>%
  summarise(mean_risk = mean(risk_prob, na.rm = TRUE), .groups = "drop") %>%
  mutate(percentile = as.numeric(sub("\\((.+),.*", "\\1", bin)))

# overall incidence
disease_prev <- mean(obs_event, na.rm = TRUE)
top_zone <- 90    # top 10% region


library(ggplot2)

# prepare data
pred_curve <- data.frame(risk_prob = risk_prob) %>%
  mutate(percentile = percent_rank(risk_prob) * 100) %>%
  group_by(bin = cut(percentile, breaks = seq(0, 100, by = 1))) %>%
  summarise(mean_risk = mean(risk_prob, na.rm = TRUE), .groups = "drop") %>%
  mutate(percentile = as.numeric(sub("\\((.+),.*", "\\1", bin)))

disease_prev <- mean(obs_event, na.rm = TRUE)
top_zone <- 90  # threshold percentile (top 10%)

# plot
predict_curve <- ggplot(pred_curve, aes(x = percentile, y = mean_risk)) +
  # --- Shaded areas (right side only) ---
  geom_rect(aes(xmin = top_zone, xmax = 100, ymin = 0, ymax = disease_prev),
            fill = "gray85", color = NA, inherit.aes = FALSE) +
  geom_rect(aes(xmin = top_zone, xmax = 100, ymin = disease_prev, ymax = 1),
            fill = "gray60", color = NA, inherit.aes = FALSE) +
  
  # --- Main curve & reference lines ---
  geom_line(linewidth = 1, color = "black") +
  geom_vline(xintercept = top_zone, linetype = "dotted", color = "black") +
  geom_hline(yintercept = disease_prev, linetype = "dashed", color = "black") +
  
  # --- Axis setup ---
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  labs(x = "Risk percentile", y = "Predicted overall T2D risk") +
  coord_cartesian(expand = FALSE, clip = "off") +
  
  # --- Theme: solid white background ---
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), 
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.2, "cm"),
    plot.title = element_blank()
  )


ggsave(predict_curve,filename=paste0(path_diabetes_subphenotypes_predictors_folder,"/figures/model evaluation predictiveness curve.png"),width=7,height=6)


